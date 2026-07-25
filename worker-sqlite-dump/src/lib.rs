//! Logical dump and atomic restore for SQLite-backed Cloudflare Durable Objects.
//!
//! This crate owns the storage-format mechanics only. Authentication, transport, locking,
//! migration policy, and domain validation belong to the Worker using it.

use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use js_sys::Map;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use worker::{
    wasm_bindgen::JsValue, Error, Result, SqlStorage, SqlStorageValue, Storage, Transaction,
};

/// The dump envelope format emitted by this version of the crate.
pub const DUMP_FORMAT: u32 = 1;

const MAX_FINITE_DOUBLE: &str = "1.7976931348623157e308";

/// One application KV entry stored alongside the SQLite database.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct KvEntry {
    pub key: String,
    pub value: Value,
}

impl KvEntry {
    pub fn new(key: impl Into<String>, value: Value) -> Self {
        Self {
            key: key.into(),
            value,
        }
    }
}

#[derive(Serialize)]
struct DumpPayload<'a> {
    dump_format: u32,
    sql: &'a [String],
    kv: &'a [KvEntry],
}

/// A portable, checksummed logical snapshot of one Durable Object's application storage.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Dump {
    pub dump_format: u32,
    pub sql: Vec<String>,
    pub kv: Vec<KvEntry>,
    pub checksum: String,
}

impl Dump {
    /// Builds and checksums a dump from ordered SQL statements and application KV entries.
    pub fn new(sql: Vec<String>, kv: Vec<KvEntry>) -> Result<Self> {
        let mut dump = Self {
            dump_format: DUMP_FORMAT,
            sql,
            kv,
            checksum: String::new(),
        };
        dump.refresh_checksum()?;
        Ok(dump)
    }

    /// Verifies the format version and checksum before a dump is accepted.
    pub fn verify(&self) -> Result<()> {
        if self.dump_format != DUMP_FORMAT {
            return Err(rust_error(format!(
                "Unsupported dump format: {}",
                self.dump_format
            )));
        }
        if payload_checksum(self)? != self.checksum {
            return Err(rust_error("Dump checksum mismatch"));
        }
        Ok(())
    }

    /// Recomputes the checksum after deliberately editing a dump.
    ///
    /// Normal exports are already checksummed. This is primarily useful for tests and explicit
    /// dump transformations.
    pub fn refresh_checksum(&mut self) -> Result<()> {
        self.checksum = payload_checksum(self)?;
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize)]
struct SchemaEntry {
    #[serde(rename = "type")]
    kind: String,
    name: String,
    sql: String,
}

#[derive(Clone, Debug, Deserialize)]
struct TableColumn {
    cid: i64,
    name: String,
    pk: i64,
    hidden: i64,
}

#[derive(Debug, Deserialize)]
struct CountRow {
    count: i64,
}

#[derive(Debug, Deserialize)]
struct SequenceRow {
    name_literal: String,
    seq_literal: String,
}

fn rust_error(message: impl ToString) -> Error {
    Error::RustError(message.to_string())
}

fn payload_checksum(dump: &Dump) -> Result<String> {
    let payload = DumpPayload {
        dump_format: dump.dump_format,
        sql: &dump.sql,
        kv: &dump.kv,
    };
    let encoded = serde_json::to_vec(&payload).map_err(rust_error)?;
    let digest = Sha256::digest(encoded);
    Ok(digest.iter().map(|byte| format!("{byte:02x}")).collect())
}

fn quote_identifier(identifier: &str) -> String {
    format!("\"{}\"", identifier.replace('"', "\"\""))
}

fn terminate_statement(statement: &str) -> String {
    let trimmed = statement.trim();
    if trimmed.ends_with(';') {
        trimmed.to_string()
    } else {
        format!("{trimmed};")
    }
}

fn sql_literal_expression(column_name: &str) -> String {
    let column = quote_identifier(column_name);
    format!(
        "CASE typeof({column})
            WHEN 'null' THEN 'NULL'
            WHEN 'integer' THEN CAST({column} AS TEXT)
            WHEN 'real' THEN CASE
                WHEN {column} > {MAX_FINITE_DOUBLE} THEN '9.0e+999'
                WHEN {column} < -{MAX_FINITE_DOUBLE} THEN '-9.0e+999'
                ELSE printf('%!.17g', {column})
            END
            WHEN 'text' THEN 'CAST(X''' || hex(CAST({column} AS BLOB)) || ''' AS TEXT)'
            WHEN 'blob' THEN 'X''' || hex({column}) || ''''
            ELSE NULL
        END"
    )
}

fn application_schema(sql: &SqlStorage) -> Result<Vec<SchemaEntry>> {
    sql.exec(
        "SELECT type, name, sql
         FROM sqlite_schema
         WHERE sql IS NOT NULL
           AND name NOT LIKE 'sqlite_%'
           AND lower(name) NOT IN ('__cf_kv', '_cf_kv')
           AND name NOT LIKE '__miniflare_%'
           AND type IN ('table', 'index', 'view', 'trigger')
         ORDER BY
           CASE type
             WHEN 'table' THEN 0
             WHEN 'index' THEN 1
             WHEN 'view' THEN 2
             WHEN 'trigger' THEN 3
           END,
           name",
        None,
    )?
    .to_array()
}

fn table_columns(sql: &SqlStorage, table_name: &str) -> Result<Vec<TableColumn>> {
    let mut columns: Vec<TableColumn> = sql
        .exec(
            &format!("PRAGMA table_xinfo({})", quote_identifier(table_name)),
            None,
        )?
        .to_array()?;
    columns.retain(|column| column.hidden == 0);
    columns.sort_by_key(|column| column.cid);
    Ok(columns)
}

fn dump_table_rows(sql: &SqlStorage, table: &SchemaEntry) -> Result<Vec<String>> {
    let columns = table_columns(sql, &table.name)?;
    if columns.is_empty() {
        return Ok(Vec::new());
    }

    let column_list = columns
        .iter()
        .map(|column| quote_identifier(&column.name))
        .collect::<Vec<_>>()
        .join(", ");
    let value_expressions = columns
        .iter()
        .enumerate()
        .map(|(index, column)| format!("{} AS value_{index}", sql_literal_expression(&column.name)))
        .collect::<Vec<_>>()
        .join(", ");

    let mut primary_key = columns
        .iter()
        .filter(|column| column.pk > 0)
        .collect::<Vec<_>>();
    primary_key.sort_by_key(|column| column.pk);
    let order_columns = if primary_key.is_empty() {
        columns.iter().collect::<Vec<_>>()
    } else {
        primary_key
    };
    let order_by = order_columns
        .iter()
        .map(|column| quote_identifier(&column.name))
        .collect::<Vec<_>>()
        .join(", ");
    let query = format!(
        "SELECT {value_expressions}
         FROM {}
         ORDER BY {order_by}",
        quote_identifier(&table.name)
    );

    sql.exec(&query, None)?
        .raw()
        .map(|row| {
            let values = row?
                .into_iter()
                .map(|value| match value {
                    SqlStorageValue::String(value) => Ok(value),
                    other => Err(rust_error(format!(
                        "Could not encode value in table {}: {other:?}",
                        table.name
                    ))),
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(format!(
                "INSERT INTO {} ({column_list}) VALUES ({});",
                quote_identifier(&table.name),
                values.join(", ")
            ))
        })
        .collect()
}

fn dump_sequence(sql: &SqlStorage) -> Result<Vec<String>> {
    let has_sequence: CountRow = sql
        .exec(
            "SELECT count(*) AS count
             FROM sqlite_schema
             WHERE type = 'table' AND name = 'sqlite_sequence'",
            None,
        )?
        .one()?;
    if has_sequence.count == 0 {
        return Ok(Vec::new());
    }

    let rows: Vec<SequenceRow> = sql
        .exec(
            "SELECT
               'CAST(X''' || hex(CAST(name AS BLOB)) || ''' AS TEXT)' AS name_literal,
               CAST(seq AS TEXT) AS seq_literal
             FROM sqlite_sequence
             ORDER BY name",
            None,
        )?
        .to_array()?;
    if rows.is_empty() {
        return Ok(Vec::new());
    }

    let mut statements = vec!["DELETE FROM sqlite_sequence;".to_string()];
    statements.extend(rows.into_iter().map(|row| {
        format!(
            "INSERT INTO sqlite_sequence (name, seq) VALUES ({}, {});",
            row.name_literal, row.seq_literal
        )
    }));
    Ok(statements)
}

fn build_sql_dump(sql: &SqlStorage) -> Result<Vec<String>> {
    let schema = application_schema(sql)?;
    let tables = schema
        .iter()
        .filter(|entry| entry.kind == "table")
        .collect::<Vec<_>>();

    for table in &tables {
        if table
            .sql
            .trim_start()
            .to_ascii_uppercase()
            .starts_with("CREATE VIRTUAL TABLE")
        {
            return Err(rust_error(format!(
                "Virtual table dumps are not supported: {}",
                table.name
            )));
        }
    }

    let mut statements = tables
        .iter()
        .map(|table| terminate_statement(&table.sql))
        .collect::<Vec<_>>();
    for table in tables {
        statements.extend(dump_table_rows(sql, table)?);
    }
    statements.extend(dump_sequence(sql)?);

    for kind in ["index", "view", "trigger"] {
        statements.extend(
            schema
                .iter()
                .filter(|entry| entry.kind == kind)
                .map(|entry| terminate_statement(&entry.sql)),
        );
    }
    Ok(statements)
}

fn map_to_kv(map: Map) -> Result<Vec<KvEntry>> {
    let values: BTreeMap<String, Value> =
        serde_wasm_bindgen::from_value(JsValue::from(map)).map_err(rust_error)?;
    Ok(values
        .into_iter()
        .map(|(key, value)| KvEntry { key, value })
        .collect())
}

fn sql_is_empty(sql: &SqlStorage) -> Result<bool> {
    let row: CountRow = sql
        .exec(
            "SELECT count(*) AS count
             FROM sqlite_schema
             WHERE name NOT LIKE 'sqlite_%'
               AND lower(name) NOT IN ('__cf_kv', '_cf_kv')
               AND name NOT LIKE '__miniflare_%'",
            None,
        )?
        .one()?;
    Ok(row.count == 0)
}

async fn transaction_is_empty(transaction: &Transaction) -> Result<bool> {
    Ok(transaction.list().await?.size() == 0)
}

/// Returns whether the Durable Object has no application SQL objects or KV entries.
pub async fn is_empty(storage: &Storage) -> Result<bool> {
    Ok(sql_is_empty(&storage.sql())? && storage.list().await?.size() == 0)
}

/// Exports application SQL and KV in one consistent storage transaction.
pub async fn export(storage: Storage) -> Result<Dump> {
    let output = Rc::new(RefCell::new(None));
    let transaction_output = output.clone();
    let sql = storage.sql();

    storage
        .transaction(move |transaction| {
            let sql = sql.clone();
            let output = transaction_output.clone();
            async move {
                let statements = build_sql_dump(&sql)?;
                let kv = map_to_kv(transaction.list().await?)?;
                *output.borrow_mut() = Some((statements, kv));
                Ok(())
            }
        })
        .await?;

    let (sql, kv) = output
        .borrow_mut()
        .take()
        .ok_or_else(|| rust_error("Dump transaction produced no output"))?;
    Dump::new(sql, kv)
}

/// Restores a verified dump into an empty Durable Object in one storage transaction.
pub async fn restore(storage: Storage, dump: Dump) -> Result<()> {
    restore_and_validate(storage, dump, |_| Ok(())).await
}

/// Restores a verified dump, then runs synchronous validation before committing.
///
/// Returning an error from `validate` rolls back both the restored SQL and KV. The callback can
/// also run synchronous SQL migrations through the supplied `SqlStorage`.
pub async fn restore_and_validate<F>(storage: Storage, dump: Dump, validate: F) -> Result<()>
where
    F: FnOnce(&SqlStorage) -> Result<()> + 'static,
{
    dump.verify()?;
    let sql = storage.sql();
    let dump = Rc::new(dump);

    storage
        .transaction(move |transaction| {
            let sql = sql.clone();
            let dump = dump.clone();
            async move {
                if !sql_is_empty(&sql)? || !transaction_is_empty(&transaction).await? {
                    return Err(rust_error("Target Durable Object is not empty"));
                }
                for statement in &dump.sql {
                    sql.exec(statement, None)?;
                }
                for entry in &dump.kv {
                    transaction.put(&entry.key, &entry.value).await?;
                }
                validate(&sql)?;
                Ok(())
            }
        })
        .await
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::{Dump, KvEntry};

    #[test]
    fn checksum_detects_changes() {
        let mut dump = Dump::new(
            vec!["CREATE TABLE example (value INTEGER);".to_string()],
            vec![KvEntry::new("version", json!(1))],
        )
        .unwrap();

        dump.verify().unwrap();
        dump.sql.push("INSERT INTO example VALUES (1);".to_string());
        assert!(dump.verify().is_err());
        dump.refresh_checksum().unwrap();
        dump.verify().unwrap();
    }

    #[test]
    fn checksum_is_deterministic() {
        let sql = vec!["CREATE TABLE example (value INTEGER);".to_string()];
        let kv = vec![KvEntry::new("version", json!(1))];

        assert_eq!(
            Dump::new(sql.clone(), kv.clone()).unwrap().checksum,
            Dump::new(sql, kv).unwrap().checksum
        );
    }
}
