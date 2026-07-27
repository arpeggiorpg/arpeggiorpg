//! Logical dump and atomic restore for SQLite-backed Cloudflare Durable Objects.
//!
//! This crate owns the storage-format mechanics only. Authentication, transport, locking,
//! migration policy, and domain validation belong to the Worker using it.

use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use js_sys::Map;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use worker::{
    wasm_bindgen::JsValue, Error, Result, SqlStorage, SqlStorageValue, Storage, Transaction,
};

/// The dump envelope format emitted by this version of the crate.
pub const DUMP_FORMAT: u32 = 2;

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

/// One SQLite value represented without embedding application data in SQL text.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum SqlValue {
    Null,
    Integer(String),
    Real(String),
    /// The exact bytes of a SQLite TEXT value, encoded as hexadecimal.
    Text(String),
    /// The bytes of a SQLite BLOB value, encoded as hexadecimal.
    Blob(String),
}

/// One ordered operation in a logical SQLite dump.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "snake_case", tag = "type")]
pub enum SqlOperation {
    Statement {
        sql: String,
    },
    Insert {
        table: String,
        columns: Vec<String>,
        rows: Vec<Vec<SqlValue>>,
    },
}

/// A portable logical snapshot of one Durable Object's application storage.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Dump {
    pub dump_format: u32,
    pub sql: Vec<SqlOperation>,
    pub kv: Vec<KvEntry>,
}

impl Dump {
    /// Builds a dump from ordered SQL operations and application KV entries.
    pub fn new(sql: Vec<SqlOperation>, kv: Vec<KvEntry>) -> Self {
        Self {
            dump_format: DUMP_FORMAT,
            sql,
            kv,
        }
    }

    /// Rejects dump envelopes whose representation this crate does not understand.
    pub fn validate_format(&self) -> Result<()> {
        if self.dump_format != DUMP_FORMAT {
            return Err(rust_error(format!(
                "Unsupported dump format: {}",
                self.dump_format
            )));
        }
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
    name_hex: String,
    seq: String,
}

fn rust_error(message: impl ToString) -> Error {
    Error::RustError(message.to_string())
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

fn encoded_value_expression(column_name: &str) -> String {
    let column = quote_identifier(column_name);
    format!(
        "CASE typeof({column})
            WHEN 'null' THEN NULL
            WHEN 'integer' THEN CAST({column} AS TEXT)
            WHEN 'real' THEN CASE
                WHEN {column} > {MAX_FINITE_DOUBLE} THEN '9.0e+999'
                WHEN {column} < -{MAX_FINITE_DOUBLE} THEN '-9.0e+999'
                ELSE printf('%!.17g', {column})
            END
            WHEN 'text' THEN hex(CAST({column} AS BLOB))
            WHEN 'blob' THEN hex({column})
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

fn dump_table_rows(sql: &SqlStorage, table: &SchemaEntry) -> Result<Option<SqlOperation>> {
    let columns = table_columns(sql, &table.name)?;
    if columns.is_empty() {
        return Ok(None);
    }

    let value_expressions = columns
        .iter()
        .enumerate()
        .flat_map(|(index, column)| {
            [
                format!("typeof({}) AS type_{index}", quote_identifier(&column.name)),
                format!(
                    "{} AS value_{index}",
                    encoded_value_expression(&column.name)
                ),
            ]
        })
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

    let rows = sql
        .exec(&query, None)?
        .raw()
        .map(|row| {
            let encoded = row?;
            if encoded.len() != columns.len() * 2 {
                return Err(rust_error(format!(
                    "Unexpected encoded column count in table {}",
                    table.name
                )));
            }
            encoded
                .chunks_exact(2)
                .map(|pair| decode_sql_value(&table.name, &pair[0], &pair[1]))
                .collect()
        })
        .collect::<Result<Vec<_>>>()?;

    Ok((!rows.is_empty()).then(|| SqlOperation::Insert {
        table: table.name.clone(),
        columns: columns.into_iter().map(|column| column.name).collect(),
        rows,
    }))
}

fn decode_sql_value(
    table_name: &str,
    kind: &SqlStorageValue,
    encoded: &SqlStorageValue,
) -> Result<SqlValue> {
    let SqlStorageValue::String(kind) = kind else {
        return Err(rust_error(format!(
            "Could not read SQLite value type in table {table_name}: {kind:?}"
        )));
    };
    let encoded_string = || match encoded {
        SqlStorageValue::String(value) => Ok(value.clone()),
        other => Err(rust_error(format!(
            "Could not encode {kind} value in table {table_name}: {other:?}"
        ))),
    };
    match kind.as_str() {
        "null" => Ok(SqlValue::Null),
        "integer" => Ok(SqlValue::Integer(encoded_string()?)),
        "real" => Ok(SqlValue::Real(encoded_string()?)),
        "text" => Ok(SqlValue::Text(encoded_string()?)),
        "blob" => Ok(SqlValue::Blob(encoded_string()?)),
        other => Err(rust_error(format!(
            "Unsupported SQLite value type in table {table_name}: {other}"
        ))),
    }
}

fn dump_sequence(sql: &SqlStorage) -> Result<Vec<SqlOperation>> {
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
               hex(CAST(name AS BLOB)) AS name_hex,
               CAST(seq AS TEXT) AS seq
             FROM sqlite_sequence
             ORDER BY name",
            None,
        )?
        .to_array()?;
    if rows.is_empty() {
        return Ok(Vec::new());
    }

    Ok(vec![
        SqlOperation::Statement {
            sql: "DELETE FROM sqlite_sequence;".to_string(),
        },
        SqlOperation::Insert {
            table: "sqlite_sequence".to_string(),
            columns: vec!["name".to_string(), "seq".to_string()],
            rows: rows
                .into_iter()
                .map(|row| vec![SqlValue::Text(row.name_hex), SqlValue::Integer(row.seq)])
                .collect(),
        },
    ])
}

fn statement(sql: impl Into<String>) -> SqlOperation {
    SqlOperation::Statement { sql: sql.into() }
}

fn build_sql_dump(sql: &SqlStorage) -> Result<Vec<SqlOperation>> {
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
        .map(|table| statement(terminate_statement(&table.sql)))
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
                .map(|entry| statement(terminate_statement(&entry.sql))),
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
    Ok(Dump::new(sql, kv))
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
    dump.validate_format()?;
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
                for operation in &dump.sql {
                    restore_sql_operation(&sql, operation)?;
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

fn restore_sql_operation(sql: &SqlStorage, operation: &SqlOperation) -> Result<()> {
    match operation {
        SqlOperation::Statement { sql: statement } => {
            sql.exec(statement, None)?;
        }
        SqlOperation::Insert {
            table,
            columns,
            rows,
        } => {
            for row in rows {
                let (statement, bindings) = prepare_insert(table, columns, row)?;
                sql.exec(&statement, Some(bindings))?;
            }
        }
    }
    Ok(())
}

fn prepare_insert(
    table: &str,
    columns: &[String],
    row: &[SqlValue],
) -> Result<(String, Vec<SqlStorageValue>)> {
    if columns.len() != row.len() || columns.is_empty() {
        return Err(rust_error(format!(
            "Invalid row width for table {table}: {} columns and {} values",
            columns.len(),
            row.len()
        )));
    }

    let mut bindings = Vec::new();
    let expressions = row
        .iter()
        .map(|value| match value {
            SqlValue::Null => Ok("NULL".to_string()),
            SqlValue::Integer(value) => {
                let value = value.parse::<i64>().map_err(rust_error)?;
                Ok(value.to_string())
            }
            SqlValue::Real(value) => {
                validate_real_literal(value)?;
                Ok(value.clone())
            }
            SqlValue::Text(value) => {
                bindings.push(SqlStorageValue::Blob(decode_hex(value)?));
                Ok("CAST(? AS TEXT)".to_string())
            }
            SqlValue::Blob(value) => {
                bindings.push(SqlStorageValue::Blob(decode_hex(value)?));
                Ok("?".to_string())
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let columns = columns
        .iter()
        .map(|column| quote_identifier(column))
        .collect::<Vec<_>>()
        .join(", ");
    Ok((
        format!(
            "INSERT INTO {} ({columns}) VALUES ({})",
            quote_identifier(table),
            expressions.join(", ")
        ),
        bindings,
    ))
}

fn validate_real_literal(value: &str) -> Result<()> {
    if matches!(value, "9.0e+999" | "-9.0e+999") {
        return Ok(());
    }
    let parsed = value.parse::<f64>().map_err(rust_error)?;
    if !parsed.is_finite() {
        return Err(rust_error(format!("Invalid SQLite real value: {value}")));
    }
    Ok(())
}

fn decode_hex(value: &str) -> Result<Vec<u8>> {
    if !value.len().is_multiple_of(2) {
        return Err(rust_error("Invalid hexadecimal SQLite value"));
    }
    value
        .as_bytes()
        .chunks_exact(2)
        .map(|pair| {
            let pair = std::str::from_utf8(pair).map_err(rust_error)?;
            u8::from_str_radix(pair, 16).map_err(rust_error)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use serde_json::json;
    use worker::SqlStorageValue;

    use super::{
        decode_sql_value, prepare_insert, Dump, KvEntry, SqlOperation, SqlValue, DUMP_FORMAT,
    };

    fn encoded(kind: &str, value: SqlStorageValue) -> SqlValue {
        decode_sql_value(
            "test table",
            &SqlStorageValue::String(kind.to_string()),
            &value,
        )
        .unwrap()
    }

    #[test]
    fn dump_uses_current_format() {
        let dump = Dump::new(
            vec![SqlOperation::Statement {
                sql: "CREATE TABLE example (value INTEGER);".to_string(),
            }],
            vec![KvEntry::new("version", json!(1))],
        );
        assert_eq!(dump.dump_format, DUMP_FORMAT);
        dump.validate_format().unwrap();
    }

    #[test]
    fn large_values_are_bound_instead_of_embedded_in_sql() {
        let value = vec![b'x'; 256 * 1024];
        let (statement, bindings) = prepare_insert(
            "example",
            &["text".to_string(), "blob".to_string()],
            &[
                SqlValue::Text(value.iter().map(|byte| format!("{byte:02X}")).collect()),
                SqlValue::Blob(value.iter().map(|byte| format!("{byte:02X}")).collect()),
            ],
        )
        .unwrap();

        assert_eq!(
            statement,
            "INSERT INTO \"example\" (\"text\", \"blob\") VALUES (CAST(? AS TEXT), ?)"
        );
        assert_eq!(
            bindings,
            vec![
                SqlStorageValue::Blob(value.clone()),
                SqlStorageValue::Blob(value)
            ]
        );
    }

    #[test]
    fn every_sql_value_type_translates_from_dump_to_restore() {
        let text = "quoted text \0 🎵".as_bytes().to_vec();
        let blob = vec![0x00, 0x01, 0x7f, 0x80, 0xfe, 0xff];
        let to_hex = |bytes: &[u8]| {
            bytes
                .iter()
                .map(|byte| format!("{byte:02X}"))
                .collect::<String>()
        };
        let row = vec![
            encoded("null", SqlStorageValue::Null),
            encoded("integer", SqlStorageValue::String(i64::MIN.to_string())),
            encoded(
                "real",
                SqlStorageValue::String("3.1415926535897931".to_string()),
            ),
            encoded("real", SqlStorageValue::String("9.0e+999".to_string())),
            encoded("real", SqlStorageValue::String("-9.0e+999".to_string())),
            encoded("text", SqlStorageValue::String(to_hex(&text))),
            encoded("blob", SqlStorageValue::String(to_hex(&blob))),
        ];

        assert_eq!(
            row,
            vec![
                SqlValue::Null,
                SqlValue::Integer(i64::MIN.to_string()),
                SqlValue::Real("3.1415926535897931".to_string()),
                SqlValue::Real("9.0e+999".to_string()),
                SqlValue::Real("-9.0e+999".to_string()),
                SqlValue::Text(to_hex(&text)),
                SqlValue::Blob(to_hex(&blob)),
            ]
        );

        let columns = [
            "null",
            "integer",
            "real",
            "positive infinity",
            "negative infinity",
            "text \"column",
            "blob",
        ]
        .map(str::to_string);
        let (statement, bindings) = prepare_insert("table \"name", &columns, &row).unwrap();

        assert_eq!(
            statement,
            "INSERT INTO \"table \"\"name\" (\"null\", \"integer\", \"real\", \
             \"positive infinity\", \"negative infinity\", \"text \"\"column\", \"blob\") \
             VALUES (NULL, -9223372036854775808, 3.1415926535897931, 9.0e+999, \
             -9.0e+999, CAST(? AS TEXT), ?)"
        );
        assert_eq!(
            bindings,
            vec![SqlStorageValue::Blob(text), SqlStorageValue::Blob(blob)]
        );
    }
}
