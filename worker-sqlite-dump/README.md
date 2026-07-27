# worker-sqlite-dump

Logical dump and atomic restore for SQLite-backed Cloudflare Durable Objects using `worker-rs`.

```rust
use worker::{Result, Storage};
use worker_sqlite_dump::{export, restore, Dump};

async fn copy_out(storage: Storage) -> Result<Dump> {
    export(storage).await
}

async fn copy_in(storage: Storage, dump: Dump) -> Result<()> {
    restore(storage, dump).await
}
```

The dump is a serializable envelope containing ordered schema statements, structured table rows,
JSON-compatible application KV entries, and a format version. TEXT and BLOB bytes use UTF-8 when
that is valid and more compact in JSON, falling back to hexadecimal otherwise. Export and restore
each run inside a Durable Object storage transaction. Restore requires an empty target and restores
SQL and KV atomically. Row values are restored through bound parameters so large values do not
produce oversized SQL statements.

The crate preserves SQLite `NULL`, integers, real values, text bytes, BLOBs, generated columns,
indexes, views, triggers, and `sqlite_sequence`. It excludes Cloudflare and SQLite internal
objects.

Current limitations:

- application KV values must deserialize through `serde_json::Value`;
- virtual tables are rejected;
- the full dump is buffered in memory;
- the full dump must fit within Worker and Durable Object request-size limits;
- foreign-key insertion ordering is left to callers;
- behavior is specific to SQLite-backed Durable Objects.
