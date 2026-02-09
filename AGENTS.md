# AGENTS.md - Guide for AI Agents Working on Arpeggio RPG

This document provides guidance for AI agents working on the Arpeggio RPG codebase, based on patterns discovered during development.

## Project Structure Overview

```
arpeggiorpg/
├── arptypes/           # Shared type definitions and commands
├── arpeggio/          # Core game logic and state management
├── arpui/             # Dioxus 0.7 frontend (WASM)
├── worker/            # Cloudflare Worker backend
├── foldertree/        # Campaign folder management
├── indexed/           # Custom indexed collections
└── nonempty/          # Non-empty collections
```

## Key Technologies

- **Frontend**: Dioxus 0.7 (Rust WASM framework)
- **Backend**: Cloudflare Workers (Rust)
- **Build Target**: `wasm32-unknown-unknown` (critical for compilation)
- **Styling**: Tailwind CSS 3 (note: uses `/10` opacity syntax, not `bg-opacity-*`)
- **Communication**: WebSocket via RPI (Arpeggio RPC Interface)

## Development Patterns

### 1. Command Structure

The codebase uses a dual-command system:
- `PlayerCommand` - Actions players can perform (security-restricted)
- `GMCommand` - Administrative actions (full game control)

**Key Insight**: Always prefer `PlayerCommand` when implementing player-facing features. Only use `GMCommand` for administrative operations.

### 2. Dioxus State Management

- **Global Signals**: `GAME`, `GAME_LOGS`, `GAME_NAME` for shared state
- **Local Signals**: `use_signal` for component-local state
- **Async Actions**: `use_action` hook for async operations (preferred over manual signal management)

### 3. Inventory System

Inventory is owner-based:
```rust
pub enum InventoryOwner {
    Scene(SceneID),
    Creature(CreatureID),
}
```

**Security Pattern**: Always validate player ownership before inventory operations:
```rust
if !player.creatures.contains(&creature_id) {
    return Err(GameError::CreatureNotFound(creature_id.to_string()));
}
```

### 4. UI Components

#### Modern Browser APIs
- Use Popover API for modals/tooltips (not legacy overlay techniques)
- CSS anchor positioning for tooltips
- Prefer native browser features over JavaScript polyfills

#### Dioxus 0.7 Patterns
```rust
// Correct event handling
let handle_click = move |evt: Event<MouseData>| {
    evt.stop_propagation();
    // Handle event
};

// Correct key handling
if evt.key() == Key::Escape { /* ... */ }

// Async actions (preferred)
let action = use_action({
    let ws = use_ws();
    move |data| {
        let ws = ws.clone();
        async move { // note: no need for tokio spawn()
            send_request(request, ws).await
        }
    }
});
```

## Code Block Formatting Requirements

**CRITICAL**: When showing code examples, always use this exact format:
```
```path/to/file.rs#L123-456
(code content here)
```
```

Never use language specifiers like `rust` or `javascript`. The Markdown parser only understands path-based syntax.

## Compilation Guidelines

### Always Use WASM Target
```bash
cargo check --target wasm32-unknown-unknown
cargo build --target wasm32-unknown-unknown
```

### Handle Incremental Compilation Issues
If you encounter "scalar size mismatch" or ICE errors:
```bash
cargo clean
# or for specific package:
cd arpui && cargo clean
```

## Testing Patterns

### Test Structure
Tests are in `src/game.rs` under `#[cfg(test)] pub mod test`.

**Helper Functions Available**:
- `t_game()` - Creates test game with creatures, scenes, abilities
- `t_perform(game, GMCommand)` - Executes GM command
- `t_player_perform(game, player_id, PlayerCommand)` - Executes player command
- `cid_rogue()`, `cid_ranger()`, `cid_cleric()` - Test creature IDs
- `t_scene_id()` - Test scene ID

### Test Best Practices
1. **Compare entire values**, not just counts or individual fields:
   ```rust
   assert_eq!(*result.get_owner_inventory(owner).unwrap(), HashMap::from([(item_id, count)]));
   ```

2. **Test both success and failure cases**
3. **Validate security constraints** (ownership, scene membership)

## Security Considerations

### Player Command Validation Pattern
1. Verify player owns source creatures
2. Verify target creatures/scenes are accessible
3. Verify player is in appropriate scene
4. Use existing `GameLog` infrastructure for state changes

### Example Security Implementation
```rust
// Validate player owns creature
if !player.creatures.contains(&creature_id) {
    return Err(GameError::CreatureNotFound(creature_id.to_string()));
}

// Validate player is in scene
let scene_id = player.scene.ok_or(GameError::BuggyProgram(
    "Player isn't in a scene".to_string(),
))?;

// Validate creature is in scene
if !scene.creatures.contains_key(&creature_id) {
    return Err(GameError::CreatureNotFound(creature_id.to_string()));
}
```

## Common Pitfalls

### 1. Tailwind CSS Version
- ❌ `bg-opacity-50` (Tailwind 2)
- ✅ `bg-black/50` (Tailwind 3)

### 2. Event Handling
- ❌ `evt.target()` (doesn't exist in Dioxus 0.7)
- ✅ `evt.stop_propagation()`, `evt.key()`, `evt.value()`

### 3. Async Actions & State Management
- ❌ Manual `is_loading` signals
- ✅ `use_action` hook with `.pending()` and `.call()`
- ❌ `onclick: move |_| { on_thing() }`
- ✅ `onclick: move |_| action.call()`

### 4. Unused Variables & Props
- ❌ `let _ = some_value` as a general way to silence unused warnings.
- ✅ Prefer renaming intentionally-unused bindings to `_some_value`.
- ✅ If a prop/parameter is now unused, prefer removing it and updating call sites.

## File Organization

### Adding New Components
1. Create directory: `arpui/src/components/component_name/`
2. Files needed:
   - `mod.rs` - Exports
   - `component.rs` - Implementation
   - `style.css` - Styling (if needed)
3. Update `arpui/src/components/mod.rs` to include new module

### Adding New Commands
1. Add to `arptypes/src/command.rs` (PlayerCommand or GMCommand)
2. Implement in `arpeggio/src/game.rs` (`perform_player_command` or `perform_gm_command`)
3. Add comprehensive tests
4. Update UI to use new commands

## Communication Patterns

### WebSocket Requests
```rust
let request = RPIGameRequest::PlayerCommand {
    command: PlayerCommand::GiveItem {
        from_creature_id,
        to_creature_id,
        item_id,
        count,
    },
};
send_request::<()>(request, ws).await
```

### Error Handling
- Use `Result<T, GameError>` for game logic
- Use `anyhow::Result` for I/O operations
- Prefer specific error types over generic strings

## Performance Notes

- WASM compilation can be slow; use `cargo check --target wasm32-unknown-unknown` during development
- Global signals should be used sparingly (only for truly global state)

## Future Development Guidelines

1. **Always add tests** for new functionality
2. **Prefer player commands** over GM commands for user features
3. **Use modern web APIs** when available
4. **Follow existing patterns** for consistency
5. **Document security implications** of new features
6. **Test on WASM target** - don't assume native compilation success

## Resources

- Dioxus 0.7 docs: Focus on hooks and event handling changes
- Tailwind 3 docs: Note opacity syntax changes
- Test existing functionality before adding new features
