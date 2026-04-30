# Crell — Claude Code Guide

## Project Overview

Crell is an Erlang/OTP observability and debugging tool — think of it as a live dashboard for inspecting running BEAM nodes. It provides:

- Process inspection and supervision tree visualization
- Real-time tracing (module/function level, cluster-wide)
- Application and module browsing
- Database (ETS/Mnesia) inspection
- Recon integration

## Architecture

```
crell/
├── apps/
│   ├── crell/           # Core Erlang OTP app — node management, tracing, WS API
│   └── crell_web/       # Cowboy HTTP + WebSocket server
│       └── priv/www/    # Frontend (AngularJS 1.3, jQuery, Bootstrap)
└── vault/crell/         # This Obsidian vault — decisions, agent guidance
```

### Frontend Stack

- **AngularJS 1.3.9** — app scaffold (`CrellApp`, `CrellController`)
- **jQuery 1.11.2** — DOM manipulation (most pages use jQuery directly, not Angular)
- **Bootstrap 3** — layout and form controls
- No build step — all files served as-is from `priv/www/`

### Backend Stack

- **Erlang/OTP** — all backend logic
- **Cowboy** — HTTP and WebSocket server
- Two WebSocket endpoints:
  - `/crell/ws` — main crell API (nodes, tracing state, modules, functions)
  - `/goanna_api/ws` — goanna tracing API (start/stop traces, poll trace events)

### WebSocket Message Protocol

Messages are JSON. Request format: `{"module": "...", "function": "...", "args": [...]}`.

Key `crell_server` functions:
- `nodes` — list connected nodes
- `is_tracing` — check tracing state
- `toggle_tracing` — enable/disable tracing on a node
- `cluster_modules` — list all loaded modules
- `cluster_module_functions` — list functions for a module

Key `goanna_api` functions:
- `trace` — start trace pattern (mod, func, time, max_msgs)
- `stop_trace` — stop trace pattern(s)
- `list_active_traces` — active trace patterns
- `pull_all_traces` — fetch buffered trace events

## Key Design Decisions

### Module Selection UX (traces page)
The system can have thousands of loaded modules. The module selector uses a text input with HTML5 `<datalist>` for native browser typeahead filtering instead of a `<select>`. The hidden `<input id="trace_mod">` holds the validated selected value; `<input id="trace_mod_input">` is the visible search field.

### No Build Step
The frontend has no bundler, transpiler, or package manager. When adding UI features, use only what is already available: AngularJS, jQuery, Bootstrap. Do not introduce npm, webpack, or new CDN-loaded libraries without discussion.

### Tracing State Machine
Tracing must be explicitly enabled (`Enable tracing` button) before trace patterns can be added. The `is_tracing` WS response gates all tracing UI. This is intentional — tracing is expensive and should be opt-in.

## Development Notes

- The Erlang backend uses rebar3.
- Frontend changes take effect immediately (static files, no build).
- The `.asdf` file pins tool versions — use `asdf install` before building.
