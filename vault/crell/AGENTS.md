# Agents Guide — Crell

Instructions and constraints for AI agents (Claude Code, etc.) working on this project.

## What Agents Should Know

- Read `CLAUDE.md` first for architecture and design decisions.
- The frontend has **no build step** — changes to `priv/www/` are live immediately.
- jQuery is used for DOM manipulation even inside the AngularJS controller. This is intentional legacy style — don't refactor to Angular data-binding unless asked.
- The WebSocket protocol is the primary API surface. When adding features, model them as WS messages to `crell_server` or `goanna_api`.

## What Agents Should Not Do

- **Do not introduce new frontend dependencies** (npm packages, new CDN scripts) without explicit approval. The no-build-step constraint makes this a significant architectural change.
- **Do not refactor working code** unless the task explicitly asks for it. The frontend is functional legacy code; leave it alone unless there's a bug or a feature to add.
- **Do not add Bootstrap 4/5 classes** — the project uses Bootstrap 3. Class names differ (e.g., `hidden` vs `d-none`).
- **Do not push to remote** without confirmation.
- **Do not drop or truncate trace data** — trace output must be append-only in the UI until explicitly cleared.

## Preferred Approaches

| Scenario | Preferred approach |
|---|---|
| Large list UX (thousands of items) | `<input>` + `<datalist>` (native typeahead, no deps) |
| New WS message | Add handler in `ws.onmessage` / `gws.onmessage` switch block |
| Feature flag / conditional UI | `disabled` attribute toggling (existing pattern) |
| Styling tweaks | Inline style or existing Bootstrap 3 utilities |

## Decision Log

- **2026-04-08** — Module selector on traces page changed from `<select>` to `<input>` + `<datalist>` because the system loads thousands of modules, making a plain select unusable.
