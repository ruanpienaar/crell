# Node Management

Nodes are the foundation of Crell. Everything else — processes, modules, tracing, DB inspection — operates against one or more connected nodes. Nodes are grouped into **clusters**, so cluster-wide operations target all nodes that share a cluster name.

## Concepts

### Node
A remote BEAM node (e.g. `myapp@192.168.1.10`) that Crell connects to and monitors. Connection is managed by [hawk](https://github.com/odo/hawk), which handles reconnection automatically.

### Cluster
A named group of nodes. Nodes in the same cluster share a `cluster_name`. When Crell runs a cluster-wide operation (e.g. `cluster_modules`, `cluster_application_consistency`) it targets all nodes in the cluster.

Cluster names are user-defined strings. If no name is given when adding a node, one is auto-generated (random base-36 string).

## Storage

Nodes are persisted in a Mnesia table (`crell_nodes`) with the following schema:

| Field          | Type   | Notes                                      |
|----------------|--------|--------------------------------------------|
| `node`         | atom   | Primary key — the Erlang node name         |
| `cookie`       | atom   | Erlang distribution cookie                 |
| `connected`    | bool   | Updated by hawk connect/disconnect events  |
| `cluster_name` | string | User-supplied or auto-generated            |

The table uses `disc_copies` so nodes survive restarts. On startup, `crell_server:init` reads all stored nodes and re-registers them with hawk, preserving their original `cluster_name`.

## Adding a Node

**UI flow** (`index.html` → `nodes.js` → `/crell/ws`):

1. Fill in **Node** (e.g. `myapp@192.168.1.10`) and **Cookie**.
2. Assign a cluster — either pick an existing one from the dropdown or type a new name. Leave both blank for an auto-generated name.
3. Click **Save**.

The UI sends `add_node` over the WebSocket with `[node, cookie, cluster_name]`. The backend:
1. Creates the mnesia record (preserving the cluster name).
2. Fires `node_connecting` via `crell_notify` — all connected browser sessions see the node appear in the **Connecting** list immediately.
3. Registers the node with hawk, which begins attempting the Erlang distribution connection.
4. When hawk connects, `node_connected` fires — the node moves from **Connecting** to **Existing nodes** across all sessions.

## Editing a Node

Click **Edit** next to a node in either the connecting or existing list. The form populates with the current values and the heading changes to **Edit node**.

- If only **cookie** or **cluster name** changes: the mnesia record is updated in place. Hawk is not disturbed — the connection stays live. A `node_edited` event is broadcast so all sessions refresh their cluster dropdown.
- If the **node name** itself changes: the old node is removed (hawk tears it down, `node_deleted` fires) and the new node is added from scratch (`node_connecting` fires for the new name).

## Deleting a Node

Select one or more nodes and click **Delete**. This removes the mnesia record, purges cached remote state, and tells hawk to stop managing the connection. A `node_deleted` event is broadcast — all sessions remove the node from both the connecting and existing lists.

## Pub/Sub — Reactive Updates

The nodes page subscribes to `{node_events}` on WebSocket open. All state changes are pushed to every connected browser session:

| Event             | Trigger                                      | UI effect                                  |
|-------------------|----------------------------------------------|--------------------------------------------|
| `node_connecting` | `add_node` called / hawk begins connecting   | Node appears in **Connecting** list        |
| `node_connected`  | hawk establishes distribution connection     | Node moves to **Existing nodes**           |
| `node_disconnected` | hawk loses connection                      | Node moves back to **Connecting**          |
| `node_deleted`    | node removed                                 | Node removed from both lists               |
| `node_edited`     | metadata updated in place                    | Cluster dropdown refreshed                 |

## WebSocket API Reference

All messages use the standard Crell WS protocol: `{"module": "crell_server", "function": "...", "args": [...]}`.

| Function       | Args                                      | Response                        |
|----------------|-------------------------------------------|---------------------------------|
| `nodes`        | `[]`                                      | `{nodes: [...]}`                |
| `connecting_nodes` | `[]`                                  | `{connecting_nodes: [...]}`     |
| `clusters`     | `[]`                                      | `{clusters: [...]}`             |
| `get_node`     | `[node]`                                  | `{node_info: {node, cookie, cluster_name}}` |
| `add_node`     | `[node, cookie, cluster_name]`            | `{connecting_nodes: [...]}`     |
| `edit_node`    | `[orig_node, new_node, cookie, cluster_name]` | `{clusters: [...]}`         |
| `del_node`     | `[node, ...]`                             | `{reply: ok}`                   |
| `conn_del_node` | `[node, ...]`                            | `{reply: ok}`                   |
| `discover_neighbour_nodes` | `[node, ...]`                | `{nodes: [...]}`                |
