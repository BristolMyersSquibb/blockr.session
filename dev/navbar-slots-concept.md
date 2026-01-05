# Navbar Slots Concept

## Overview

The navbar is split between two packages:

- **blockr.dock** provides the navbar shell with customization slots
- **blockr.session** (or other packages) populates those slots via `navbar_provider`

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ NAVBAR (blockr.dock provides shell)                                         │
│                                                                             │
│  ┌─────────────────────────────────┐        ┌────────────────────────────┐  │
│  │ LEFT SLOT                       │        │ RIGHT SLOT                 │  │
│  │ (navbar_provider populates)     │        │ (navbar_provider populates)│  │
│  │                                 │        │                            │  │
│  │ - Hamburger menu dropdown       │        │ - User avatar              │  │
│  │ - Editable workflow title       │        │ - (future items)           │  │
│  │ - Save status + button          │        │                            │  │
│  │ - New button                    │        │                            │  │
│  └─────────────────────────────────┘        └────────────────────────────┘  │
│                                                                             │
│  Fixed elements (stay in blockr.dock):                                      │
│  - Settings gear button (opens existing offcanvas sidebar)                  │
│  - Code button (blockr.core is always loaded with blockr.dock)              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Key Concepts

### navbar_provider vs dock_extension

| Type | Purpose | Creates Panel | Serialized |
|------|---------|---------------|------------|
| `dock_extension` | Provides UI in a dock panel | Yes | Yes |
| `navbar_provider` | Provides navbar content only | No | No |

Both can be passed in the `extensions` list when creating a `dock_board`. blockr.dock automatically separates them:
- `dock_extension` objects go into `dock_extensions(board)`
- `navbar_provider` objects go into `dock_navbar_providers(board)` (stored as attribute)

### Why navbar_provider?

- **No dock panel**: Session management doesn't need a panel
- **Not serialized**: Navbar providers are transient UI, not board state
- **Extensible**: Any package can provide navbar content, not just blockr.session
- **Clean separation**: blockr.dock doesn't need to know about blockr.session

## Usage

```r
library(blockr.dock)
library(blockr.session)

serve(
  new_dock_board(
    extensions = list(
      blockr.dag::new_dag_extension(),   # dock_extension -> creates panel
      new_session_extension()             # navbar_provider -> navbar only
    )
  )
)
```

## API

### navbar_provider (blockr.dock)

```r
new_navbar_provider <- function(id,
                                navbar_left_ui = NULL,
                                navbar_left_server = NULL,
                                navbar_right_ui = NULL,
                                navbar_right_server = NULL,
                                class = character())
```

### session_extension (blockr.session)

```r
new_session_extension <- function()
# Returns a navbar_provider with:
# - Left slot: Workflows dropdown, editable title, save button, new button
# - Right slot: User avatar
```

## Implementation Files

### blockr.dock

```
R/navbar-provider.R  - navbar_provider class
R/navbar-ui.R        - Navbar shell with slots
R/navbar-server.R    - Calls slot servers from providers
R/board-ui.R         - Include navbar above dockview
R/board-server.R     - Initialize navbar servers
R/dock-board.R       - Separate navbar_providers from extensions
R/ext-class.R        - Filter navbar_providers from extensions list
R/utils-serdes.R     - Restore navbar_providers after load
```

### blockr.session

```
R/session-extension.R   - new_session_extension() constructor
R/navbar-left-ui.R      - Tabbed dropdown, title, save, new
R/navbar-left-server.R  - Save, load, workflows/versions modals
R/navbar-right-ui.R     - User avatar
R/navbar-right-server.R - Avatar rendering
```
