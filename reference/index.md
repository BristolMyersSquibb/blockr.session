# Package index

## Board management plugin

The navbar plugin that adds project save, history and restore to a
board.

- [`manage_project()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
  [`manage_project_server()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
  [`manage_project_ui()`](https://bristolmyerssquibb.github.io/blockr.session/reference/manage_project.md)
  : Project management

## Storage backend contract

The generics and constructors a custom rack storage backend implements.

- [`new_rack_id()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`new_rack_record()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_content_hash()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_name()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_rename()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_exists()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_list()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`as_rack_id()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_info()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_download()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_upload()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_delete()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_purge()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_capabilities()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_tags()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_set_tags()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_acl()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_set_acl()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_share()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_unshare()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_shares()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  [`rack_find_users()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack-backend.md)
  : Rack storage backend contract

## Saving and loading

The high-level save and load API, and the board loader that restores
from a request URL.

- [`rack_create()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
  [`rack_append()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_create.md)
  : Create or append to a session record on a rack backend
- [`rack_load()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_load.md)
  : Load a session from a rack backend
- [`rack_loader()`](https://bristolmyerssquibb.github.io/blockr.session/reference/rack_loader.md)
  : Rack-backed board loader

## Default pins backend

The shipped storage backend, resolving per-user pin storage on Posit
Connect.

- [`user_pins_board()`](https://bristolmyerssquibb.github.io/blockr.session/reference/user_pins_board.md)
  : Posit Connect storage backend with graceful fallback
