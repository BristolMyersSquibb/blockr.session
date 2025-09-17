deploy_to_connect <- function() {

  withr::local_temp_libpaths()

  install_github("BristolMyersSquibb/blockr.core")
  install_github("BristolMyersSquibb/blockr.session")

  dir <- withr::local_tempdir()

  file.copy(
    system.file("scripts", "app.R", package = "blockr.session"),
    dir
  )

  rsconnect::writeManifest(dir)

  bundle <- connectapi::bundle_dir(dir)

  connectapi::connect() |>
    connectapi::deploy(bundle, name = "block-core-session") |>
    connectapi::poll_task()
}

deploy_to_connect()
