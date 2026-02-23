deploy_to_connect <- function(app, name,
                              pkgs = c("blockr.core", "blockr.session")) {

  if (length(pkgs)) {
    lib <- withr::local_tempdir()
    withr::local_libpaths(lib, "prefix")
  }

  for (pkg in paste0("BristolMyersSquibb/", pkgs)) {
    remotes::install_github(pkg, upgrade = FALSE, lib = lib)
  }

  dir <- withr::local_tempdir()

  file.copy(
    system.file("scripts", app, package = "blockr.session"),
    file.path(dir, "app.R")
  )

  rsconnect::writeManifest(dir, appPrimaryDoc = "app.R", appMode = "shiny")

  bundle <- connectapi::bundle_dir(dir)

  connectapi::connect() |>
    connectapi::deploy(bundle, name = name) |>
    connectapi::poll_task()
}

deploy_to_connect(
  app = "app-full.R",
  name = "blockr-project",
  pkgs = c(
    "blockr.core",
    "blockr.dock",
    "blockr.dag",
    "blockr",
    "blockr.session@14-user"
  )
)
