#' Resolve the STURM input data directory.
#'
#' - **default** — bundled inputs in \code{message_ix_buildings/sturm/data/}.
#' - **private** — \code{<local-data>/buildings/sturm/}, a mirror of \file{sturm/data/}
#'   under :ref:`local data <local-data>` (typically the \code{message-static-data}
#'   repository root).
#'
#' Local data root is resolved by \code{local_data_root()}:
#' \enumerate{
#'   \item ixmp config \code{message_local_data} (\file{~/.local/share/ixmp/config.json})
#'   \item \file{sturm/local_data.yaml} key \code{local_data} (if ixmp is not set)
#' }
#' If neither is configured, private runs stop with setup instructions.
#'
#' Run \code{setup_sturm_local_data.R} once to write both files, or configure only
#' \file{local_data.yaml} if you do not use ixmp.
#'
#' Select the source with \code{--data=default} or \code{--data=private}, or
#' \env{STURM_DATA_SOURCE}.
#'
#' @param sturm_dir STURM working directory (parent of \file{data/} for default).
#' @param source Optional \code{"default"} or \code{"private"}; otherwise parsed
#'   from the CLI / environment.
#' @return Normalized path to the data directory (no trailing slash).
#' @export
resolve_sturm_data_dir <- function(sturm_dir = getwd(), source = NULL) {
  allowed <- c("default", "private")
  if (is.null(source) || !nzchar(source)) {
    source <- Sys.getenv("STURM_DATA_SOURCE", unset = "")
  }
  if (!nzchar(source)) {
    args <- commandArgs(trailingOnly = TRUE)
    data_arg <- grep("^--data=", args, value = TRUE)
    if (length(data_arg)) {
      source <- sub("^--data=", "", data_arg[1])
    }
  }
  if (!nzchar(source)) {
    source <- "default"
  }
  source <- tolower(source)
  if (!source %in% allowed) {
    stop(
      "STURM data source must be one of: ",
      paste(allowed, collapse = ", "),
      " (got: ", source, ")"
    )
  }

  if (identical(source, "default")) {
    path <- file.path(sturm_dir, "data")
    if (!dir.exists(path)) {
      stop("STURM default data directory not found: ", path)
    }
    message("STURM data (default): ", normalizePath(path, winslash = "/"))
    return(normalizePath(path, winslash = "/"))
  }

  root_info <- local_data_root(sturm_dir)
  path <- file.path(root_info$path, "buildings", "sturm")
  if (!dir.exists(path)) {
    stop(
      "STURM private data directory not found: ", path,
      "\nLocal data root (", root_info$source, "): ", root_info$path,
      "\nExpected a mirror of sturm/data/ at <local-data>/buildings/sturm/.",
      "\nConfigure the local data root in ixmp or sturm/local_data.yaml ",
      "(see setup_sturm_local_data.R)."
    )
  }
  message(
    "STURM data (private): ", normalizePath(path, winslash = "/"),
    " (local data via ", root_info$source, ")"
  )
  normalizePath(path, winslash = "/")
}

#' Return local data root and how it was resolved.
#'
#' @param sturm_dir STURM working directory (for \file{local_data.yaml} lookup).
#' @return List with \code{path} (character) and \code{source} (character label).
#' @export
local_data_root <- function(sturm_dir = getwd()) {
  from_ixmp <- ixmp_message_local_data()
  if (nzchar(from_ixmp)) {
    return(list(
      path = normalize_config_path(from_ixmp),
      source = "ixmp config (message_local_data)"
    ))
  }

  from_yaml <- sturm_local_data_yaml(sturm_dir)
  if (nzchar(from_yaml)) {
    return(list(
      path = normalize_config_path(from_yaml),
      source = "sturm/local_data.yaml"
    ))
  }

  stop(
    "Private STURM data requires a local data root, but none is configured.\n",
    "  1. ixmp users: set message_local_data in ~/.local/share/ixmp/config.json\n",
    "  2. R-only users: copy local_data.yaml.example to local_data.yaml and set local_data\n",
    "  3. Or run once: Rscript setup_sturm_local_data.R /path/to/message-static-data\n",
    "Private inputs must then live at: <local_data>/buildings/sturm/"
  )
}

#' @keywords internal
private_sturm_data_dir <- function(sturm_dir = getwd()) {
  file.path(local_data_root(sturm_dir)$path, "buildings", "sturm")
}

#' @keywords internal
normalize_config_path <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) {
    return("")
  }
  path <- path.expand(path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x) || !nzchar(x)) y else x

#' Read \code{local_data} from \file{sturm/local_data.yaml}.
#' @keywords internal
sturm_local_data_yaml <- function(sturm_dir = getwd()) {
  path <- file.path(sturm_dir, "local_data.yaml")
  if (!file.exists(path)) {
    return("")
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    warning("Install R package 'yaml' to read ", path)
    return("")
  }
  cfg <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (is.null(cfg) || !length(cfg)) {
    return("")
  }
  for (key in c("local_data", "message local data", "message_local_data")) {
    if (!is.null(cfg[[key]]) && nzchar(as.character(cfg[[key]]))) {
      return(as.character(cfg[[key]]))
    }
  }
  ""
}

#' @keywords internal
ixmp_message_local_data <- function() {
  for (path in ixmp_config_paths()) {
    value <- ixmp_config_value(path, "message_local_data")
    if (nzchar(value)) {
      return(value)
    }
  }
  ""
}

#' Paths to ixmp configuration files (existing files first).
#' @export
ixmp_config_paths <- function() {
  candidates <- c(
    file.path(ixmp_data_dir(), "config.json"),
    file.path(Sys.getenv("HOME", unset = ""), ".config", "ixmp", "config.yaml"),
    file.path(Sys.getenv("XDG_CONFIG_HOME", unset = ""), "ixmp", "config.yaml"),
    file.path(Sys.getenv("APPDATA", unset = ""), "ixmp", "config.yaml")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(existing)
  }
  file.path(ixmp_data_dir(), "config.json")
}

#' ixmp data directory (\file{~/.local/share/ixmp} on Linux/macOS).
#' @keywords internal
ixmp_data_dir <- function() {
  home <- Sys.getenv("HOME", unset = "")
  if (.Platform$OS.type == "windows") {
    base <- Sys.getenv("LOCALAPPDATA", unset = Sys.getenv("APPDATA", unset = home))
    return(file.path(base, "ixmp"))
  }
  xdg <- Sys.getenv("XDG_DATA_HOME", unset = "")
  if (nzchar(xdg)) {
    return(file.path(xdg, "ixmp"))
  }
  file.path(home, ".local", "share", "ixmp")
}

#' @keywords internal
read_ixmp_config <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  if (grepl("\\.json$", path, ignore.case = TRUE)) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      return(tryCatch(
        jsonlite::fromJSON(path, simplifyVector = FALSE),
        error = function(e) NULL
      ))
    }
    return(NULL)
  }
  if (grepl("\\.ya?ml$", path, ignore.case = TRUE) &&
      requireNamespace("yaml", quietly = TRUE)) {
    return(tryCatch(yaml::read_yaml(path), error = function(e) NULL))
  }
  NULL
}

#' @keywords internal
read_ixmp_config_value_python <- function(path, key) {
  py <- Sys.which("python3")
  if (!nzchar(py)) {
    return("")
  }
  cmd <- paste(
    shQuote(py),
    "-c",
    shQuote(sprintf(
      paste(
        "import json, pathlib, sys",
        "cfg = json.loads(pathlib.Path(sys.argv[1]).read_text())",
        "keys = [sys.argv[2], sys.argv[2].replace('_', ' ')]",
        "val = next((cfg[k] for k in keys if k in cfg and cfg[k]), '')",
        "print(val)"
      ),
      sep = "; "
    )),
    shQuote(path),
    shQuote(key)
  )
  out <- tryCatch(
    suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE)),
    error = function(e) character()
  )
  trimws(out[1] %||% "")
}

#' @keywords internal
write_ixmp_config <- function(path, cfg) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (grepl("\\.json$", path, ignore.case = TRUE)) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::write_json(cfg, path, auto_unbox = TRUE, pretty = TRUE)
      return(invisible(path))
    }
    stop("Install R package 'jsonlite' to write ", path)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Install R package 'yaml' to write ", path)
  }
  yaml::write_yaml(cfg, path)
  invisible(path)
}

#' @keywords internal
ixmp_config_value <- function(path, key) {
  cfg <- read_ixmp_config(path)
  if (!is.null(cfg) && length(cfg)) {
    keys <- c(key, gsub(" ", "_", key, fixed = TRUE), gsub("_", " ", key, fixed = TRUE))
    keys <- unique(keys)
    for (k in keys) {
      if (!is.null(cfg[[k]]) && nzchar(as.character(cfg[[k]]))) {
        return(as.character(cfg[[k]]))
      }
    }
  }
  if (grepl("\\.json$", path, ignore.case = TRUE)) {
    return(read_ixmp_config_value_python(path, key))
  }
  ""
}

#' Guess \code{message-static-data} next to common clone locations.
#' @keywords internal
detect_message_static_data <- function(sturm_dir = getwd()) {
  sturm_dir <- normalizePath(sturm_dir, winslash = "/", mustWork = FALSE)
  roots <- unique(c(
    dirname(dirname(dirname(sturm_dir))), # .../message-ix-buildings
    dirname(dirname(sturm_dir)),
    file.path(Sys.getenv("HOME", unset = ""), "scripts"),
    file.path(Sys.getenv("HOME", unset = ""), "git"),
    file.path(Sys.getenv("HOME", unset = ""), "src")
  ))
  roots <- roots[nzchar(roots)]

  for (root in roots) {
    candidate <- file.path(root, "message-static-data")
    if (dir.exists(candidate)) {
      return(candidate)
    }
  }
  ""
}

#' @keywords internal
platform_user_data_path <- function(appname) {
  py <- Sys.which("python3")
  if (nzchar(py)) {
    cmd <- paste(
      shQuote(py),
      "-c",
      shQuote(
        paste(
          "from platformdirs import user_data_path",
          sprintf("print(user_data_path(%s))", shQuote(appname)),
          sep = "; "
        )
      )
    )
    out <- tryCatch(
      suppressWarnings(system(cmd, intern = TRUE, ignore.stderr = TRUE)),
      error = function(e) character()
    )
    out <- trimws(out)
    if (length(out) && nzchar(out[1])) {
      return(out[1])
    }
  }

  home <- Sys.getenv("HOME", unset = "")
  if (.Platform$OS.type == "windows") {
    base <- Sys.getenv("APPDATA", unset = home)
    return(file.path(base, appname))
  }
  if (Sys.info()[["sysname"]] == "Darwin") {
    return(file.path(home, "Library", "Application Support", appname))
  }
  file.path(
    Sys.getenv("XDG_DATA_HOME", unset = file.path(home, ".local", "share")),
    appname
  )
}

#' Write \file{sturm/local_data.yaml} and optionally sync ixmp config.
#'
#' @param local_data Absolute path to the local data root (message-static-data).
#' @param sturm_dir STURM working directory.
#' @param sync_ixmp If \code{TRUE}, set \code{message local data} in ixmp config.
#' @return Invisibly, the normalized \code{local_data} path.
#' @export
write_sturm_local_data_config <- function(
    local_data,
    sturm_dir = getwd(),
    sync_ixmp = TRUE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Install R package 'yaml' (install.packages('yaml')).")
  }

  local_data <- normalize_config_path(local_data)
  if (!nzchar(local_data) || !dir.exists(local_data)) {
    stop("local_data is not an existing directory: ", local_data)
  }

  sturm_yaml <- file.path(sturm_dir, "local_data.yaml")
  yaml::write_yaml(list(local_data = local_data), sturm_yaml)
  message("Wrote ", sturm_yaml)

  if (isTRUE(sync_ixmp)) {
    write_ixmp_message_local_data(local_data)
  }

  invisible(local_data)
}

#' Set \code{message_local_data} in the ixmp config file.
#' @export
write_ixmp_message_local_data <- function(local_data) {
  local_data <- normalize_config_path(local_data)
  if (!nzchar(local_data) || !dir.exists(local_data)) {
    stop("local_data is not an existing directory: ", local_data)
  }

  config_path <- ixmp_config_paths()[1]
  cfg <- read_ixmp_config(config_path)
  if (is.null(cfg)) {
    cfg <- list()
  }

  cfg[["message_local_data"]] <- local_data
  write_ixmp_config(config_path, cfg)
  message("Wrote ixmp config: ", config_path)
  invisible(config_path)
}
