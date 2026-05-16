#' Load scenarios from scenario YAML config.
#'
#' @param sturm_dir STURM working directory (contains default \code{scenario_config.yaml}).
#' @return Character vector of scenario names.
load_scenarios <- function(sturm_dir = getwd()) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Install R package 'yaml' to read scenario config (install.packages('yaml')).")
  }

  args <- commandArgs(trailingOnly = TRUE)
  config_arg <- grep("^--config=", args, value = TRUE)
  from_cli <- if (length(config_arg)) sub("^--config=", "", config_arg[1]) else ""

  path <- Sys.getenv("SCENARIO_CONFIG", unset = "")
  if (!nzchar(path)) {
    path <- if (nzchar(from_cli)) from_cli else file.path(sturm_dir, "scenario_config.yaml")
  }

  if (!file.exists(path)) {
    stop("Scenario config not found: ", path)
  }

  cfg <- yaml::read_yaml(path)
  scenarios <- cfg$scenarios
  if (is.null(scenarios) || !length(scenarios)) {
    stop("'scenarios' must be a non-empty list in: ", path)
  }

  unname(as.character(unlist(scenarios, use.names = FALSE)))
}
