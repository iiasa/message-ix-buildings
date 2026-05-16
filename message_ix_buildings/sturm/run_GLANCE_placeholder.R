# Placeholder runner for GLANCE model (for now it only creates static copies of ACCESS results)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    setwd(dirname(normalizePath(script_path)))
  } else {
    stop("Cannot set working directory: run from RStudio, or `Rscript run_GLANCE_placeholder.R` from the sturm folder.")
  }
}

dir_message_linking <- file.path(getwd(), "message_linking")
dir.create(dir_message_linking, recursive = TRUE, showWarnings = FALSE)
source(file.path(dir_message_linking, "load_scenario_config.R"))
scenarios <- load_scenarios()

data_dir <- file.path(getwd(), "data")
path_cook <- file.path(data_dir, "access_cook.csv")
path_app <- file.path(data_dir, "access_app.csv")

missing <- c(if (!file.exists(path_cook)) path_cook, if (!file.exists(path_app)) path_app)
if (length(missing)) {
  stop(
    "Missing GLANCE base inputs under sturm/data:\n",
    paste0("  ", missing, collapse = "\n"),
    "\nAdd access_cook.csv and access_app.csv, then re-run."
  )
}

for (s in scenarios) {
  out_glance <- file.path(dir_message_linking, paste0("resid_comm_glance_", s, ".csv"))
  glance <- rbind(
    read.csv(path_cook, stringsAsFactors = FALSE),
    read.csv(path_app, stringsAsFactors = FALSE)
  )
  write.csv(glance, out_glance, row.names = FALSE)
  message("GLANCE placeholder: wrote ", basename(out_glance))
}

message("GLANCE placeholder done (", length(scenarios), " scenario(s)).")
