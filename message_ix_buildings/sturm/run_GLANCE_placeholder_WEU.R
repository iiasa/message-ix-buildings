# ============================================================
# Placeholder for GLANCE runs (WEU)
# Writes blank MESSAGE-linking CSV(s); does not run GLANCE.
# ============================================================

library(rstudioapi)


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

script_path <- tryCatch(
  rstudioapi::getSourceEditorContext()$path,
  error = function(e) ""
)

if (nzchar(script_path)) {
  setwd(dirname(script_path))
}

dir_message_linking <- file.path(
  getwd(),
  "message_linking"
)

dir.create(
  dir_message_linking,
  recursive = TRUE,
  showWarnings = FALSE
)

source(
  file.path(
    dir_message_linking,
    "load_scenario_config.R"
  )
)


# ------------------------------------------------------------
# 1. Write blank linking outputs
# ------------------------------------------------------------

scenarios <- load_scenarios()

cat(
  "\nGLANCE placeholder\nScenarios: ",
  paste(scenarios, collapse = ", "),
  "\n",
  sep = ""
)

for (code in scenarios) {

  out_file <- file.path(
    dir_message_linking,
    paste0("resid_comm_glance_aligned_", code, ".csv")
  )

  # Blank CSV placeholder (same schema as STURM linking exports;
  # any future numeric values should be rounded to 4 decimals)
  blank <- data.frame(
    node = character(),
    commodity = character(),
    level = character(),
    year = integer(),
    time = character(),
    value = numeric(),
    unit = character(),
    stringsAsFactors = FALSE
  )

  write.csv(
    blank,
    out_file,
    row.names = FALSE
  )

  cat("Wrote blank file:\n", out_file, "\n", sep = "")
}
