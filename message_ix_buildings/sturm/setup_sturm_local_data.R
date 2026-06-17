# One-time setup for --data=private (message-static-data).
#
# WHAT THIS DOES
#   Writes the path to your message-static-data clone so STURM can find
#   <local-data>/buildings/sturm/ (private inputs mirroring sturm/data/).
#
# WHERE THE PATH IS STORED (checked in this order when you use --data=private):
#   1. ~/.local/share/ixmp/config.json  →  key: message_local_data
#   2. sturm/local_data.yaml       →  key: local_data  (if ixmp is not set)
#
# USAGE (from the sturm folder):
#   Rscript setup_sturm_local_data.R /path/to/message-static-data
#   Rscript setup_sturm_local_data.R --sturm-only  /path/...  # yaml only, no ixmp
#   Rscript setup_sturm_local_data.R --ixmp-only   /path/...  # ixmp only, no yaml
#
# THEN RUN STURM:
#   Rscript run_STURM_bmt_resid.R --data=private
#   Rscript run_STURM_bmt_resid.R --data=default   # bundled sturm/data/ (no setup needed)

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", ca, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    setwd(dirname(normalizePath(script_path)))
  }
}

source(file.path(getwd(), "message_linking", "resolve_sturm_data_dir.R"))

args <- commandArgs(trailingOnly = TRUE)
sync_ixmp <- !any(args == "--sturm-only")
write_sturm_yaml <- !any(args == "--ixmp-only")
path_args <- setdiff(args, c("--ixmp-only", "--sturm-only"))

if (!length(path_args)) {
  stop(
    "Pass the message-static-data path, e.g.:\n",
    "  Rscript setup_sturm_local_data.R ~/scripts/message-static-data"
  )
}

local_data <- normalizePath(path_args[1], winslash = "/", mustWork = FALSE)
if (!dir.exists(local_data)) {
  stop("Directory not found: ", local_data)
}

private_data <- file.path(local_data, "buildings", "sturm")
if (!dir.exists(private_data)) {
  message(
    "Note: private STURM inputs not found yet at:\n  ",
    private_data,
    "\nCreate buildings/sturm/ (mirror of sturm/data/) when you add private inputs."
  )
}

if (write_sturm_yaml) {
  write_sturm_local_data_config(local_data, sturm_dir = getwd(), sync_ixmp = FALSE)
}
if (sync_ixmp) {
  write_ixmp_message_local_data(local_data)
}

info <- local_data_root(getwd())
message(
  "Done. Local data root: ", info$path,
  " (via ", info$source, ")\n",
  "Run STURM with: Rscript run_STURM_bmt_resid.R --data=private"
)
