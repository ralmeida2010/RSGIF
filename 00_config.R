# =============================================================================
# 00_config.R  --  Paths and options shared by all RSGIF scripts
# =============================================================================
# The project folder is taken from the environment variable RSGIF_DIR or, if it
# is not set, from the working directory (open the RStudio project, or setwd()
# to the repository root before sourcing).
#
# Credentials are NEVER written in the scripts. Put them in ~/.Renviron (see
# .Renviron.example) or set them in the session with Sys.setenv():
#   SGIF_USER, SGIF_PASS            HTTP download of the SGIF files (02)
# =============================================================================

project_dir <- normalizePath(Sys.getenv("RSGIF_DIR", unset = getwd()),
                             winslash = "/", mustWork = FALSE)
scripts_dir <- file.path(project_dir, "R")
data_dir    <- file.path(project_dir, "data")     # inputs (raw and processed)
output_dir  <- file.path(project_dir, "output")   # final tables, reports, checkpoints
base_path   <- project_dir                         # used by 17_external_validation.R

for (d in c(data_dir, file.path(data_dir, "processed"), output_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
rm(d)

options(stringsAsFactors = FALSE, timeout = max(600, getOption("timeout")))
message("RSGIF project folder: ", project_dir)
