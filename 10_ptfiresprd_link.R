# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# 26-ID_PT-FireSprd_Fogos_Association.R  --  Link to the PT-FireSprd database
# =============================================================================
# SUMMARY (English)
#   Links the PRFD records to the Portuguese Large Wildfire Spread database
#   (PT-FireSprd; Benali et al., 2023) through the incident code of the
#   Civil Protection (CodigoSado), which is the "Incident ID" of the PT-FireSprd
#   metadata. A metadata row may list several incident codes separated by "/".
#   Output: column IDPT-FireSprd in fogos (NA when the fire is not in
#   PT-FireSprd) and the number of linked fires (84 in the manuscript).
#
# REVIEW NOTES (changes marked "# REVIEW:")
#   - If one incident code appeared in several PT-FireSprd rows, the join
#     duplicated the fire records; the PT-FireSprd IDs of an incident are now
#     collapsed into one value ("ID1;ID2") before the join, and the number of
#     rows of fogos is checked.
#   - Empty incident codes are ignored.
#   - The path is built from data_dir / project_dir.
#   - Manuscript (Sect. 2.3): the link uses the incident code (CodigoSado), not
#     the PRFD identifier (Codigo).
# =============================================================================

# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

suppressPackageStartupMessages({ library(readxl); library(dplyr); library(tidyr) })

if (!exists("project_dir")) project_dir <- Sys.getenv("RSGIF_DIR", getwd())
file_path <- file.path(project_dir, "PT-FireSprd", "PT-FireSprd_Metadata.xls")

metadata <- read_excel(file_path)

metadata_clean <- metadata %>%
        separate_rows(`Incident ID`, sep = "/") %>%
        mutate(`Incident ID` = trimws(as.character(`Incident ID`))) %>%
        filter(!is.na(`Incident ID`), `Incident ID` != "") %>%                     # REVIEW
        distinct(`Incident ID`, ID) %>%
        # REVIEW: one row per incident code (several PT-FireSprd IDs collapsed)
        group_by(`Incident ID`) %>%
        summarise(`IDPT-FireSprd` = paste(sort(unique(as.character(ID))), collapse = ";"), .groups = "drop")

n_before <- nrow(fogos)
fogos <- fogos %>%
        select(-any_of("IDPT-FireSprd")) %>%
        mutate(CodigoSado = trimws(as.character(CodigoSado))) %>%
        left_join(metadata_clean, by = c("CodigoSado" = "Incident ID"))
stopifnot(nrow(fogos) == n_before)                                                  # REVIEW

matches_count <- sum(!is.na(fogos$`IDPT-FireSprd`))
cat("--------------------------------------------\n",
    "Fires linked to PT-FireSprd:", matches_count, "\n",
    "PT-FireSprd incident codes found in the PRFD:",
    sum(metadata_clean$`Incident ID` %in% fogos$CodigoSado), "of", nrow(metadata_clean), "\n",
    "Codes with more than one PT-FireSprd ID:", sum(grepl(";", metadata_clean$`IDPT-FireSprd`)), "\n",
    "--------------------------------------------\n")
rm(metadata, metadata_clean, n_before, matches_count)
