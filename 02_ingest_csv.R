# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# 2-OpenDatabaseCSV.R  --  Ingestion and consolidation of the PRFD (Step 1)
# =============================================================================
#
# PURPOSE
#   Downloads the raw SGIF/ICNF files, loads the two legacy record tables
#   (1980-2000 and 2001-present), merges them into a single table `fogos`,
#   recovers missing parish codes (INE) from the ignition coordinates, updates
#   the administrative names from the official reference table, and exports the
#   consolidated table. This is Step 1 of the PRFD pipeline (Fig. 2 / Fig. B1);
#   the cleaning, imputation, and reconciliation steps run in later scripts.
#
# INPUTS (downloaded from https://fogos.icnf.pt/download/ExportarDadosSGIF/,
#         HTTP basic authentication)
#   Data1980_2000.csv             fire records 1980-2000 (periods t1-t4)
#   Data2001_now.csv              fire records 2001-present (period t5)
#   Data1980_2000eliminados.csv   records removed by the Forest Service (1980-2000)
#   Data2001_noweliminados.csv    records removed by the Forest Service (2001-)
#   AdministrativeRelations.csv   INE code -> district, municipality, parish
#   CAOP2012.zip                  official parish boundaries (CAOP 2012)
#   Other files (BurnedAreas*, FRP15, meteorology, fogos.*) are downloaded here
#   for the later scripts but are not used in this one.
#
# PROCESSING
#   0. Load packages.
#   1. Set the project, data, and output folders.
#   2. Define text-cleaning helpers: in every character column, commas are
#      replaced by semicolons and line breaks removed, so that the exported CSV
#      cannot be broken by free-text fields (e.g. Observacoes, Local).
#   3-5. Download all files (credentials from environment variables) and unzip
#      the .zip archives into data/<name>/.
#   6. Read the four record tables and the administrative table. All columns
#      are type-guessed from the whole file (guess_max = Inf), not only from the
#      first 1000 rows, so that sparse columns (e.g. coordinates) are not lost.
#   7. Convert the 1980-2000 coordinates from PT-TM06 (EPSG:3763) to WGS84
#      (EPSG:4326) and store them in Lat_4326 / Lon_4326.
#   8. Merge the two periods into `fogos`; harmonize "Viana do Castelo"; drop
#      auxiliary columns; keep only records up to `max_year` (records of the
#      current, incomplete year are excluded); check that Codigo is unique; print
#      the number of records per data period (t1-t5) for comparison with Table 1.
#   9. Assign INE codes to records without INE but with coordinates, using the
#      nearest CAOP 2012 parish polygon; the change is noted in Observacoes.
#  10. Join the administrative reference table by INE and overwrite district,
#      municipality, and parish names with the official ones.
#  11. Export output/fogos_consolidado.csv.
#  12. (Optional, off by default) Load the FINAL processed outputs of the whole
#      pipeline (fogos.gpkg, BurnedAreasClosed.gpkg, FRP15.csv) for analysis.
#      This block replaces `fogos` and must NOT be run before scripts 3-4.
#
# OUTPUT
#   output/fogos_consolidado.csv   consolidated fire records (input of script 3)
#   objects in memory: fogos, fogoseliminados, AdministrativeRelations
#
# REVIEW NOTES (changes from the previous version are marked "# REVIEW:")
#   - Credentials are read from the environment (SGIF_USER, SGIF_PASS).
#   - Duplicated download entry removed (BurnedAreasClosed.zip).
#   - guess_max = Inf in read_delim (column types guessed from all rows).
#   - Coordinates converted in place instead of a join by Codigo (a join
#     duplicates rows if Codigo is not unique, and creates .x/.y columns if
#     Lat_4326/Lon_4326 already exist).
#   - Records after `max_year` excluded (the 390 records of 2026).
#   - Uniqueness check of Codigo and counts per data period added.
#   - CAOP polygons transformed to EPSG:3763 before the nearest-feature search;
#     Observacoes no longer becomes "NA ; ..." when it was empty.
#   - AdministrativeRelations de-duplicated by INE before the join, and the
#     ".ref" helper columns removed after it.
#   - Block 12 (final outputs) guarded by a switch, so that running the whole
#     script does not overwrite the consolidated `fogos`.
#   - Time zone: date-times are read as UTC (readr default), as before. If the
#     SGIF times are local (Europe/Lisbon), set tz_data below; this matters for
#     the hourly ERA5 join in the later scripts.
# =============================================================================


# =========================================
# 0. LIBRARY ACTIVATION
# =========================================

# List of required packages
# REVIEW: jmv, jmvcore, jmvconnect, jmvReadWrite and reshape2 were removed: no script
# of the pipeline uses them; DBI and odbc are not needed (no database connection).
packages <- c(
        "dplyr", "sf", "readr", "httr", "lubridate", "ggplot2", "utils"
)

# Function to automatically install and load packages
install_if_missing <- function(p) {
        if (!require(p, character.only = TRUE)) {
                install.packages(p, dependencies = TRUE)
                library(p, character.only = TRUE)
        }
}

invisible(lapply(packages, install_if_missing))
cat("✔ All libraries have been loaded successfully.\n")

# =========================================
# 1. DIRECTORY AND ENVIRONMENT CONFIGURATION
# =========================================

if (!exists("project_dir")) project_dir <- Sys.getenv("RSGIF_DIR", getwd())
if (!exists("data_dir"))    data_dir    <- file.path(project_dir, "data")
if (!exists("output_dir"))  output_dir  <- file.path(project_dir, "output")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
setwd(project_dir)

# REVIEW: parameters of the consolidation
max_year           <- 2025      # last complete year of the series
tz_data            <- "UTC"     # time zone used to read date-times ("Europe/Lisbon" if local)
load_final_outputs <- FALSE     # TRUE only to load the final pipeline outputs (block 12)

# =========================================
# 2. CLEANING HELPER FUNCTIONS
# =========================================

# Replace commas by semicolons and remove line breaks in a character vector
replace_problems <- function(x) {
        if (!is.character(x)) return(x)
        x <- gsub(",", ";", x)
        x <- gsub("[\r\n]", "", x)
        return(x)
}

# Apply the cleaning to all character columns of a data frame
clean_df <- function(df) {
        df %>% mutate(across(where(is.character), replace_problems))
}

# =========================================
# 3. HTTPS ACCESS CONFIGURATION
# =========================================

# REVIEW: credentials are no longer written in the script.
# Set them once per session with Sys.setenv(SGIF_USER = "...", SGIF_PASS = "...")
userhttps <- Sys.getenv("SGIF_USER", "xxxxx")
passhttps <- Sys.getenv("SGIF_PASS", "xxxxx")
auth <- authenticate(userhttps, passhttps, type = "basic")

# Download one file from the SGIF server into data_dir
download_sgif <- function(file_name) {
        url  <- paste0("https://fogos.icnf.pt/download/ExportarDadosSGIF/", file_name)
        dest <- file.path(data_dir, file_name)

        # write_disk streams the file directly to disk (safer for large files)
        res <- GET(url, auth, write_disk(dest, overwrite = TRUE), progress())

        if (status_code(res) == 200) {
                cat("✔ Success:", file_name, "\n")
        } else {
                # Delete the empty or corrupt file created by write_disk
                if (file.exists(dest)) file.remove(dest)
                warning("❌ Download failed for: ", file_name, " (Status: ", status_code(res), ")")
        }
}

# =========================================
# 4. EXECUTE DOWNLOADS
# =========================================

# REVIEW: "BurnedAreasClosed.zip" appeared twice; duplicates are removed
files_to_download <- unique(c(
        "AdministrativeRelations.csv",
        "BurnedAreasClosed.zip",
        "BurnedAreasOpen.zip",
        "CAOP2012.zip",
        "DailyMeanMeteoDistritoFrom1980.csv",
        "DailyMeanMeteoFrom1980.csv",
        "Data1980_2000.csv",
        "Data1980_2000eliminados.csv",
        "Data2001_now.csv",
        "Data2001_noweliminados.csv",
        "Fires_TotalMeteo.csv",
        "fogos.csv",
        "FRP15.csv",
        "fogos.zip"
))

cat(">>> Starting file downloads...\n")
invisible(lapply(files_to_download, download_sgif))

# =========================================
# 5. AUTOMATIC UNZIP OF ALL ZIP FILES
# =========================================

zip_files <- files_to_download[grepl("\\.zip$", files_to_download)]

for (z_file in zip_files) {
        path_to_zip <- file.path(data_dir, z_file)
        if (file.exists(path_to_zip)) {
                ex_dir <- file.path(data_dir, sub("\\.zip$", "", z_file))
                unzip(path_to_zip, exdir = ex_dir)
                cat(paste0("✔ ", z_file, " unzipped into: ", basename(ex_dir), "/\n"))
        } else {
                cat(paste0("✖ Warning: ", z_file, " not found for extraction.\n"))
        }
}
cat(">>> All downloads and extractions completed.\n")

# =========================================
# 6. LOADING DATA INTO MEMORY
# =========================================
cat(">>> Loading CSV files into R...\n")

# Read a CSV file and clean its character columns
read_and_clean <- function(file_name) {
        path <- file.path(data_dir, file_name)
        if (file.exists(path)) {
                # REVIEW: guess_max = Inf -> types guessed from all rows (columns that
                # are empty in the first 1000 rows, such as coordinates, are not lost);
                # tz_data controls how date-times are interpreted.
                df <- read_delim(path, delim = ",",
                                 locale = locale(encoding = "UTF-8", tz = tz_data),
                                 guess_max = Inf, show_col_types = FALSE)
                return(clean_df(df))
        } else {
                warning("File not found: ", file_name)
                return(NULL)
        }
}

Data1980_2000           <- read_and_clean("Data1980_2000.csv")
Data2001_now            <- read_and_clean("Data2001_now.csv")
Data1980_2000eliminados <- read_and_clean("Data1980_2000eliminados.csv")
Data2001_noweliminados  <- read_and_clean("Data2001_noweliminados.csv")
AdministrativeRelations <- read_and_clean("AdministrativeRelations.csv")

# =========================================
# 7. GEOGRAPHIC PROCESSING (EPSG:3763 -> EPSG:4326)
# =========================================
cat(">>> Processing coordinates (EPSG 3763 -> 4326)...\n")

if (!is.null(Data1980_2000)) {
        # REVIEW: conversion in place (no join by Codigo)
        if (!"Lat_4326" %in% names(Data1980_2000)) Data1980_2000$Lat_4326 <- NA_real_
        if (!"Lon_4326" %in% names(Data1980_2000)) Data1980_2000$Lon_4326 <- NA_real_

        ok <- !is.na(Data1980_2000$x_3763) & !is.na(Data1980_2000$y_3763)
        if (any(ok)) {
                xy <- st_coordinates(st_transform(
                        st_as_sf(Data1980_2000[ok, c("x_3763", "y_3763")],
                                 coords = c("x_3763", "y_3763"), crs = 3763),
                        4326))
                Data1980_2000$Lat_4326[ok] <- xy[, 2]
                Data1980_2000$Lon_4326[ok] <- xy[, 1]
        }
        cat("✔ 1980-2000 coordinates converted for", sum(ok), "records.\n")
}

# =========================================
# 8. CONSOLIDATING THE 'FOGOS' DATABASE
# =========================================
cat(">>> Consolidating 'fogos' and 'fogoseliminados' datasets...\n")

# Merge the two periods (bind_rows aligns columns by name)
fogos <- bind_rows(Data1980_2000, Data2001_now) %>%
        mutate(Distrito = ifelse(Distrito == "Viana Do Castelo", "Viana do Castelo", Distrito))

# Records removed by the Forest Service (kept for reference, not merged)
fogoseliminados <- bind_rows(Data1980_2000eliminados, Data2001_noweliminados)

# Remove auxiliary columns
# REVIEW: "Fogacho" and "Agricola" flag small and agricultural fires; consider
# keeping them, since they document which occurrence types are in the PRFD.
cols_to_remove <- c("Nut2", "Nut3", "...1", "...2", "Fogacho", "Agricola")
fogos <- fogos %>% dplyr::select(-any_of(cols_to_remove))

# REVIEW: keep only complete years (removes the records of the current year)
year_col <- if ("Ano" %in% names(fogos)) fogos$Ano else lubridate::year(fogos$DHInicio)
n_after  <- sum(year_col > max_year, na.rm = TRUE)
fogos    <- fogos[is.na(year_col) | year_col <= max_year, ]
cat("✔ Records after", max_year, "removed:", n_after, "\n")

# REVIEW: Codigo must be unique (it is the key of all later joins)
n_dup <- sum(duplicated(fogos$Codigo))
if (n_dup > 0) warning("Codigo is not unique: ", n_dup, " duplicated codes")

# REVIEW: records per data period, to compare with Table 1 of the manuscript
yr <- if ("Ano" %in% names(fogos)) fogos$Ano else lubridate::year(fogos$DHInicio)
print(table(Periodo = cut(yr, c(1980, 1993, 1994, 1996, 2001, max_year + 1), right = FALSE,
                          labels = c("t1", "t2", "t3", "t4", "t5")), useNA = "ifany"))
cat("✔ Base consolidated. Total records:", nrow(fogos), "\n")

# =========================================
# 9. INE UPDATE VIA CAOP (SPATIAL JOIN)
# =========================================
cat(">>> Recovering missing INE codes via CAOP 2012...\n")

shp_path <- list.files(file.path(data_dir, "CAOP2012"), pattern = "\\.shp$", full.names = TRUE)

if (length(shp_path) > 0) {
        # REVIEW: make sure the polygons are in the same CRS as the points
        shp_data <- st_transform(st_read(shp_path[1], quiet = TRUE), 3763)

        # Records without INE but with coordinates
        fires_na <- fogos %>% filter(is.na(INE) & !is.na(x_3763) & !is.na(y_3763))

        if (nrow(fires_na) > 0) {
                fires_na_geo <- st_as_sf(fires_na, coords = c("x_3763", "y_3763"), crs = 3763)

                # Nearest parish polygon (the containing one when the point is inside)
                nearest_idx      <- st_nearest_feature(fires_na_geo, shp_data)
                fires_na$INE_new <- shp_data$Dicofre[nearest_idx]

                fogos <- fogos %>%
                        left_join(fires_na %>% dplyr::select(Codigo, INE_new), by = "Codigo") %>%
                        mutate(
                                # REVIEW: avoid "NA ; ..." when Observacoes is empty
                                Observacoes = ifelse(is.na(INE) & !is.na(INE_new),
                                                     paste0(coalesce(Observacoes, ""), "; ine updated via CAOP"),
                                                     Observacoes),
                                INE = coalesce(as.character(INE), as.character(INE_new))
                        ) %>%
                        dplyr::select(-INE_new)

                cat("✔ INE updated for", nrow(fires_na), "records.\n")
        }
} else {
        warning("CAOP2012 shapefile not found: INE codes not recovered.")
}

# =========================================
# 10. ADMINISTRATIVE JOIN
# =========================================
cat(">>> Merging administrative relations...\n")

if (!is.null(AdministrativeRelations)) {
        # REVIEW: one row per INE, otherwise the join duplicates fire records
        AdminRel <- AdministrativeRelations %>%
                mutate(INE = as.numeric(INE)) %>%
                distinct(INE, .keep_all = TRUE)

        n_before <- nrow(fogos)
        fogos <- fogos %>%
                mutate(INE = as.numeric(INE)) %>%
                left_join(AdminRel, by = "INE", suffix = c("", ".ref")) %>%
                mutate(
                        Distrito  = coalesce(Distrito.ref,  Distrito),
                        Concelho  = coalesce(Concelho.ref,  Concelho),
                        Freguesia = coalesce(Freguesia.ref, Freguesia)
                ) %>%
                # REVIEW: remove the helper columns created by the join
                dplyr::select(-ends_with(".ref"))
        stopifnot(nrow(fogos) == n_before)
}

# =========================================
# 11. FINAL EXPORT
# =========================================
write.csv(fogos, file.path(output_dir, "fogos_consolidado.csv"),
          row.names = FALSE, na = "", fileEncoding = "UTF-8")

cat("✔ Process completed successfully!\n")
cat("-> File saved at:", file.path(output_dir, "fogos_consolidado.csv"), "\n")


# =========================================
# 12. (OPTIONAL) LOAD THE FINAL PIPELINE OUTPUTS
# =========================================
# REVIEW: this block loads the FINAL processed database. It replaces `fogos`,
# so it must not run before scripts 3-4 (which expect the consolidated table).
if (load_final_outputs) {
        fogos <- st_read(file.path(output_dir, "fogos.gpkg"))

        cols_to_remove <- c(
                "EstadoRegisto", "Cod_SGIF", "ClassificacaoRegisto",
                "OriginouReacendimento", "Reacendimento", "Reacendimentos",
                "DistIncSimul500090", "DistIncSimul5000", "SCORE_ESTIMADO",
                "METODO_ESTIMATIVA", "Effis_Alta", "Effis_Baixa", "Effis_Média",
                "Effis_Muito_Alta", "Effis_Muito_Baixa", "Modis_Alta",
                "Modis_Baixa", "Modis_Média", "Modis_Muito_Alta",
                "Modis_Muito_Baixa", "Total_Ha_Effis", "Total_Ha_Modis"
        )
        fogos <- fogos %>% dplyr::select(-any_of(cols_to_remove))

        BurnedAreasClosed <- st_read(file.path(output_dir, "BurnedAreasClosed.gpkg"))
        FRP15             <- read_and_clean("FRP15.csv")
}
