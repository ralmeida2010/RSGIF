# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 11 - former 28-Severity_Fogos_Association.R
# =============================================================================
# 28-Severity_Fogos_Association.R  --  Burn severity classes per fire
#   Merges 28-EFFis_Severity_Fogos_Association.R, 29-Modis_Severity_...R and
#   31-Sentinel_Severity_...R (31 was the newer version of the EFFIS step)
# =============================================================================
# SUMMARY (English)
#   For every fire linked to a PBA perimeter, extracts the burn severity rasters
#   inside the perimeter and stores, per severity class, the area (ha) and the
#   share of the area (%):
#   A. EFFIS (Sentinel-2), 2018 onwards: one raster per year, classes 1-5
#      (very low to very high), used as delivered.
#        columns Effis_Muito_Baixa ... Effis_Muito_Alta, Total_Ha_Effis, *_perc
#   B. MODIS (SeverusPT, https://zenodo.org/records/10636820), 2006 onwards: three
#      rasters per year (January-June, July-September, October-December); each
#      fire uses the raster of the period of its END date; the continuous index
#      is reclassified into 5 classes (< 100, 100-270, 270-440, 440-660, >= 660).
#        columns Modis_Muito_Baixa ... Modis_Muito_Alta, Total_Ha_Modis, *_perc
#   C. Portuguese Burn Severity Atlas (PBSA; Jahanianfard et al., 2025,
#      https://doi.org/10.5281/zenodo.12773611), Landsat 30 m, 1984-2022, fires
#      of at least 100 ha: the longest severity series cited in the manuscript.
#      It replaces MODIS (block B, now off by default). The index (dNBR by
#      default; RdNBR, RBR or dNBR-EVI possible) is read from the GeoTIFFs of the
#      atlas, the files of each year are combined into one virtual mosaic, and
#      the values are classified with the dNBR thresholds of Key and Benson (2006)
#      (< 0.10, 0.10-0.27, 0.27-0.44, 0.44-0.66, >= 0.66; values scaled by 1000
#      are detected and rescaled).
#        columns PBSA_Muito_Baixa ... PBSA_Muito_Alta, Total_Ha_PBSA, *_perc
#      The atlas can be a folder of GeoTIFFs or the archive downloaded from
#      Zenodo (.zip, .rar or .7z): for each year ONLY that year's files of the
#      chosen index are extracted to a temporary folder, used, and deleted
#      (.zip with R; .rar / .7z with 7-Zip, https://www.7-zip.org).
#   A pixel is counted when its centre falls inside the perimeter, so fires
#   smaller than a pixel may get no value.
#
#   NOTE FOR THE MANUSCRIPT: Total_Ha_Effis, Total_Ha_PBSA and Total_Ha_Modis are the areas of
#   severity pixels INSIDE the PBA perimeter. They exist only for fires with a
#   perimeter and are not independent burned areas of the event; this matters
#   for QI_Area ("satellite area") and QI_MultiSource (Sect. 2.5, Appendix C).
#   The MODIS class 1 (< 100) includes unburned and unchanged pixels.
#
# REVIEW NOTES (changes marked "# REVIEW:")
#   - Only polygon geometries are processed (points or lines would give one
#     pixel and a meaningless area).
#   - The MODIS period uses the estimated end date (DHFimEstimado), so fires
#     whose end time was imputed are no longer excluded.
#   - EFFIS is no longer limited to 2018-2024 (28) and the national boundary is
#     no longer downloaded (31): the raster is cropped to the fires of the year.
#   - Previous columns of each product are removed before the join (no .x/.y).
#   - Output folders unified (output_dir); the GeoPackage is written by the
#     final export script, not here.
# =============================================================================

# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

suppressPackageStartupMessages({ library(sf); library(terra); library(dplyr); library(tidyr); library(lubridate) })

if (!exists("project_dir")) {
  .p <- c(Sys.getenv("RSGIF_DIR", getwd()), Sys.getenv("RSGIF_DIR", getwd()))
  project_dir <- .p[dir.exists(.p)][1]
}
effis_path <- file.path(project_dir, "Effis Severity")
modis_path <- file.path(project_dir, "Modis Severity")
# Atlas folder: PBSA/<year>/dNBR_<year>.tif, RdNBR_<year>.tif, RBR_<year>.tif, dNBR_EVI_<year>.tif,
# confidence_<year>.tif, <year>.shp, burn_severity_details_<year>.csv (or a .zip/.rar/.7z of it)
.pb <- c(file.path(project_dir, "PBSA"), file.path(Sys.getenv("RSGIF_DIR", getwd()), "PBSA"),
         file.path(Sys.getenv("RSGIF_DIR", getwd()), "PBSA"))
pbsa_path  <- .pb[dir.exists(.pb)][1]
pbsa_use_confidence <- FALSE   # TRUE: ignore pixels with confidence_<year>.tif below pbsa_min_confidence
pbsa_min_confidence <- 1
pbsa_archive <- NULL   # NULL = first .zip/.rar/.7z in pbsa_path (used only if there are no loose GeoTIFFs)
path_7z    <- c("C:/Program Files/7-Zip/7z.exe", "C:/Program Files (x86)/7-Zip/7z.exe", Sys.which("7z"))
run_effis  <- TRUE
run_modis  <- FALSE   # replaced by the longer Landsat series (block C)
run_pbsa   <- TRUE

pbsa_index  <- "dNBR"                         # "dNBR", "RdNBR", "RBR" or "dNBR_EVI"
pbsa_breaks <- c(0.10, 0.27, 0.44, 0.66)      # Key and Benson (2006), unscaled index
pbsa_labels <- c("1" = "PBSA_Muito_Baixa", "2" = "PBSA_Baixa", "3" = "PBSA_Média",
                 "4" = "PBSA_Alta", "5" = "PBSA_Muito_Alta")

effis_labels <- c("1" = "Effis_Muito_Baixa", "2" = "Effis_Baixa", "3" = "Effis_Média",
                  "4" = "Effis_Alta", "5" = "Effis_Muito_Alta")
modis_labels <- c("1" = "Modis_Muito_Baixa", "2" = "Modis_Baixa", "3" = "Modis_Média",
                  "4" = "Modis_Alta", "5" = "Modis_Muito_Alta")
modis_rcl <- matrix(c(-Inf, 100, 1,  100, 270, 2,  270, 440, 3,  440, 660, 4,  660, Inf, 5),
                    ncol = 3, byrow = TRUE)

# -----------------------------------------------------------------------------
# 0. Helpers
# -----------------------------------------------------------------------------
# Fires with a polygon (linked PBA perimeter)
fire_polygons <- function(f) {
  g <- if (inherits(f, "sf")) f else st_as_sf(f, crs = 3763)
  g <- g[!st_is_empty(g), ]
  g <- g[st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON"), ]          # REVIEW
  g$Codigo <- as.character(g$Codigo)
  g
}

# Area (ha) per class inside each polygon, for one raster
extract_classes <- function(polys, raster_file, labels, rcl = NULL) {
  r <- rast(raster_file)
  v <- vect(polys)
  if (crs(v) != crs(r)) v <- project(v, crs(r))
  r <- crop(r, ext(v) + 1000)
  if (!is.null(rcl)) r <- classify(r, rcl)
  px_ha <- prod(res(r)) / 10000
  ex <- terra::extract(r, v, ID = TRUE)
  if (!nrow(ex)) return(NULL)
  names(ex)[2] <- "Severity"
  ex$Codigo <- polys$Codigo[ex$ID]
  out <- ex %>% filter(!is.na(Severity), as.character(Severity) %in% names(labels)) %>%
    count(Codigo, Severity) %>%
    mutate(ha = n * px_ha, cls = labels[as.character(Severity)]) %>%
    select(Codigo, cls, ha) %>%
    pivot_wider(names_from = cls, values_from = ha, values_fill = 0)
  rm(r, v, ex); tmpFiles(remove = TRUE)
  out
}

# Totals, percentages, and join to fogos
add_product <- function(fogos, tab, labels, total_name) {
  cols <- unname(labels)
  old  <- c(cols, paste0(cols, "_perc"), total_name)
  fogos <- fogos %>% select(-any_of(old))                                   # REVIEW
  if (is.null(tab) || !nrow(tab)) return(fogos)
  for (cl in cols) if (!cl %in% names(tab)) tab[[cl]] <- 0
  tab <- tab %>%
    group_by(Codigo) %>% summarise(across(all_of(cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(!!total_name := rowSums(across(all_of(cols)))) %>%
    mutate(across(all_of(cols), ~ ifelse(.data[[total_name]] > 0, round(100 * .x / .data[[total_name]], 2), 0),
                  .names = "{.col}_perc"))
  fogos %>% mutate(Codigo = as.character(Codigo)) %>% left_join(tab, by = "Codigo")
}

polys_all <- fire_polygons(fogos)
message("Fires with a perimeter: ", nrow(polys_all))

# -----------------------------------------------------------------------------
# A. EFFIS (Sentinel-2)
# -----------------------------------------------------------------------------
if (run_effis) {
  tifs <- list.files(effis_path, pattern = "\\.tif{1,2}$", full.names = TRUE)
  res_e <- list()
  for (yr in sort(unique(polys_all$Ano[polys_all$Ano >= 2018]))) {
    f <- tifs[grepl(as.character(yr), basename(tifs))][1]
    if (is.na(f)) { message("EFFIS ", yr, ": raster not found"); next }
    p <- polys_all[polys_all$Ano == yr, ]
    message("EFFIS ", yr, ": ", nrow(p), " fires")
    res_e[[as.character(yr)]] <- extract_classes(p, f, effis_labels)
  }
  fogos <- add_product(fogos, bind_rows(res_e), effis_labels, "Total_Ha_Effis")
  message("EFFIS severity linked to ", sum(fogos$Total_Ha_Effis > 0, na.rm = TRUE), " fires")
}

# -----------------------------------------------------------------------------
# B. MODIS (SeverusPT)
# -----------------------------------------------------------------------------
if (run_modis) {
  tifs <- list.files(modis_path, pattern = "\\.tif$", full.names = TRUE)
  # REVIEW: period from the estimated end date
  end_date <- if ("DHFimEstimado" %in% names(polys_all)) coalesce(as.Date(polys_all$DHFimEstimado),
                                                                  as.Date(polys_all$DHFim)) else as.Date(polys_all$DHFim)
  polys_all$Mes_Fim <- month(end_date)
  periods <- list(list(patt = "P006", months = 1:6), list(patt = "P009", months = 7:9),
                  list(patt = "P12",  months = 10:12))
  res_m <- list()
  for (yr in sort(unique(polys_all$Ano[polys_all$Ano >= 2006]))) {
    tifs_y <- tifs[grepl(paste0("E", yr), basename(tifs))]
    if (!length(tifs_y)) { message("MODIS ", yr, ": rasters not found"); next }
    for (pr in periods) {
      p <- polys_all[polys_all$Ano == yr & polys_all$Mes_Fim %in% pr$months, ]
      if (!nrow(p)) next
      f <- tifs_y[grepl(pr$patt, basename(tifs_y))][1]
      if (is.na(f)) f <- tifs_y[grepl("P12|P012", basename(tifs_y))][1]
      if (is.na(f)) next
      message("MODIS ", yr, " ", pr$patt, ": ", nrow(p), " fires")
      res_m[[paste(yr, pr$patt)]] <- extract_classes(p, f, modis_labels, modis_rcl)
    }
  }
  fogos <- add_product(fogos, bind_rows(res_m), modis_labels, "Total_Ha_Modis")
  message("MODIS severity linked to ", sum(fogos$Total_Ha_Modis > 0, na.rm = TRUE), " fires")
}

# -----------------------------------------------------------------------------
# C. Portuguese Burn Severity Atlas (Landsat, 1984-2022), one year at a time
# -----------------------------------------------------------------------------
# --- archive helpers: list and extract (.zip with R, .rar / .7z with 7-Zip) ---------
find_7z <- function() { p <- path_7z[nzchar(path_7z) & file.exists(path_7z)]
if (!length(p)) stop("7-Zip not found (needed for .rar / .7z). Install it from https://www.7-zip.org ",
                     "or set path_7z.", call. = FALSE); p[1] }
arch_list <- function(arch) {
  if (grepl("\\.zip$", arch, ignore.case = TRUE)) return(utils::unzip(arch, list = TRUE)$Name)
  out  <- system2(find_7z(), c("l", "-slt", shQuote(arch)), stdout = TRUE, stderr = TRUE)
  k <- which(trimws(out) == "----------")[1]               # entries start after this line
  if (!is.na(k)) out <- out[(k + 1):length(out)]
  path <- sub("^Path = ", "", grep("^Path = ", out, value = TRUE))
  fold <- sub("^Folder = ", "", grep("^Folder = ", out, value = TRUE))
  if (length(fold) == length(path)) path <- path[fold != "+"]      # drop folders
  path
}
arch_extract <- function(arch, files, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  if (grepl("\\.zip$", arch, ignore.case = TRUE)) {
    utils::unzip(arch, files = files, exdir = exdir)       # keeps the folders (no name clashes)
  } else {
    lst <- tempfile(fileext = ".txt"); writeLines(files, lst, useBytes = TRUE)
    st <- system2(find_7z(), c("x", shQuote(arch), paste0("-o", shQuote(exdir)), "-y", "-scsUTF-8",
                               paste0("@", shQuote(lst))), stdout = FALSE, stderr = FALSE)
    unlink(lst)
    if (!identical(st, 0L) && !identical(st, 0)) warning("7-Zip returned code ", st, " for ", basename(arch))
  }
  list.files(exdir, pattern = "\\.tif{1,2}$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
}
pick_index <- function(f) {
  u <- toupper(basename(f))
  switch(toupper(pbsa_index),
         "DNBR"     = grepl("(^|[^R])DNBR", u) & !grepl("EVI", u),
         "RDNBR"    = grepl("RDNBR", u),
         "RBR"      = grepl("RBR", u),
         "DNBR_EVI" = grepl("DNBR", u) & grepl("EVI", u),
         stop("pbsa_index must be dNBR, RdNBR, RBR or dNBR_EVI"))
}
year_of <- function(f) {                       # year in the file name, otherwise in the folders
  y <- regmatches(basename(f), regexpr("(198[4-9]|199[0-9]|20[0-2][0-9])", basename(f)))
  if (!length(y)) y <- regmatches(dirname(f), regexpr("(198[4-9]|199[0-9]|20[0-2][0-9])", dirname(f)))
  if (length(y)) as.integer(y[1]) else NA_integer_
}

if (run_pbsa) {
  # 1. index of the files: loose GeoTIFFs in pbsa_path, otherwise inside the archive
  loose <- list.files(pbsa_path, pattern = "\\.tif{1,2}$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  loose <- loose[!grepl("/_tmp_pbsa", gsub("\\\\", "/", loose))]
  if (length(loose)) {
    src_type <- "folder"; files <- loose
  } else {
    arch <- if (!is.null(pbsa_archive)) pbsa_archive else
      list.files(pbsa_path, pattern = "\\.(zip|rar|7z)$", full.names = TRUE, ignore.case = TRUE)[1]
    if (is.na(arch) || !file.exists(arch)) stop("No GeoTIFFs and no .zip/.rar/.7z found in ", pbsa_path, call. = FALSE)
    src_type <- "archive"; message("PBSA: reading the list of files in ", basename(arch), " ...")
    files <- arch_list(arch)
    files <- files[grepl("\\.tif{1,2}$", files, ignore.case = TRUE)]
  }
  files <- files[pick_index(files)]
  f_year <- vapply(files, year_of, integer(1), USE.NAMES = FALSE)
  message("PBSA (", src_type, "): ", length(files), " ", pbsa_index, " files, years ",
          paste(range(f_year, na.rm = TRUE), collapse = "-"),
          if (any(is.na(f_year))) paste0(" (", sum(is.na(f_year)), " without year, ignored)") else "")
  if (!length(files)) stop("No ", pbsa_index, " GeoTIFF found in the PBSA atlas", call. = FALSE)
  
  miss <- setdiff(1984:2022, f_year)
  if (length(miss)) message("PBSA: years without ", pbsa_index, " file: ", paste(miss, collapse = ", "))
  # 2. one year at a time: extract (if archive), mosaic, classify, extract, delete
  tmp_root <- file.path(pbsa_path, "_tmp_pbsa")
  res_p <- list()
  for (yr in intersect(sort(unique(polys_all$Ano)), sort(unique(f_year)))) {
    p <- polys_all[polys_all$Ano == yr, ]
    fy <- files[f_year %in% yr]
    if (!nrow(p) || !length(fy)) next
    tmp_dir <- file.path(tmp_root, yr)
    tifs <- if (src_type == "archive") arch_extract(arch, fy, tmp_dir) else fy
    if (!length(tifs)) { message("PBSA ", yr, ": extraction failed"); unlink(tmp_dir, recursive = TRUE); next }
    src <- if (length(tifs) == 1) tifs else { v <- file.path(tmp_root, paste0("mosaic_", yr, ".vrt"))
    dir.create(tmp_root, showWarnings = FALSE); vrt(tifs, filename = v, overwrite = TRUE); v }
    # optional: mask pixels of low confidence (confidence_<year>.tif in the same folder)
    if (pbsa_use_confidence) {
      fc <- list.files(unique(dirname(tifs)), pattern = paste0("^confidence.*", yr, ".*\\.tif$"),
                       full.names = TRUE, ignore.case = TRUE)[1]
      if (!is.na(fc)) {
        vv <- vect(p); r0 <- rast(src); if (crs(vv) != crs(r0)) vv <- project(vv, crs(r0))
        e  <- ext(vv) + 1000
        r0 <- crop(r0, e); cf <- crop(rast(fc), e)
        r0 <- mask(r0, cf < pbsa_min_confidence, maskvalues = TRUE)
        src <- file.path(tmp_root, paste0("masked_", yr, ".tif")); dir.create(tmp_root, showWarnings = FALSE)
        writeRaster(r0, src, overwrite = TRUE); rm(r0, cf, vv)
        message("PBSA ", yr, ": pixels with confidence < ", pbsa_min_confidence, " ignored")
      } else message("PBSA ", yr, ": confidence raster not found (no mask)")
    }
    rs  <- rast(src)
    smp <- suppressWarnings(spatSample(rs, min(20000, ncell(rs)), na.rm = TRUE, warn = FALSE)[[1]])
    q   <- if (length(smp)) quantile(abs(smp), 0.99, na.rm = TRUE) else NA
    scale <- if (is.finite(q) && q > 5) 1000 else 1          # index stored x 1000
    rm(rs); invisible(gc())
    rcl <- cbind(c(-Inf, pbsa_breaks), c(pbsa_breaks, Inf), 1:5); rcl[, 1:2] <- rcl[, 1:2] * scale
    message("PBSA ", yr, ": ", nrow(p), " fires, ", length(tifs), " file(s), scale ", scale)
    res_p[[as.character(yr)]] <- tryCatch(extract_classes(p, src, pbsa_labels, rcl),
                                          error = function(e) { message("   error: ", conditionMessage(e)); NULL })
    # free disk space: delete this year's extracted files and mosaic
    invisible(gc()); terra::tmpFiles(remove = TRUE)
    if (src_type == "archive") unlink(tmp_dir, recursive = TRUE)
    if (grepl("(\\.vrt$)|(masked_[0-9]{4}\\.tif$)", src)) unlink(src)
  }
  unlink(tmp_root, recursive = TRUE)
  
  tab_p <- bind_rows(res_p)
  if (nrow(tab_p)) {
    fogos <- add_product(fogos, tab_p, pbsa_labels, "Total_Ha_PBSA")
    message("PBSA severity linked to ", sum(fogos$Total_Ha_PBSA > 0, na.rm = TRUE), " fires")
    write.csv(tab_p, file.path(if (exists("output_dir")) output_dir else file.path(project_dir, "output"),
                               "pbsa_severidade_por_fogo.csv"), row.names = FALSE)
  } else warning("No PBSA value was extracted.")
}

rm(polys_all, fire_polygons, extract_classes, add_product)