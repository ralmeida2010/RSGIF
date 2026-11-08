# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# 16b_Variable_inventory.R  --  Inventory of the PRFD variables: original,
#   estimated, new (by information category i-viii), quality, and auxiliary
# =============================================================================
# SUMMARY (English)
#   Classifies every column of the final tables and counts them, to report in
#   the manuscript the number of new variables, the number of information
#   categories they cover (framework of Camia et al. 2014 / Pourmohamad et al.
#   2024), and the number of auxiliary variables.
#   Roles:
#     original    field of the SGIF records (basic occurrence, cause, area, times)
#     estimated   corrected / imputed version of an original field
#                 (AreaTotalEstimada, DHFimEstimado, DuracaoHorasEstimado, HaHoraEstimado)
#     new         variable added by the pipeline (Step 5), by category i-viii
#     quality     quality indicators and tiers (Step 6)
#     auxiliary   technical variables: flags, rules, mechanisms, composite dates,
#                 link details, percentages per class, etc. All columns of
#                 fogos_anexo are auxiliary.
#   Inputs (in memory or read from output/): fogos (or output/fogos.gpkg /
#   fogos.csv), fogos_anexo (or output/fogos_anexo.csv), audit_table_prfd
#   (optional, quality columns). The headers of the raw SGIF exports
#   (data/processed/Data1980_2000.csv, Data2001_now.csv) are read, without the
#   data, to show which "new" variables were already in the SQL Server export.
#   Outputs (output/inventory/): variable_inventory.csv (one row per column,
#   with role and category: review it), inventory_summary.csv, and the sentence
#   for the manuscript.
# =============================================================================

select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate
summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count
suppressPackageStartupMessages({ library(dplyr); library(data.table) })

if (!exists("output_dir")) output_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "output")
if (!exists("data_dir"))   data_dir   <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "data")
inv_dir <- file.path(output_dir, "inventory"); dir.create(inv_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Column names of the tables (only the header is read from disk)
# -----------------------------------------------------------------------------
header_of <- function(obj, files) {
        if (exists(obj, envir = globalenv())) {
                x <- get(obj, envir = globalenv())
                return(setdiff(names(x), attr(x, "sf_column")))
        }
        for (f in files) if (file.exists(f)) {
                if (grepl("\\.gpkg$", f)) return(setdiff(names(sf::st_read(f, query = "SELECT * FROM fogos LIMIT 0", quiet = TRUE)), "geom"))
                return(names(fread(f, nrows = 0)))
        }
        character(0)
}
cols_main  <- header_of("fogos", file.path(output_dir, c("fogos.csv", "fogos.gpkg")))
cols_annex <- header_of("fogos_anexo", file.path(output_dir, "fogos_anexo.csv"))
cols_audit <- header_of("audit_table_prfd", file.path(output_dir, "audit_table_prfd.csv"))
raw_hdr    <- unique(unlist(lapply(file.path(data_dir, "processed", c("Data1980_2000.csv", "Data2001_now.csv")),
                                   function(f) if (file.exists(f)) names(fread(f, nrows = 0)))))
has_geom   <- exists("fogos") && inherits(fogos, "sf")
message("Columns: main ", length(cols_main), " | annex ", length(cols_annex),
        " | audit ", length(cols_audit), " | raw SGIF export ", length(raw_hdr))

# -----------------------------------------------------------------------------
# 2. Classification rules (first rule that matches the name; edit if needed)
# -----------------------------------------------------------------------------
rules <- tribble(
  ~pattern,                                                                       ~role,       ~category,
  # quality (Step 6)
  "^(QI_|PRFD_Overall_Quality_Tier|Tier|Match_Low_Confidence|G[0-9]_|Has_Geometry)", "quality",   "quality",
  # auxiliary / technical
  "^(Flag_|Imp_Mechanism|DHFim_Rules|DHFimRegras|TipoEstimado|Outside_PT|MODIS_|ID$|ROW_ID)", "auxiliary", "auxiliary",
  "_(perc|idade|data)$|^nac_|_p0?5$|_p50$|_p95$|^N_Imputacoes$|^Funcao$",            "auxiliary", "auxiliary",
  "^(BACodAssociacao|BACodEstimado|BAMetodoManual|Cod_SGIF)$",                       "auxiliary", "auxiliary",
  # estimated versions of original fields
  "^(AreaTotalEstimada|AreaTotalEstimado|DHFimEstimado|DuracaoHorasEstimado|HaHoraEstimado)$", "estimated", "(iii) magnitude / (i) occurrence",
  # original SGIF fields
  "^(Codigo|CodigoSado|Ano|Mes|Dia|Hora|Tipo|TipoIncendio|DHInicio|DH1Intervencao|DHResolucao|DHConclusao|DHFim|DuracaoHoras|Tempo1Intervencao|TempoResolucao|TempoRescaldo)$", "original", "(i) occurrence",
  "^(NUTS2|NUTS3|Distrito|Concelho|Freguesia|Freguesia2014|Local|INE|DDCCFF2014|QO|x_20790|y_20790|FonteAlerta)$", "original", "(i) occurrence",
  "^(CodCausa|TipoCausa|GrupoCausa|DescricaoCausa)$",                                "original", "(ii) causes",
  "^(AreaPov|AreaMato|AreaAgric|AreaTotal|HaHora|ClasseArea)$",                       "original", "(iii) magnitude",
  "^(Reacendimento|Reacendimento_IncendioPai|OriginouReacendimento|Reacendimentos|EstadoRegisto|ClassificacaoRegisto|Observacoes)$", "original", "(i) occurrence",
  # new variables by category
  "^(x_3763|y_3763|Lat_4326|Lon_4326)$",                                              "new", "(i) occurrence",
  "^(BAMetodo|BAScore|Perimetro|APS|geom)$",                                          "new", "(iii) magnitude",
  "^(FRE|FRE_GJ|Num_Pontos_FRP|IDPT-FireSprd|Total_Ha_|Effis_|PBSA_|Modis_)",         "new", "(iv) behaviour and effects",
  "^(Temperatura|HumidadeRelativa|Vento|Precepitacao|fwi|dsr|isi|dc|dmc|ffmc|bui|hFWI|hFFMC|hISI|Max.*_48h|RCM)", "new", "(v) atmospheric",
  "^(AltitudeMedia|DecliveMedio|HorasExposicaoMedia|Rugosidade|CosN5Variedade|ModFarsite|AreaManchaModFarsite|Perigosidade|gpp_|fapar|lai|ndvi|evi|ndii7|rg)$|^gpp_", "new", "(vi) landscape and ecology",
  "^(DensidadeResidentes|DensidadeEdificios|DendidadeRV)$",                            "new", "(vii) socio-economic",
  "^(Dist_CBS_m|CBS|NIncSimul|AreaTotalIncSimul|DistIncSimul)",                        "new", "(viii) suppression"
)

classify <- function(v) {
        for (k in seq_len(nrow(rules))) if (grepl(rules$pattern[k], v)) return(c(rules$role[k], rules$category[k]))
        c("UNCLASSIFIED", "UNCLASSIFIED")
}

inv <- bind_rows(
        tibble(variable = cols_main,  table = "fogos (main)"),
        if (has_geom) tibble(variable = "geom", table = "fogos (main)"),
        tibble(variable = setdiff(cols_annex, "Codigo"), table = "fogos_anexo"),
        tibble(variable = setdiff(cols_audit, c(cols_main, cols_annex)), table = "audit_table_prfd")) %>%
        distinct(variable, .keep_all = TRUE)
cl <- t(vapply(inv$variable, classify, character(2)))
inv$role <- cl[, 1]; inv$category <- cl[, 2]
inv$role[inv$table == "fogos_anexo"] <- "auxiliary"            # everything in the annex is auxiliary
inv$category[inv$table == "fogos_anexo"] <- "auxiliary"
inv$in_raw_SGIF_export <- inv$variable %in% raw_hdr

# -----------------------------------------------------------------------------
# 3. Counts
# -----------------------------------------------------------------------------
by_role <- inv %>% count(role, name = "n")
by_cat  <- inv %>% filter(role == "new") %>% count(category, name = "n_new_variables")
cats8   <- c("(i) occurrence", "(ii) causes", "(iii) magnitude", "(iv) behaviour and effects",
             "(v) atmospheric", "(vi) landscape and ecology", "(vii) socio-economic", "(viii) suppression")
covered <- cats8[cats8 %in% c(by_cat$category,
                              inv$category[inv$role %in% c("original", "estimated")] %>% strsplit(" / ") %>% unlist())]
n_new  <- sum(inv$role == "new"); n_est <- sum(inv$role == "estimated")
n_aux  <- sum(inv$role == "auxiliary"); n_q <- sum(inv$role == "quality")
n_new_cats <- length(unique(by_cat$category))

cat("\n===== Variables by role =====\n"); print(by_role)
cat("\n===== New variables by information category =====\n"); print(by_cat)
cat("\nCategories covered (original + new):", length(covered), "of 8\n")
if (any(inv$role == "UNCLASSIFIED")) {
        cat("\n!! Unclassified columns (add a rule or classify by hand):\n")
        print(inv$variable[inv$role == "UNCLASSIFIED"])
}
pre <- inv$variable[inv$role == "new" & inv$in_raw_SGIF_export]
if (length(pre)) cat("\nNote:", length(pre), "'new' variables already come in the SQL Server export",
                     "(computed in the SGIF database, not in R):\n ", paste(pre, collapse = ", "), "\n")

summary_tab <- tibble(
        item  = c("original fields", "estimated versions of original fields", "new variables",
                  "categories with new variables", "categories covered (of 8)", "quality indicators", "auxiliary variables"),
        value = c(sum(inv$role == "original"), n_est, n_new, n_new_cats, length(covered), n_q, n_aux))
fwrite(inv, file.path(inv_dir, "variable_inventory.csv"))
fwrite(summary_tab, file.path(inv_dir, "inventory_summary.csv"))
fwrite(by_cat, file.path(inv_dir, "new_variables_by_category.csv"))

cat("\n===== Sentence for the manuscript =====\n")
cat(sprintf(paste0("In total, %d new variables were added, covering %d of the eight information categories ",
                   "(%d covered in total with the original fields), together with %d estimated versions of ",
                   "original fields, %d quality indicators, and %d auxiliary variables.\n"),
            n_new, n_new_cats, length(covered), n_est, n_q, n_aux))
message("Inventory written to ", inv_dir, " (review variable_inventory.csv)")
