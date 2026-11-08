# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 12 - former 30-FRE_Fogos_Association.R
# =============================================================================
# 30-FRE_Fogos_Association.R  --  Fire Radiative Energy (FRE) per fire
# =============================================================================
# SUMMARY (English)
#   Associates SEVIRI/Meteosat Fire Radiative Power hotspots (FRP15, 15-min
#   slots, LSA SAF; script 25) with the PRFD fires and integrates them in time:
#   Phase 1 "Poligono"  fires with a PBA perimeter: hotspots acquired between
#                       4 h before the start and 24 h after the end, inside a
#                       buffer of the perimeter of 5 km + 400 m x sqrt(area ha),
#                       limited to 5-40 km.
#   Phase 2 "Ponto"     fires without perimeter, >= 0.5 ha: hotspots between 4 h
#                       before the start and 28 h after the end, within
#                       5 km + 400 m x sqrt(area), limited to 5-45 km of the
#                       ignition point, taken from the nearest, while the
#                       accumulated FRE stays below the value expected for the
#                       fire area (linear FRE ~ area model fitted on phase 1).
#   A hotspot is assigned to one fire only (the first fire processed that
#   reaches it). Outputs:
#     FRE            sum of the FRP of the assigned hotspots (MW, one value per
#                    15-min slot), as before;
#     FRE_GJ         energy in GJ = 0.9 x FRE (MW x 900 s = 0.9 GJ per slot),
#                    the unit of Sect. 2.3 and 3.2.1;
#     Num_Pontos_FRP number of hotspots; Funcao  phase ("Poligono", "Ponto",
#                    "Nenhum");
#     FRP15_sf_final all hotspots with the fire they were assigned to.
#
#   NOTE FOR THE MANUSCRIPT (Sect. 2.3 and Appendix C4): the hotspots are taken
#   from a buffer of 5-40 km around the perimeter, not only inside it; phase 2
#   (fires without perimeter) is not described; the FRP confirmation used by
#   QI_MultiSource follows these windows (5-45 km, -4 h / +28 h), not "2 km".
#
# REVIEW NOTES (changes marked "# REVIEW:")
#   - The time windows used the ORIGINAL end time (DHFim): fires whose end
#     time was missing got no hotspot. They now use the estimated end time of
#     script 4 (DHFimEstimado, or DHFim when missing).
#   - FRE_GJ added (the manuscript reports GJ; FRE is a sum of MW).
#   - process_order: "as_is" (row order, as before) or "largest_first". With
#     exclusive assignment the result depends on the order; "largest_first"
#     avoids small nearby fires taking the hotspots of a large fire.
#   - The per-hotspot diagnostic print (one line per candidate) is optional.
#   - CAOP path built from data_dir.
# =============================================================================

# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

library(sf)
library(dplyr)
library(lubridate)

if (!exists("data_dir")) data_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "data")
process_order   <- "as_is"   # REVIEW: "as_is" (as before) or "largest_first"
run_point_phase <- TRUE      # phase 2 (fires without perimeter)
verbose_points  <- FALSE     # REVIEW: per-hotspot diagnostic print
order_fires <- function(df) if (process_order == "largest_first") df[order(-df$AreaTotal), ] else df
# REVIEW: FRP15 from data/processed/FRP15.csv (downloaded by 02_ingest_csv.R) when not in memory
if (!exists("FRP15")) {
        f <- c(file.path(data_dir, "processed", "FRP15.csv"), file.path(data_dir, "FRP15.csv"))
        f <- f[file.exists(f)][1]
        if (is.na(f)) stop("FRP15 not found (data/processed/FRP15.csv)")
        FRP15 <- read.csv(f); message("FRP15 read from ", f)
}

# ==========================================================
# 1. PREPARAÇÃO GLOBAL
# ==========================================================

message("\n>>> 1. A iniciar a preparação global dos dados...")

# 1. Your original data processing step
# (Make sure 'longitude' and 'latitude' columns exist in your FRP15 data)
fileFRP15 <- FRP15 %>%
        mutate(
                ACQTIME = with_tz(make_datetime(ano, mes, dia, hora, minuto), tzone = "UTC"),
                ANO = year(ACQTIME)
        )

# 2. Convert your data frame into a spatial (sf) object 
# Change "LONGITUDE" and "LATITUDE" to match your exact column names!
frp_spatial <- st_as_sf(
        fileFRP15, 
        coords = c("lon", "lat"), 
        crs = 4326
)
# 3. Read the CAOP 2012 shapefile
# st_read automatically handles the .shp components inside the directory
caop <- st_read(list.files(file.path(data_dir, "../data/CAOP2012"), pattern = "\\.shp$", full.names = TRUE)[1])   # REVIEW

# 4. Ensure both datasets use the exact same Coordinate Reference System (CRS)
# This is crucial for accurate spatial alignment!
if (st_crs(frp_spatial) != st_crs(caop)) {
        frp_spatial <- st_transform(frp_spatial, st_crs(caop))
}

# 5. Cross the data and classify (inside / outside)
# st_intersects returns a list of matching polygon indices for each point.
# If a point doesn't touch any polygon, its list length is 0.
FRP15 <- fileFRP15 %>%
        mutate(
                intersects_list = st_intersects(frp_spatial, caop),
                portugal = if_else(lengths(intersects_list) > 0, "inside", "outside")
        ) %>%
        select(-intersects_list) # Clean up the temporary list column

# Converter FRP para sf e garantir Projeção PT (3763)
frp_sf_global <- st_as_sf(FRP15, coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(3763) %>%
        mutate(ID_FRP = row_number())

# Limpar colunas antigas para evitar conflitos de join
fogos <- fogos %>%
        mutate(
                DHInicio = with_tz(as.POSIXct(DHInicio), tzone = "UTC"),
                DHFim    = with_tz(as.POSIXct(DHFim), tzone = "UTC"),
                # REVIEW: end of the time window = estimated end time (script 4)
                FimJanela = with_tz(if ("DHFimEstimado" %in% names(.)) coalesce(as.POSIXct(DHFimEstimado), as.POSIXct(DHFim))
                                    else as.POSIXct(DHFim), tzone = "UTC")
        ) %>%
        select(-any_of(c("FRE", "FRE_GJ", "Num_Pontos_FRP", "Funcao")))

# Separar polígonos válidos para a 1ª Fase
fogos_poligonos <- fogos %>%
        filter(!is.na(geom), !st_is_empty(geom)) %>%
        st_as_sf(crs = 3763) %>% 
        st_make_valid()

# ==============================================================================
# 2. PROCESSAMENTO DE POLÍGONOS (1ª FASE - "Poligono")
# ==============================================================================

# 1. Encontra os anos em comum e ordena de forma crescente
anos_disponiveis <- sort(intersect(unique(fogos_poligonos$Ano), unique(frp_sf_global$ANO)))

# 2. Inicializa a lista vazia para os resultados
matches_poligonos_list <- list()

for(ano_ref in anos_disponiveis) {
        message(paste("\n>>> Polígonos - Processando Ano:", ano_ref))
        
        fogos_ano <- order_fires(fogos_poligonos %>% filter(Ano == ano_ref))
        frp_ano   <- frp_sf_global %>% filter(ANO == ano_ref)
        
        if(nrow(fogos_ano) == 0 || nrow(frp_ano) == 0) next
        
        # Guardar a quantidade de elementos na lista antes de iniciar o ano
        idx_inicio_ano <- length(matches_poligonos_list)
        
        pb <- txtProgressBar(min = 0, max = nrow(fogos_ano), style = 3)
        
        for(i in seq_len(nrow(fogos_ano))) {
                fogo_pol <- fogos_ano[i,]
                
                janela_inicio <- fogo_pol$DHInicio - hours(4)
                janela_fim    <- fogo_pol$FimJanela + hours(24)   # REVIEW
                
                cand_tempo <- frp_ano %>%
                        filter(ACQTIME >= janela_inicio & ACQTIME <= janela_fim)
                
                if(nrow(cand_tempo) > 0) {
                        # Calcular raio dinâmico: Base de 5km + fator multiplicador baseado na área
                        raio_dinamico <- 5000 + (400 * sqrt(fogo_pol$AreaTotal))
                        
                        # Trancar o raio entre o mínimo absoluto (5 km) e o máximo exigido (40 km)
                        raio_final <- max(5000, min(40000, raio_dinamico))
                        
                        # Aplicar o buffer geométrico com o raio calculado
                        pol_buffer_dinamico <- st_buffer(fogo_pol, raio_final)
                        
                        # 🛠️ CORREÇÃO AQUI: Extração geométrica correta usando o buffer dinâmico criado
                        frps_no_raio        <- cand_tempo[pol_buffer_dinamico, ]
                        
                        if(nrow(frps_no_raio) > 0) {
                                matches_poligonos_list[[length(matches_poligonos_list) + 1]] <- data.frame(
                                        Codigo   = fogo_pol$Codigo,
                                        FRP_val  = frps_no_raio$FRP,
                                        ID_FRP   = frps_no_raio$ID_FRP,
                                        Funcao   = "Poligono"
                                )
                                # Exclusividade em tempo real para este ano
                                frp_ano <- frp_ano %>% filter(!ID_FRP %in% frps_no_raio$ID_FRP)
                        }
                }
                setTxtProgressBar(pb, i)
        }
        close(pb)
        
        # ======================================================================
        # 📊 CÁLCULO E IMPRESSÃO DOS RESULTADOS DO ANO CORRENTE
        # ======================================================================
        idx_fim_ano <- length(matches_poligonos_list)
        
        if (idx_fim_ano > idx_inicio_ano) {
                # Agrupar apenas os dataframes que foram adicionados neste ano específico
                dados_ano_atual <- bind_rows(matches_poligonos_list[(idx_inicio_ano + 1):idx_fim_ano])
                
                n_fogos_associados <- length(unique(dados_ano_atual$Codigo))
                n_frp_associados   <- nrow(dados_ano_atual)
                
                cat(paste0("\n--------------------------------------------------",
                           "\n📊 BALANÇO PARCIAL (POLÍGONOS) - ANO: ", ano_ref,
                           "\n🔹 N.º de Fogos com dados associados: ", n_fogos_associados,
                           "\n🔹 N.º de registos FRP (SEVIRI) vinculados: ", n_frp_associados,
                           "\n--------------------------------------------------\n"))
        } else {
                cat(paste0("\n--------------------------------------------------",
                           "\n📊 BALANÇO PARCIAL (POLÍGONOS) - ANO: ", ano_ref,
                           "\n⚠️ Nenhuma associação conseguida para os polígonos deste ano.",
                           "\n--------------------------------------------------\n"))
        }
}

# Criar dataframe base dos polígonos
if(length(matches_poligonos_list) > 0) {
        tabela_pol_bruta <- bind_rows(matches_poligonos_list)
        message(paste("\n✅ Fase 1 Concluída! Total acumulado de registos associados por Polígono:", nrow(tabela_pol_bruta)))
} else {
        tabela_pol_bruta <- data.frame(Codigo=character(), FRP_val=numeric(), ID_FRP=numeric(), Funcao=character())
        message("\n⚠️ Fase 1 Concluída, mas a tabela final ficou vazia.")
}


# ==============================================================================
# 3. PROCESSAMENTO PARA PONTOS (2ª FASE - "Ponto")
# ==============================================================================

# Identificar fogos que ficaram a zeros na primeira fase
fogos_fase1_com_dados <- unique(tabela_pol_bruta$Codigo)

fogos_pontos <- fogos %>%
        filter(
                run_point_phase,
                !(Codigo %in% fogos_fase1_com_dados),
                !is.na(x_3763), !is.na(y_3763), !is.na(AreaTotal), 
                AreaTotal >= 0.5
        )

matches_pontos_list <- list()
falhas_pontos_list  <- list() # Inicialização explícita da lista de falhas para o funil

if(nrow(fogos_pontos) > 0) {
        
        # Calibração do modelo com base no que foi encontrado na Fase 1
        tabela_pol_fres <- tabela_pol_bruta %>%
                group_by(Codigo) %>%
                summarise(FRE = sum(FRP_val, na.rm = TRUE))
        
        dados_calibracao <- fogos_poligonos %>%
                inner_join(tabela_pol_fres, by = "Codigo") %>%
                st_drop_geometry() %>%
                filter(FRE > 0, AreaTotal > 0)
        
        if (nrow(dados_calibracao) >= 2) {
                modelo_fre <- lm(FRE ~ AreaTotal, data = dados_calibracao)
                b0 <- coef(modelo_fre)[1]
                b1 <- coef(modelo_fre)[2]
        } else {
                b0 <- 0; b1 <- 5.5 
        }
        
        # Correção preventiva caso os coeficientes retornem NA por problemas nos dados
        if(is.na(b0)) b0 <- 0
        if(is.na(b1)) b1 <- 5.5
        
        fogos_pontos_sf <- st_as_sf(fogos_pontos, coords = c("x_3763", "y_3763"), crs = 3763, remove = FALSE)
        anos_pontos <- intersect(unique(fogos_pontos_sf$Ano), unique(frp_sf_global$ANO))
        anos_pontos <- anos_pontos[!is.na(anos_pontos)]
        
        for(ano_ref in anos_pontos) {
                message(paste("\n>>> Pontos - Processando Ano Comum:", ano_ref))
                
                fogos_ano_pt <- order_fires(fogos_pontos_sf %>% filter(Ano == ano_ref))
                frp_ano <- frp_sf_global %>%
                        filter(
                                ANO == ano_ref, 
                                !ID_FRP %in% tabela_pol_bruta$ID_FRP,
                                portugal == "inside"
                        )
                
                
                # ─── ADICIONA ESTA CORREÇÃO LOGO A SEGUIR: ─────────────────────────────────
                # Garante que o frp_ano é um objeto sf válido e está rigidamente no CRS 3763
                if(!inherits(frp_ano, "sf")) {
                        frp_ano <- st_as_sf(frp_ano)
                }
                frp_ano <- st_transform(frp_ano, 3763)
                
                # Opcional: Remover geometrias vazias se existirem
                frp_ano <- frp_ano %>% filter(!st_is_empty(geometry))
                
                if(nrow(fogos_ano_pt) == 0 || nrow(frp_ano) == 0) {
                        message(paste("ℹ️ Sem dados suficientes para processar o ano:", ano_ref))
                        next
                }
                
                # Guardar o tamanho da lista antes de começar o ano para sabermos o que mudou
                idx_inicio_ano <- length(matches_pontos_list)
                
                pb <- txtProgressBar(min = 0, max = nrow(fogos_ano_pt), style = 3)
                
                for(i in seq_len(nrow(fogos_ano_pt))) {
                        fogo_pt <- fogos_ano_pt[i,]
                        
                        fre_max_permitido <- max(0, b0 + (b1 * fogo_pt$AreaTotal))
                        if(is.na(fre_max_permitido)) fre_max_permitido <- 0
                        
                        janela_inicio     <- fogo_pt$DHInicio - hours(4)
                        janela_fim        <- fogo_pt$FimJanela + hours(28)   # REVIEW
                        
                        # Filtro temporal + Remoção preventiva de NAs na coluna FRP
                        cand_tempo <- frp_ano %>%
                                filter(
                                        ACQTIME >= janela_inicio, 
                                        ACQTIME <= janela_fim,
                                        !is.na(FRP)
                                )
                        
                        # ─── PRINT DO FILTRO TEMPORAL ──────────────────────────────────────
                        #cat(paste0("\n🔥 Fogo: ", fogo_pt$Codigo, 
                        #           " | [Filtro Temporal]: ", nrow(cand_tempo), " candidatos encontrados na janela."))
                        # ──────────────────────────────────────────────────────────────────
                        
                        if(nrow(cand_tempo) > 0) {
                                raio_dinamico <- 5000 + (400 * sqrt(fogo_pt$AreaTotal))
                                raio_final    <- max(5000, min(45000, raio_dinamico))
                                
                                # Cálculo da distância linear exata (Euclidiana) entre as geometrias no CRS 3763
                                distancias <- st_distance(fogo_pt, cand_tempo)
                                
                                # Acoplar a distância temporariamente para podermos ordenar por ela
                                cand_tempo$dist_fogo <- as.numeric(distancias)
                                
                                # Filtragem dos pontos dentro do raio dinâmico estipulado
                                # Para esta versão mais segura:
                                frps_no_raio <- cand_tempo %>% filter(!is.na(dist_fogo) & dist_fogo <= raio_final)
                                
                                # ─── PRINT DO FILTRO ESPACIAL ──────────────────────────────────────
                                #cat(paste0(" -> [Filtro Espacial]: ", nrow(frps_no_raio), " pontos dentro do raio de ", round(raio_final, 0), "m."))
                                # ──────────────────────────────────────────────────────────────────
                                
                                if(nrow(frps_no_raio) > 0) {
                                        
                                        # ─── ALTERAÇÃO SOLICITADA: ORDENAÇÃO POR DISTÂNCIA E DEPOIS DATA ───
                                        frps_match <- frps_no_raio %>% arrange(dist_fogo, ACQTIME)
                                        # ──────────────────────────────────────────────────────────────────
                                        
                                        fre_acumulado <- 0
                                        pontos_validos_idx <- c()
                                        quebrou_por_energia <- FALSE
                                        
                                        for(j in seq_len(nrow(frps_match))) {
                                                proximo_frp <- frps_match$FRP[j]
                                                dist_ponto  <- frps_match$dist_fogo[j]
                                                
                                                # Salvaguarda para evitar avaliação lógica com valores nulos
                                                if(is.na(proximo_frp)) next
                                                
                                                # ─── IMPRESSÃO DE DIAGNÓSTICO EM TEMPO REAL ───────────────────────
                                                if (verbose_points) cat(paste0("\n   └─ Iteração: ", j, 
                                                           " | Dist: ", round(dist_ponto, 0), "m",
                                                           " | Próximo FRP: ", round(proximo_frp, 2), 
                                                           " | FRE Acumulada: ", round(fre_acumulado, 2), 
                                                           " | Limite Máx: ", round(fre_max_permitido, 2)))
                                                # ──────────────────────────────────────────────────────────────────
                                                
                                                if((fre_acumulado + proximo_frp) <= fre_max_permitido || length(pontos_validos_idx) == 0) {
                                                        fre_acumulado <- fre_acumulado + proximo_frp
                                                        pontos_validos_idx <- c(pontos_validos_idx, j)
                                                } else {
                                                        # Avisa na consola que bateu no teto de energia
                                                        if (verbose_points) cat(paste0(" -> 🛑 TETO EXCEDIDO! Novo acumulado seria ", 
                                                                   round(fre_acumulado + proximo_frp, 2), 
                                                                   " (Parando associação)."))
                                                        
                                                        quebrou_por_energia <- TRUE
                                                        break
                                                }
                                        }
                                        
                                        if(length(pontos_validos_idx) > 0) {
                                                frps_finais <- frps_match[pontos_validos_idx, ]
                                                
                                                matches_pontos_list[[length(matches_pontos_list) + 1]] <- data.frame(
                                                        Codigo   = fogo_pt$Codigo,
                                                        FRP_val  = frps_finais$FRP,
                                                        ID_FRP   = frps_finais$ID_FRP,
                                                        Funcao   = "Ponto"
                                                )
                                                # Remove os pontos atribuídos para não serem usados noutros fogos
                                                frp_ano <- frp_ano %>% filter(!ID_FRP %in% frps_finais$ID_FRP)
                                                
                                                # CORREÇÃO: Alimentar diretamente a lista global de falhas
                                                if(quebrou_por_energia) {
                                                        falhas_pontos_list[[length(falhas_pontos_list) + 1]] <- data.frame(Codigo = fogo_pt$Codigo, Ano = ano_ref, Razao = "Filtro de Energia")
                                                }
                                        } else {
                                                falhas_pontos_list[[length(falhas_pontos_list) + 1]] <- data.frame(Codigo = fogo_pt$Codigo, Ano = ano_ref, Razao = "Filtro de Energia")
                                        }
                                } else {
                                        falhas_pontos_list[[length(falhas_pontos_list) + 1]] <- data.frame(Codigo = fogo_pt$Codigo, Ano = ano_ref, Razao = "Filtro Espacial")
                                }
                        } else {
                                falhas_pontos_list[[length(falhas_pontos_list) + 1]] <- data.frame(Codigo = fogo_pt$Codigo, Ano = ano_ref, Razao = "Filtro Temporal")
                        }
                        
                        setTxtProgressBar(pb, i)
                }
                
                # ======================================================
                # 📊 EXIBIÇÃO DE METRICAS LOGO APÓS TERMINAR O ANO
                # ======================================================
                idx_fim_ano <- length(matches_pontos_list)
                
                # FIX: Compilar corretamente as falhas do ano de referência atual
                falhas_ano_atual <- list()
                if(length(falhas_pontos_list) > 0) {
                        falhas_ano_atual <- bind_rows(falhas_pontos_list) %>% 
                                filter(Ano == ano_ref)
                }
                
                # Contagem de exclusões por tipo de filtro do funil
                excluidos_tempo   <- if(nrow(falhas_ano_atual) > 0) sum(falhas_ano_atual$Razao == "Filtro Temporal") else 0
                excluidos_espaco  <- if(nrow(falhas_ano_atual) > 0) sum(falhas_ano_atual$Razao == "Filtro Espacial") else 0
                excluidos_energia <- if(nrow(falhas_ano_atual) > 0) sum(falhas_ano_atual$Razao == "Filtro de Energia") else 0
                excluidos_macro   <- if(nrow(falhas_ano_atual) > 0) sum(falhas_ano_atual$Razao == "Macro") else 0
                
                total_excluidos   <- excluidos_tempo + excluidos_espaco + excluidos_energia + excluidos_macro
                
                if (idx_fim_ano > idx_inicio_ano) {
                        dados_ano_atual <- bind_rows(matches_pontos_list[(idx_inicio_ano + 1):idx_fim_ano])
                        
                        n_fogos_associados <- length(unique(dados_ano_atual$Codigo))
                        n_frp_associados   <- nrow(dados_ano_atual)
                        
                        cat(paste0("\n--------------------------------------------------",
                                   "\n📊 BALANÇO PARCIAL DO ANO: ", ano_ref,
                                   "\n🔹 N.º de Fogos resgatados nesta fase: ", n_fogos_associados, " fogos",
                                   "\n🔹 N.º de pontos SEVIRI (FRP) vinculados: ", n_frp_associados, " observações",
                                   "\n",
                                   "\n❌ DIAGNÓSTICO DE NÃO ASSOCIAÇÃO (Funil de Perdas):",
                                   "\n 🛑 Retidos no [1. Filtro Temporal]: ", excluidos_tempo, " fogos",
                                   "\n 📐 Retidos no [2. Filtro Espacial]: ", excluidos_espaco, " fogos",
                                   "\n ⚡ Retidos no [3. Filtro de Energia]: ", excluidos_energia, " fogos",
                                   if(excluidos_macro > 0) paste0("\n ⚠️ Falhas Macro (Sem dados anuais): ", excluidos_macro, " fogos") else "",
                                   "\n 📉 Total de fogos não associados neste ano: ", total_excluidos, " fogos",
                                   "\n--------------------------------------------------\n"))
                } else {
                        cat(paste0("\n--------------------------------------------------",
                                   "\n📊 BALANÇO PARCIAL DO ANO: ", ano_ref,
                                   "\n⚠️ Nenhuma associação conseguida para os pontos deste ano.",
                                   "\n",
                                   "\n❌ DIAGNÓSTICO DE NÃO ASSOCIAÇÃO (Funil de Perdas):",
                                   "\n 🛑 Retidos no [1. Filtro Temporal]: ", excluidos_tempo, " fogos",
                                   "\n 📐 Retidos no [2. Filtro Espacial]: ", excluidos_espaco, " fogos",
                                   "\n ⚡ Retidos no [3. Filtro de Energia]: ", excluidos_energia, " fogos",
                                   if(excluidos_macro > 0) paste0("\n ⚠️ Falhas Macro (Sem dados anuais): ", excluidos_macro, " fogos") else "",
                                   "\n 📉 Total de fogos não associados neste ano: ", total_excluidos, " fogos",
                                   "\n--------------------------------------------------\n"))
                }
        }
}

# Unificar a tabela global final
if(length(matches_pontos_list) > 0) {
        tabela_pt_bruta <- bind_rows(matches_pontos_list)
        message(paste("\n🚀 FASE 2 TERMINADA! Total acumulado de todas as associações por Ponto:", nrow(tabela_pt_bruta)))
} else {
        tabela_pt_bruta <- data.frame(Codigo=character(), FRP_val=numeric(), ID_FRP=numeric(), Funcao=character())
}
# ==========================================================
# 4. CRIAÇÃO DOS TRÊS OUTPUTS SOLICITADOS
# ==========================================================

message("\n>>> 4. A gerar e consolidar os outputs finais...")

# --- OUTPUT 1: FRE15_final ---
# Consolida a agregação por incêndio e por tipo de origem ("Poligono" ou "Ponto")
mapeamento_total <- bind_rows(tabela_pol_bruta, tabela_pt_bruta)

if(nrow(mapeamento_total) > 0) {
        FRE15_final <- mapeamento_total %>%
                group_by(Codigo, Funcao) %>%
                summarise(
                        FRE = sum(FRP_val, na.rm = TRUE),
                        Num_Pontos_FRP = n(),
                        .groups = "drop"
                )
} else {
        FRE15_final <- data.frame(Codigo=character(), Funcao=character(), FRE=numeric(), Num_Pontos_FRP=numeric())
}


# --- OUTPUT 2: fogos ---
# Atualização do dataset de fogos original com FRE, Num_Pontos_FRP e a respetiva Funcao
fogos <- fogos %>%
        left_join(FRE15_final, by = "Codigo") %>%
        mutate(
                FRE = coalesce(FRE, 0),
                FRE_GJ = 0.9 * FRE,                          # REVIEW: GJ (0.9 GJ per MW and 15-min slot)
                Num_Pontos_FRP = coalesce(Num_Pontos_FRP, 0),
                Funcao = coalesce(Funcao, "Nenhum")
        ) %>%
        select(-any_of("FimJanela"))


# --- OUTPUT 3: FRP15_sf_final ---
# Objeto geográfico contendo TODOS os pontos originais do satélite, anotados com o vínculo do fogo
tabela_relacao_satelite <- mapeamento_total %>%
        select(ID_FRP, Codigo_Fogo = Codigo, Funcao_Satelite = Funcao) %>%
        distinct(ID_FRP, .keep_all = TRUE) # Segurança contra duplicados involuntários

FRP15_sf_final <- frp_sf_global %>%
        left_join(tabela_relacao_satelite, by = "ID_FRP") %>%
        mutate(
                associado_a_fogo = !is.na(Codigo_Fogo),
                Funcao_Satelite = coalesce(Funcao_Satelite, "Nao Associado")
        )

message("\n✅ Todos os outputs foram criados com sucesso e integridade de dados garantida!")



library(dplyr)

# Filtrar apenas os pontos associados e contar por tipo de geometria de origem
contagem_por_tipo <- FRP15_sf_final %>%
        st_drop_geometry() %>% 
        filter(associado_a_fogo == TRUE) %>% 
        count(Funcao_Satelite, name = "Quantidade") %>%
        # REVIEW: share of all associated hotspots (the previous version divided by the
        # group size inside summarise(), so the share was always 100 %)
        mutate(Percentagem_Do_Total_Associado = 100 * Quantidade / sum(Quantidade))

# Exibir o resultado na consola
print(contagem_por_tipo)

# REVIEW: save the outputs (the previous version kept them only in memory, so a new
# session reloaded fogos without FRE). FRP15_sf_final is the FRP file of the dataset.
if (!exists("output_dir")) output_dir <- file.path(dirname(data_dir), "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(FRE15_final, file.path(output_dir, "FRE15_final.rds"))
saveRDS(fogos[, c("Codigo", "FRE", "FRE_GJ", "Num_Pontos_FRP", "Funcao")] %>%
          { if (inherits(., "sf")) st_drop_geometry(.) else . },
        file.path(output_dir, "fogos_FRE.rds"))
st_write(FRP15_sf_final, file.path(output_dir, "FRP.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message("FRE outputs written to ", output_dir)
