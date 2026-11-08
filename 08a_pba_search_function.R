# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# 08a_PBA_search_function.R  --  Search function of the PBA-PRFD linkage
# =============================================================================
# SUMMARY (English)
#   Defines executar_busca_exclusiva(), used by 08_PBA_linkage.R (and by 09).
#   The ORIGINAL function of the linkage is not in the code that was found; this
#   is the reconstruction of script 09, based on the calls of the former script
#   27 and on Eq. 1 of the manuscript:
#     S_ij = d_ij / B + ln(1 + |A_PBA,i - BA_PRFD,j| / A_PBA,i)
#   For each radius (from the smallest), the candidate with the lowest S is
#   taken; the search stops at the first radius with S <= UMBRAL (0.6), and
#   otherwise returns the best candidate of all radii. Fires already used in
#   the year are excluded. The distance is measured from the interior point of
#   the polygon (CX, CY) to the ignition point (X_cent, Y_cent). The time window
#   is taken in whole days (00:00 of the first day to 23:59:59 of the last).
#   If you find the original function, source it instead of this file; the
#   results of Tables 5 and 6 may then differ slightly.
# =============================================================================
if (!exists("MODO_B")) MODO_B <- "raio"   # "raio": B = radius of the step | "max": B = largest radius
if (!exists("UMBRAL")) UMBRAL <- 0.6      # stop at the first radius with S <= UMBRAL
suppressPackageStartupMessages(library(sf))
`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

# --- função de procura (RECONSTRUÍDA) ------------------------------------------
# Assinatura deduzida das chamadas do script 27:
#   executar_busca_exclusiva(row, cand, d_ini, d_fim, raios, metodo, usados)
# Eq. 1: S_ij = d_ij / B + ln(1 + |A_PBA,i - BA_PRFD,j| / A_PBA,i)
#   modo "raio": B = raio do passo; pára no 1.º raio com melhor S <= umbral
#   modo "max" : B = max(raios); menor S entre todos os candidatos <= B
# Pressupostos: distância ao ponto interior do polígono (CX, CY); janela inclusiva até
# ao fim do dia de d_fim; BA_PRFD = AreaTotal (NA/<0 -> 0 ha).
executar_busca_exclusiva_reconstruida <- function(row, cand, d_ini, d_fim, raios, metodo,
                                                  usados = character(0),
                                                  modo_B = MODO_B, umbral_parar = UMBRAL) {
        if (is.null(cand) || nrow(cand) == 0) return(NULL)
        cand  <- if (inherits(cand, "sf")) st_drop_geometry(cand) else cand
        raios <- sort(raios)
        ok <- !(as.character(cand$Codigo) %in% usados)
        if (!is.na(d_ini) && !is.na(d_fim)) {
                # window by whole days: from 00:00 of the first day to 23:59:59 of the last
                # (a fire starts before the first hotspot is detected; also makes the
                # comparison insensitive to the UTC / local-time difference)
                d_ini <- as.POSIXct(format(d_ini, "%Y-%m-%d"), tz = attr(d_ini, "tzone") %||% "UTC")
                d_fim <- as.POSIXct(format(d_fim, "%Y-%m-%d"), tz = attr(d_fim, "tzone") %||% "UTC")
                dh <- cand$DHInicio
                ok <- ok & !is.na(dh) & dh >= d_ini & dh <= (d_fim + 86399)
        }
        if (!any(ok)) return(NULL)
        d  <- sqrt((cand$X_cent[ok] - row$CX)^2 + (cand$Y_cent[ok] - row$CY)^2)
        ba <- suppressWarnings(as.numeric(cand$AreaTotal[ok])); ba[is.na(ba) | ba < 0] <- 0
        termo_area <- log1p(abs(row$AreaHaPoly - ba) / row$AreaHaPoly)
        cod <- as.character(cand$Codigo[ok])
        if (all(is.na(d) | d > max(raios))) return(NULL)
        
        if (modo_B == "max") {
                B <- max(raios); dentro <- which(!is.na(d) & d <= B)
                s <- d[dentro] / B + termo_area[dentro]; k <- which.min(s)
                return(list(cod = cod[dentro[k]], met = metodo, sc = s[k]))
        }
        melhor <- NULL
        for (r in raios) {
                dentro <- which(!is.na(d) & d <= r)
                if (length(dentro) == 0) next
                s <- d[dentro] / r + termo_area[dentro]; k <- which.min(s)
                if (s[k] <= umbral_parar) return(list(cod = cod[dentro[k]], met = metodo, sc = s[k]))
                if (is.null(melhor) || s[k] < melhor$sc) melhor <- list(cod = cod[dentro[k]], met = metodo, sc = s[k])
        }
        melhor
}


executar_busca_exclusiva <- executar_busca_exclusiva_reconstruida
message("executar_busca_exclusiva(): RECONSTRUCTED version (modo_B = ", MODO_B, ", umbral = ", UMBRAL, ")")
