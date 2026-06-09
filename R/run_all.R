# =============================================================================
# run_all.R — Orchestrateur principal
# =============================================================================

run_all_active <- TRUE

source("R/config.R")
source("R/00_fonctions.R")

library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(readr)

# === CANTONS COMPLETS — une seule fois ===
source("R/02_cantons_complets.R", local = TRUE)

# =============================================================================
# === INITIALISATION DU LOG ===
# =============================================================================

log_path <- "data/outputs/log_etl.csv"

log_entree <- function(code_dept, etape, statut, message = "") {
  tibble(
    code_dept = as.character(code_dept),
    etape     = as.character(etape),
    statut    = as.character(statut),
    message   = as.character(message),
    timestamp = as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )
}

if (file.exists(log_path)) {
  log_global <- read_csv(log_path,
                         show_col_types = FALSE,
                         col_types = cols(
                           code_dept = col_character(),
                           etape     = col_character(),
                           statut    = col_character(),
                           message   = col_character(),
                           timestamp = col_character()
                         ))
} else {
  log_global <- tibble(
    code_dept = character(),
    etape     = character(),
    statut    = character(),
    message   = character(),
    timestamp = character()
  )
}

logger <- function(code_dept, etape, statut, message = "") {
  nouvelle_entree <- log_entree(code_dept, etape, statut, message)
  log_global      <<- bind_rows(log_global, nouvelle_entree)
  write_csv(log_global, log_path)
  emoji <- if (statut == "succès") "✅" else "❌"
  message(emoji, " [", code_dept, "] ", etape, " — ", statut,
          if (message != "") paste0(" : ", message) else "")
}

message("✅ Log initialisé → ", log_path)

# =============================================================================
# === CHARGEMENT DES FONCTIONS ETL ===
# =============================================================================

source("R/03_masking.R",     local = TRUE)
source("R/04_diff.R",        local = TRUE)
source("R/05_agregation.R",  local = TRUE)
source("R/05b_population.R", local = TRUE)
source("R/06_cs_us.R",       local = TRUE)

# =============================================================================
# === WRAPPER AVEC LOG ===
# =============================================================================

executer_etape <- function(fn, code_dept, nom_etape) {
  fn_safe  <- possibly(fn, otherwise = NULL)
  resultat <- fn_safe(code_dept)
  
  if (is.null(resultat)) {
    msg <- tryCatch(fn(code_dept), error = \(e) e$message)
    logger(code_dept, nom_etape, "échec", as.character(msg))
    return(FALSE)
  } else {
    logger(code_dept, nom_etape, "succès")
    return(TRUE)
  }
}

# =============================================================================
# === TRAITEMENT PAR DÉPARTEMENT ===
# =============================================================================

traiter_departement <- function(code_dept) {
  
  message("\n", strrep("=", 50))
  message("🔄 DÉPARTEMENT : ", code_dept)
  message(strrep("=", 50))
  
  # Masking déjà fait — commenté temporairement
  # ok_masking <- executer_etape(masker_departement, code_dept, "masking")
  # if (!ok_masking) {
  #   message("⏭️ Étapes suivantes ignorées pour dep", code_dept)
  #   return(invisible(NULL))
  # }
  
  #ok_diff <- executer_etape(calculer_diff, code_dept, "diff")
  #if (!ok_diff) {
    #message("⏭️ Étapes suivantes ignorées pour dep", code_dept)
    #return(invisible(NULL))
  #}
  
  #ok_agr <- executer_etape(agreger_departement, code_dept, "agregation")
  #if (!ok_agr) return(invisible(NULL))
  
  ok_csus <- executer_etape(analyser_cs_us, code_dept, "cs_us")
  if (!ok_csus) return(invisible(NULL))
  
  logger(code_dept, "complet", "succès")
  return(invisible(NULL))
}

walk(dpt_pilotes, traiter_departement)

# =============================================================================
# === CARTES ET GRAPHIQUES ===
# =============================================================================

message("\n", strrep("=", 50))
message("🗺️  PRODUCTION DES CARTES")
message(strrep("=", 50))

tryCatch({
  source("R/07_cartes.R", local = TRUE)
  logger("national", "cartes", "succès")
}, error = \(e) {
  logger("national", "cartes", "échec", e$message)
})

message("\n", strrep("=", 50))
message("📊 PRODUCTION DES GRAPHIQUES")
message(strrep("=", 50))

tryCatch({
  source("R/08_graphiques.R", local = TRUE)
  logger("national", "graphiques", "succès")
}, error = \(e) {
  logger("national", "graphiques", "échec", e$message)
})

# =============================================================================
# === RÉSUMÉ FINAL ===
# =============================================================================

message("\n", strrep("=", 50))
message("📋 RÉSUMÉ ETL")
message(strrep("=", 50))

log_global |>
  filter(etape == "complet") |>
  count(statut) |>
  purrr::pwalk(\(statut, n) message(statut, " : ", n, " départements"))

message("\nLog complet → ", log_path)
message("🎉 run_all.R terminé")