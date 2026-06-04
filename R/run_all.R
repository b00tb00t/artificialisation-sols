# =============================================================================
# run_all.R — Orchestrateur principal
# Lance l'ETL complet pour tous les départements définis dans config.R
# Produit un log CSV des succès/échecs par étape
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")
source("R/02_cantons_complets.R")  # une seule fois — indépendant des départements

library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

# =============================================================================
# === INITIALISATION DU LOG ===
# =============================================================================

log_path <- "data/outputs/log_etl.csv"

# Créer structure du log
log_entree <- function(code_dept, etape, statut, message = "") {
  tibble(
    code_dept  = code_dept,
    etape      = etape,
    statut     = statut,
    message    = message,
    timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# Initialiser ou charger log existant
if (file.exists(log_path)) {
  log_global <- read_csv(log_path, show_col_types = FALSE)
} else {
  log_global <- tibble(
    code_dept = character(),
    etape     = character(),
    statut    = character(),
    message   = character(),
    timestamp = character()
  )
}

# Fonction pour ajouter une entrée et sauvegarder
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
# === FONCTIONS ETL WRAPPÉES AVEC possibly() ===
# =============================================================================

source("R/03_masking.R",    local = TRUE)
source("R/04_diff.R",       local = TRUE)
source("R/05_agregation.R", local = TRUE)
source("R/05b_population.R",local = TRUE)
source("R/06_cs_us.R",      local = TRUE)

# Wrapper générique avec log
executer_etape <- function(fn, code_dept, nom_etape) {
  fn_safe <- possibly(fn, otherwise = NULL)
  resultat <- fn_safe(code_dept)
  
  if (is.null(resultat)) {
    logger(code_dept, nom_etape, "échec",
           as.character(tryCatch(fn(code_dept), error = \(e) e$message)))
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
  
  # Étape 1 — Masking
  ok_masking <- executer_etape(masker_departement, code_dept, "masking")
  if (!ok_masking) {
    message("⏭️ Étapes suivantes ignorées pour dep", code_dept)
    return(invisible(NULL))
  }
  
  # Étape 2 — Diff
  ok_diff <- executer_etape(calculer_diff, code_dept, "diff")
  if (!ok_diff) {
    message("⏭️ Étapes suivantes ignorées pour dep", code_dept)
    return(invisible(NULL))
  }
  
  # Étape 3 — Agrégation
  ok_agr <- executer_etape(agreger_departement, code_dept, "agregation")
  if (!ok_agr) return(invisible(NULL))
  
  # Étape 4 — CS×US
  ok_csus <- executer_etape(analyser_cs_us, code_dept, "cs_us")
  if (!ok_csus) return(invisible(NULL))
  
  logger(code_dept, "complet", "succès")
  return(invisible(NULL))
}

# Exécution sur tous les départements définis dans config.R
walk(dpt_pilotes, traiter_departement)

# =============================================================================
# === CARTES ET GRAPHIQUES (après tous les départements) ===
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

message("\n Log complet → ", log_path)
message("🎉 run_all.R terminé")