# =============================================================================
# 05_agregation.R — Agrégation et calcul des indicateurs finaux
# =============================================================================

source("R/config.R")

library(dplyr)
library(purrr)
library(stringr)
library(DBI)
library(RSQLite)

# === FONCTION PRINCIPALE ===

agreger_departement <- function(code_dept) {
  
  message("🔄 Agrégation département : ", code_dept)
  
  # 1. Chargement
  chemin_diff <- str_glue("data/processed/diff/diff_dep{code_dept}.gpkg")
  
  con      <- dbConnect(RSQLite::SQLite(), chemin_diff)
  niveau_1 <- dbReadTable(con, "niveau_1")
  niveau_2 <- dbReadTable(con, "niveau_2")
  dbDisconnect(con)
  
  message("   📂 Niveau 1 chargé — ", nrow(niveau_1), " lignes")
  message("   📂 Niveau 2 chargé — ", nrow(niveau_2), " lignes")
  
  # 2. Calcul du seuil de significativité (documentaire uniquement)
  deltas_non_nuls <- niveau_1 |>
    filter(delta != 0) |>
    pull(delta)
  
  seuil_calc <- quantile(abs(deltas_non_nuls), 0.05)
  message("   📏 Seuil 5% calculé : ", round(seuil_calc, 2), 
          " m² (non appliqué — conservé pour les flux)")
  
  # 3. Calcul aire totale par canton
  aire_totale_canton <- niveau_2 |>
    group_by(code_insee) |>
    summarise(aire_totale_m2 = sum(aire_A, na.rm = TRUE))
  
  message("   📐 Aires totales calculées — ", nrow(aire_totale_canton), " cantons")
  
  # 4. Jointure et calcul des indicateurs
  niveau_1_final <- niveau_1 |>
    left_join(aire_totale_canton, by = "code_insee") |>
    mutate(
      aire_A_ha = aire_A / 10000,
      aire_B_ha = aire_B / 10000,
      delta_ha  = delta  / 10000,
      delta_pct = delta  / aire_totale_m2 * 100
    )
  
  message("   📊 Indicateurs calculés — ", nrow(niveau_1_final), " lignes")
  
  # 5. Sauvegarde
  chemin_sortie <- str_glue("data/processed/agregats/agregats_dep{code_dept}.gpkg")
  
  con <- dbConnect(RSQLite::SQLite(), chemin_sortie)
  
  dbWriteTable(con, "niveau_1", niveau_1_final, overwrite = TRUE)
  message("   💾 Niveau 1 sauvegardé")
  
  dbWriteTable(con, "niveau_2", niveau_2, overwrite = TRUE)
  message("   💾 Niveau 2 sauvegardé")
  
  dbDisconnect(con)
  
  return(paste("✅ Département", code_dept, "agrégation terminée"))
}

# === EXÉCUTION SUR LES DÉPARTEMENTS PILOTES ===

resultats <- map(dpt_pilotes, agreger_departement)
walk(resultats, message)