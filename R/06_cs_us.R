# =============================================================================
# 06_cs_us.R — Croisements CS×US
# Sous-analyse 5a : Résidentiel vs Économique
# Sous-analyse 5b : Devenir des forêts et espaces herbacés
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")

library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(DBI)
library(RSQLite)

# === FONCTION PRINCIPALE ===

analyser_cs_us <- function(code_dept) {
  
  message("🔄 Analyse CS×US département : ", code_dept)
  
  # -------------------------------------------------------------------
  # PARTIE 5a — Résidentiel vs Économique
  # -------------------------------------------------------------------
  
  chemin_diff <- str_glue("data/processed/diff/diff_dep{code_dept}.gpkg")
  con         <- dbConnect(RSQLite::SQLite(), chemin_diff)
  niveau_3    <- dbReadTable(con, "niveau_3")
  dbDisconnect(con)
  message("   📂 Niveau 3 chargé — ", nrow(niveau_3), " lignes")
  
  analyse_5a <- niveau_3 |>
    filter(code_cs %in% terrain_artificiel & delta > 0) |>
    mutate(type_us = case_when(
      code_us == "US5"              ~ "résidentiel",
      code_us %in% c("US2", "US3") ~ "économique",
      code_us == "US235"            ~ "mixte",
      TRUE                          ~ "autre"
    )) |>
    group_by(code_insee, type_us) |>
    summarise(
      aire_B_ha = sum(aire_B / 10000, na.rm = TRUE),
      delta_ha  = sum(delta  / 10000, na.rm = TRUE),
      .groups   = "drop"
    )
  
  message("   📊 5a calculé — ", nrow(analyse_5a), " lignes")
  
  # -------------------------------------------------------------------
  # PARTIE 5b — Devenir des forêts et espaces herbacés
  # -------------------------------------------------------------------
  
  # Charger le différentiel IGN
  chemin_dept     <- file.path(chemin_ocsge, paste0("dep_", code_dept))
  chemin_diff_ign <- trouver_diff_ign(chemin_dept)
  diff_ign        <- st_read(chemin_diff_ign, quiet = TRUE)
  message("   📂 Différentiel IGN chargé — ", nrow(diff_ign), " features")
  
  # Charger les cantons du département
  cantons_dept <- st_read(
    "data/processed/cantons_complets/cantons_complets.gpkg",
    quiet = TRUE
  ) |>
    filter(code_insee_du_departement == code_dept_vers_ade(code_dept))
  
  # Intersection différentiel IGN × cantons
  diff_inter   <- st_intersection(diff_ign, cantons_dept)
  diff_cantons <- diff_inter |>
    mutate(aire_ha = as.numeric(st_area(diff_inter)) / 10000)
  message("   ✂️ Intersection différentiel × cantons — ", nrow(diff_cantons), " features")
  
  # Filtrer forêts et herbacés au millésime A
  # Classifier la destination au millésime B
  analyse_5b <- diff_cantons |>
    st_drop_geometry() |>
    filter(CS_2018 %in% cs_nature_5b) |>
    mutate(destination = case_when(
      is.na(CS_2021)                                        ~ label_disparu,
      CS_2021 %in% terrain_artificiel                       ~ "artificialisation",
      CS_2018 %in% cs_forets & CS_2021 %in% cs_herbaces    ~ "dégradation — forêt vers herbacé",
      CS_2018 %in% cs_herbaces & CS_2021 %in% cs_forets    ~ "renaturation pérenne",
      CS_2021 == CS_2018                                    ~ "stable",
      CS_2021 %in% terrain_nature & CS_2021 != CS_2018     ~ "classe de nature similaire",
      TRUE                                                  ~ "autre"
    )) |>
    group_by(code_insee, CS_2018, CS_2021, destination) |>
    summarise(
      aire_ha = sum(aire_ha, na.rm = TRUE),
      .groups = "drop"
    )
  
  message("   📊 5b calculé — ", nrow(analyse_5b), " lignes")
  
  # -------------------------------------------------------------------
  # SAUVEGARDE
  # -------------------------------------------------------------------
  
  chemin_sortie <- str_glue("data/processed/agregats/cs_us_dep{code_dept}.gpkg")
  
  con <- dbConnect(RSQLite::SQLite(), chemin_sortie)
  dbWriteTable(con, "analyse_5a", analyse_5a, overwrite = TRUE)
  message("   💾 Analyse 5a sauvegardée")
  dbWriteTable(con, "analyse_5b", analyse_5b, overwrite = TRUE)
  message("   💾 Analyse 5b sauvegardée")
  dbDisconnect(con)
  
  return(paste("✅ Département", code_dept, "CS×US terminé"))
}

# === EXÉCUTION SUR LES DÉPARTEMENTS PILOTES ===

if (!exists("run_all_active")) {
  resultats <- map(dpt_pilotes, masker_departement)
  walk(resultats, message)
}