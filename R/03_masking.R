# =============================================================================
# 03_masking.R — Intersection OCS GE × Cantons
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")

library(sf)
library(dplyr)
library(stringr)
library(purrr)

# === FONCTION PRINCIPALE ===

masker_departement <- function(code_dept) {
  
  message("🔄 Traitement département : ", code_dept)
  
  # 1. Charger cantons_complets et filtrer le département
  cantons_dept <- st_read(
    "data/processed/cantons_complets/cantons_complets.gpkg",
    quiet = TRUE
  ) |>
    filter(code_insee_du_departement == code_dept)
  message("   📍 ", nrow(cantons_dept), " cantons trouvés pour le département ", code_dept)
  
  # 2. Trouver et charger les millésimes OCS GE
  chemin_dept <- file.path(chemin_ocsge, paste0("dep_", code_dept))
  millesimes  <- trouver_millesimes(chemin_dept)
  
  ocsge_A <- st_read(millesimes$millesime_A, quiet = TRUE)
  message("   📂 Millésime A chargé (", millesimes$annee_A, ") — ", nrow(ocsge_A), " features")
  
  ocsge_B <- st_read(millesimes$millesime_B, quiet = TRUE)
  message("   📂 Millésime B chargé (", millesimes$annee_B, ") — ", nrow(ocsge_B), " features")
  
  # 3. Intersection OCS GE × cantons
  inter_A_raw    <- st_intersection(ocsge_A, cantons_dept)
  intersection_A <- inter_A_raw |>
    mutate(aire_m2 = as.numeric(st_area(inter_A_raw)))
  message("   ✂️ Intersection A — ", nrow(intersection_A), " features")
  
  inter_B_raw    <- st_intersection(ocsge_B, cantons_dept)
  intersection_B <- inter_B_raw |>
    mutate(aire_m2 = as.numeric(st_area(inter_B_raw)))
  message("   ✂️ Intersection B — ", nrow(intersection_B), " features")
  
  # 4. Contrôle qualité
  surface_cantons <- cantons_dept |>
    mutate(aire_canton_m2 = as.numeric(st_area(cantons_dept))) |>
    st_drop_geometry() |>
    select(code_insee, aire_canton_m2)
  
  surface_ocsge_A <- intersection_A |>
    st_drop_geometry() |>
    group_by(code_insee) |>
    summarise(aire_ocsge_m2 = sum(aire_m2))
  
  controle_A <- surface_cantons |>
    left_join(surface_ocsge_A, by = "code_insee") |>
    mutate(taux_couverture = aire_ocsge_m2 / aire_canton_m2 * 100)
  
  message("   📊 Couverture moyenne : ", 
          round(mean(controle_A$taux_couverture, na.rm = TRUE), 1), "%")
  
  cantons_problematiques <- controle_A |> filter(taux_couverture < 90)
  if (nrow(cantons_problematiques) > 0) {
    warning("⚠️ ", nrow(cantons_problematiques), " cantons avec couverture < 90%")
  }
  
  # 5. Sauvegarder
  # Sauvegarder intersection A
  chemin_A <- str_glue("data/processed/intersections/intersection_dep{code_dept}_A.gpkg")
  if (file.exists(chemin_A)) file.remove(chemin_A)
  st_write(intersection_A, chemin_A, quiet = TRUE)
  message("   💾 Intersection A sauvegardée")
  
  # Sauvegarder intersection B
  chemin_B <- str_glue("data/processed/intersections/intersection_dep{code_dept}_B.gpkg")
  if (file.exists(chemin_B)) file.remove(chemin_B)
  st_write(intersection_B, chemin_B, quiet = TRUE)
  message("   💾 Intersection B sauvegardée")
  
  return(paste("✅ Département", code_dept, "traité et sauvegardé"))
}

# === EXÉCUTION SUR LES DÉPARTEMENTS PILOTES ===

resultats <- map(dpt_pilotes, masker_departement)
walk(resultats, message)