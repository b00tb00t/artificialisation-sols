# =============================================================================
# 02_cantons_complets.R — Construction de la couche canton complète
# Ajoute Paris et Lyon manquants dans la couche canton Admin Express
# =============================================================================

source("R/config.R")

library(sf)
library(dplyr)

# === CHARGEMENT COUCHES SPÉCIFIQUES ===

ade_cantons <- st_read(chemin_ade, layer = "canton", quiet = TRUE)
message("✅ Cantons chargés — ", nrow(ade_cantons), " features")

ade_epci <- st_read(chemin_ade, layer = "epci", quiet = TRUE)
message("✅ EPCI chargés — ", nrow(ade_epci), " features")

ade_arrondissements <- st_read(chemin_ade, layer = "arrondissement", quiet = TRUE)
message("✅ Arrondissements chargés — ", nrow(ade_arrondissements), " features")

# === EXTRACTION PARIS ET LYON ===

canton_paris <- ade_arrondissements |>
  filter(cleabs == "ARR_DEP_0000000000000751")
message("✅ Paris extrait — ", nrow(canton_paris), " feature")

canton_lyon <- ade_epci |>
  filter(cleabs == "EPCI____0000000200046977")
message("✅ Lyon extrait — ", nrow(canton_lyon), " feature")

# === HARMONISATION PARIS ===

canton_paris_harmonise <- canton_paris |>
  mutate(
    code_insee                      = code_canton_paris,
    numero_du_canton                = NA_character_,
    codes_insee_des_arrondissements = NA_character_,
    composition_du_canton           = NA_character_,
    code_insee_de_la_region         = "11"
  ) |>
  select(all_of(names(ade_cantons)))

message("✅ Paris harmonisé")

# === HARMONISATION LYON ===

canton_lyon_harmonise <- canton_lyon |>
  mutate(
    code_insee                      = code_canton_lyon,
    code_insee_du_departement       = "69",
    code_insee_de_la_region         = "84",
    numero_du_canton                = NA_character_,
    codes_insee_des_arrondissements = NA_character_,
    composition_du_canton           = NA_character_
  ) |>
  select(all_of(names(ade_cantons)))

message("✅ Lyon harmonisé")

# === FUSION FINALE ===

cantons_complets <- bind_rows(ade_cantons, canton_paris_harmonise, canton_lyon_harmonise)
message("✅ Cantons complets — ", nrow(cantons_complets), " features (", 
        nrow(ade_cantons), " + Paris + Lyon)")

st_write(cantons_complets, 
         "data/processed/cantons_complets/cantons_complets.gpkg", 
         delete_if_exists = TRUE)
message("✅ Sauvegardé → data/processed/cantons_complets/cantons_complets.gpkg")