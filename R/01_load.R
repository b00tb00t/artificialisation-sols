# =============================================================================
# 01_load.R — Chargement de toutes les données sources
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")

library(sf)
library(readr)

# === CHARGEMENT ADMIN EXPRESS ===

ade_regions      <- st_read(chemin_ade, layer = "region",      quiet = TRUE)
message("✅ Régions chargées — ", nrow(ade_regions), " features")

ade_departements <- st_read(chemin_ade, layer = "departement", quiet = TRUE)
message("✅ Départements chargés — ", nrow(ade_departements), " features")

ade_cantons      <- st_read(chemin_ade, layer = "canton",      quiet = TRUE)
message("✅ Cantons chargés — ", nrow(ade_cantons), " features")

ade_communes     <- st_read(chemin_ade, layer = "commune",     quiet = TRUE)
message("✅ Communes chargées — ", nrow(ade_communes), " features")

# === CHARGEMENT OCS GE ===

# Département 34 — Hérault
resultats_34 <- trouver_millesimes(chemin_ocsge_dep_34)
message("✅ Millésimes trouvés — Année A : ", resultats_34$annee_A, " / Année B : ", resultats_34$annee_B)

ocsge_34_A <- st_read(resultats_34$millesime_A, quiet = TRUE)
message("✅ Millésime A chargé (", resultats_34$annee_A, ") — ", nrow(ocsge_34_A), " features")

ocsge_34_B <- st_read(resultats_34$millesime_B, quiet = TRUE)
message("✅ Millésime B chargé (", resultats_34$annee_B, ") — ", nrow(ocsge_34_B), " features")

# Département 33 — Gironde
resultats_33 <- trouver_millesimes(chemin_ocsge_dep_33)
message("✅ Millésimes trouvés — Année A : ", resultats_33$annee_A, " / Année B : ", resultats_33$annee_B)

ocsge_33_A <- st_read(resultats_33$millesime_A, quiet = TRUE)
message("✅ Millésime A chargé (", resultats_33$annee_A, ") — ", nrow(ocsge_33_A), " features")

ocsge_33_B <- st_read(resultats_33$millesime_B, quiet = TRUE)
message("✅ Millésime B chargé (", resultats_33$annee_B, ") — ", nrow(ocsge_33_B), " features")

# === CHARGEMENT INSEE ===

population_raw <- read_csv2(chemin_insee, show_col_types = FALSE)
message("✅ Population chargée — ", nrow(population_raw), " lignes")