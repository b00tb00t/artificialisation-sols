# =============================================================================
# 05b_population.R — Traitement des données de population INSEE
# Agrégation des populations communales au niveau canton
# =============================================================================

source("R/config.R")

library(dplyr)
library(readr)
library(stringr)

# === CHARGEMENT ===

population_raw <- read_delim(
  chemin_insee,
  delim           = ";",
  show_col_types  = FALSE
)
message("✅ Population brute chargée — ", nrow(population_raw), " lignes")

# === NETTOYAGE ===

# Garder uniquement les communes + population municipale
population_com <- population_raw |>
  filter(
    GEO_OBJECT      == "COM",
    POPREF_MEASURE  == "PMUN",
    TIME_PERIOD     %in% c(2018, 2021)
  ) |>
  select(
    code_insee_commune = GEO,
    annee             = TIME_PERIOD,
    population        = OBS_VALUE
  )

message("✅ Population filtrée — ", nrow(population_com), " lignes")

# === JOINTURE COMMUNES → CANTONS ===

# Charger la couche commune Admin Express pour récupérer code_canton
communes_ade <- sf::st_read(chemin_ade, layer = "commune", quiet = TRUE) |>
  sf::st_drop_geometry() |>
  select(
    code_insee_commune = code_insee,
    code_canton        = code_insee_du_canton,
    code_dept          = code_insee_du_departement
  )

# Joindre population × canton
population_cantons <- population_com |>
  left_join(communes_ade, by = "code_insee_commune") |>
  filter(!is.na(code_canton))

# === AGRÉGATION PAR CANTON ===

population_par_canton <- population_cantons |>
  group_by(code_canton, code_dept, annee) |>
  summarise(
    population = sum(population, na.rm = TRUE),
    .groups    = "drop"
  )

message("✅ Population agrégée — ", nrow(population_par_canton), " lignes")

# === CALCUL CROISSANCE DÉMOGRAPHIQUE ===

population_wide <- population_par_canton |>
  tidyr::pivot_wider(
    names_from   = annee,
    values_from  = population,
    names_prefix = "pop_"
  ) |>
  mutate(
    croissance_pct = round((pop_2021 - pop_2018) / pop_2018 * 100, 2)
  )

message("✅ Croissance calculée — ", nrow(population_wide), " cantons")

# === SAUVEGARDE ===

readr::write_csv(
  population_wide,
  "data/processed/agregats/population_cantons.csv"
)
message("✅ Sauvegardé → data/processed/agregats/population_cantons.csv")