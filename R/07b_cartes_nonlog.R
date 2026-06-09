# =============================================================================
# 07_cartes_nonlog.R — Cartes choroplèthes sans transformation logarithmique
# Version annexe pour transparence méthodologique
# Discrétisation : Jenks, 6 classes, valeurs brutes
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")

library(sf)
library(dplyr)
library(tidyr)
library(tmap)
library(stringr)
library(purrr)
library(DBI)
library(RSQLite)
library(here)

tmap_mode("view")

# === CHARGEMENT DES DONNÉES DE FOND ===

cantons_complets <- st_read(
  "data/processed/cantons_complets/cantons_complets.gpkg",
  quiet = TRUE
)

departements <- st_read(chemin_ade, layer = "departement", quiet = TRUE)
regions      <- st_read(chemin_ade, layer = "region",      quiet = TRUE)

message("✅ Fonds de carte chargés")

# === ASSEMBLAGE DES AGRÉGATS ===

tous_agregats <- dpt_pilotes |>
  map_df(\(dept) {
    chemin <- str_glue("data/processed/agregats/agregats_dep{dept}.gpkg")
    if (!file.exists(chemin)) return(NULL)
    con <- dbConnect(RSQLite::SQLite(), chemin)
    df  <- dbReadTable(con, "niveau_1")
    dbDisconnect(con)
    df
  })

agregats_wide <- tous_agregats |>
  select(-aire_totale_m2, -aire_A, -aire_B, -delta) |>
  pivot_wider(
    names_from  = type_cs,
    values_from = c(aire_A_ha, aire_B_ha, delta_ha, delta_pct, annee_A, annee_B)
  ) |>
  mutate(
    across(starts_with("delta_pct"), as.numeric),
    across(starts_with("delta_ha"),  as.numeric),
    across(starts_with("aire_"),     as.numeric),
    annee_A = annee_A_artificiel,
    annee_B = annee_B_artificiel
  ) |>
  select(-starts_with("annee_A_"), -starts_with("annee_B_"))

ref_dept_region <- st_read(chemin_ade, layer = "departement", quiet = TRUE) |>
  st_drop_geometry() |>
  select(
    code_insee_du_departement = code_insee,
    nom_departement           = nom_officiel,
    code_region               = code_insee_de_la_region
  ) |>
  left_join(
    st_read(chemin_ade, layer = "region", quiet = TRUE) |>
      st_drop_geometry() |>
      select(code_region = code_insee, nom_region = nom_officiel),
    by = "code_region"
  )

cantons_carte <- cantons_complets |>
  select(code_insee, code_insee_du_departement, nom_officiel, geom) |>
  left_join(agregats_wide,   by = "code_insee") |>
  left_join(ref_dept_region, by = "code_insee_du_departement")

dpts_ade        <- map_chr(dpt_pilotes, code_dept_vers_ade)
cantons_pilotes <- cantons_carte |>
  filter(code_insee_du_departement %in% dpts_ade)

message("✅ Données assemblées — ", nrow(cantons_pilotes), " cantons")

# === SIMPLIFICATION DES GÉOMÉTRIES ===

cantons_pilotes <- cantons_pilotes |> st_simplify(dTolerance = 100)
departements    <- departements    |> st_simplify(dTolerance = 100)
regions         <- regions         |> st_simplify(dTolerance = 100)

message("✅ Géométries simplifiées")

# === FONCTION CHOROPLÈTHE GÉNÉRIQUE ===

faire_choroplèthe_nonlog <- function(data, variable, palette,
                                     titre, titre_legende, fichier,
                                     popup_vars) {
  carte <- tm_shape(data) +
    tm_polygons(
      fill        = variable,
      fill.scale  = tm_scale_intervals(
        values   = palette,
        style    = "jenks",
        n        = 6,
        value.na = "grey90",
        label.na = "0"
      ),
      fill.legend = tm_legend(title = titre_legende),
      id          = "nom_officiel",
      popup.vars  = popup_vars
    ) +
    tm_shape(departements) +
    tm_borders(col = "grey15", lwd = 1.5) +
    tm_shape(regions) +
    tm_borders(col = "grey5", lwd = 2.5) +
    tm_title(titre) +
    tm_credits(
      "Source : OCS GE NG — IGN | Traitement : R\nNote : discrétisation Jenks sans transformation logarithmique",
      position = c("left", "bottom")
    )
  
  tmap_save(carte, fichier, selfcontained = FALSE)
  message("✅ Carte sauvegardée → ", fichier)
  
  return(invisible(carte))
}

# === CARTE 1 — ARTIFICIALISATION (non log) ===

faire_choroplèthe_nonlog(
  data          = cantons_pilotes,
  variable      = "delta_pct_artificiel",
  palette       = "brewer.reds",
  titre         = "Évolution de l'artificialisation des sols (Jenks brut, 6 classes)",
  titre_legende = "% de variation",
  fichier       = here("data/outputs/cartes/carte_1_artificialisation_nonlog.html"),
  popup_vars    = c("nom_officiel", "nom_departement", "nom_region",
                    "annee_A", "annee_B",
                    "aire_A_ha_artificiel", "aire_B_ha_artificiel",
                    "delta_ha_artificiel", "delta_pct_artificiel")
)

# === CARTE 2 — RENATURATION (non log) ===

faire_choroplèthe_nonlog(
  data          = cantons_pilotes,
  variable      = "delta_pct_naturel",
  palette       = "brewer.greens",
  titre         = "Évolution de la renaturation (Jenks brut, 6 classes)",
  titre_legende = "% de variation",
  fichier       = here("data/outputs/cartes/carte_2_renaturation_nonlog.html"),
  popup_vars    = c("nom_officiel", "nom_departement", "nom_region",
                    "annee_A", "annee_B",
                    "aire_A_ha_naturel", "aire_B_ha_naturel",
                    "delta_ha_naturel", "delta_pct_naturel")
)

message("🎉 Cartes non log produites")