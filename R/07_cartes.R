# =============================================================================
# 07_cartes.R — Production des cartes interactives
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
library(classInt)

tmap_mode("view")

# =============================================================================
# === CHARGEMENT DES DONNÉES DE FOND ===
# =============================================================================

cantons_complets <- st_read(
  "data/processed/cantons_complets/cantons_complets.gpkg",
  quiet = TRUE
)

departements <- st_read(chemin_ade, layer = "departement", quiet = TRUE)
regions      <- st_read(chemin_ade, layer = "region",      quiet = TRUE)

message("✅ Fonds de carte chargés")

# =============================================================================
# === ASSEMBLAGE DES AGRÉGATS ===
# =============================================================================

tous_agregats <- dpt_pilotes |>
  map_df(\(dept) {
    chemin <- str_glue("data/processed/agregats/agregats_dep{dept}.gpkg")
    if (!file.exists(chemin)) return(NULL)
    con <- dbConnect(RSQLite::SQLite(), chemin)
    df  <- dbReadTable(con, "niveau_1")
    dbDisconnect(con)
    df
  })

# Aire totale — une ligne par canton depuis type artificiel
aire_totale <- tous_agregats |>
  filter(type_cs == "artificiel") |>
  select(code_insee, aire_totale_m2)

# Pivot wide — une ligne par canton
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
  select(-starts_with("annee_A_"), -starts_with("annee_B_")) |>
  left_join(aire_totale, by = "code_insee")

# Référentiel département → région avec noms
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

# Jointure géométries × données × référentiel
cantons_carte <- cantons_complets |>
  select(code_insee, code_insee_du_departement, nom_officiel, geom) |>
  left_join(agregats_wide,   by = "code_insee") |>
  left_join(ref_dept_region, by = "code_insee_du_departement")

# Filtrer APRÈS la jointure complète
dpts_ade        <- map_chr(dpt_pilotes, code_dept_vers_ade)
cantons_pilotes <- cantons_carte |>
  filter(code_insee_du_departement %in% dpts_ade)

message("✅ Données assemblées — ", nrow(cantons_pilotes), " cantons")

# =============================================================================
# === SIMPLIFICATION DES GÉOMÉTRIES ===
# =============================================================================

cantons_pilotes <- cantons_pilotes |> st_simplify(dTolerance = 100)
departements    <- departements    |> st_simplify(dTolerance = 100)
regions         <- regions         |> st_simplify(dTolerance = 100)

message("✅ Géométries simplifiées")

# =============================================================================
# === TRANSFORMATION LOGARITHMIQUE + BORNES JENKS ===
# =============================================================================

cantons_pilotes <- cantons_pilotes |>
  mutate(
    log_delta_artificiel = if_else(
      delta_pct_artificiel > 0 &
        !is.na(delta_pct_artificiel) &
        is.finite(delta_pct_artificiel),
      log10(delta_pct_artificiel),
      NA_real_
    ),
    log_delta_naturel = if_else(
      delta_pct_naturel < 0 &
        !is.na(delta_pct_naturel) &
        is.finite(delta_pct_naturel),
      log10(abs(delta_pct_naturel)),
      NA_real_
    )
  )

# Bornes jenks — artificialisation
bornes_artif_log <- classIntervals(
  cantons_pilotes$log_delta_artificiel |> na.omit(),
  n     = 5,
  style = "jenks"
)$brks

labels_artif <- paste0(round(10^bornes_artif_log, 3), "%")

# Bornes jenks — renaturation
bornes_nat_log <- classIntervals(
  cantons_pilotes$log_delta_naturel |> na.omit(),
  n     = 5,
  style = "jenks"
)$brks

labels_nat <- paste0("-", round(10^bornes_nat_log, 3), "%")

message("✅ Bornes jenks calculées sur échelle log")

# =============================================================================
# === TAUX ET RANG D'ARTIFICIALISATION ===
# =============================================================================

cantons_pilotes <- cantons_pilotes |>
  mutate(
    taux_artif = aire_B_ha_artificiel / (aire_totale_m2 / 10000) * 100
  ) |>
  group_by(code_region) |>
  mutate(
    rang_artif = percent_rank(taux_artif)
  ) |>
  ungroup()

message("✅ Taux et rang d'artificialisation calculés")

# =============================================================================
# === FONCTION CHOROPLÈTHE GÉNÉRIQUE ===
# =============================================================================

faire_choroplèthe <- function(data, variable, palette, titre,
                              titre_legende, fichier, popup_vars,
                              breaks = NULL, labels = NULL) {
  
  scale_fill <- if (!is.null(breaks)) {
    tm_scale_intervals(
      values       = palette,
      values.range = c(0.25,1),
      breaks       = breaks,
      labels       = labels,
      value.na     = "white",
      label.na     = "0 - Pas de changement"
    )
  } else {
    tm_scale_intervals(
      values       = palette,
      values.range = c(0.25,1),
      style        = "jenks",
      n            = 5,
      value.na     = "white",
      label.na     = "0 - Pas de changement"
    )
  }
  
  carte <- tm_shape(data) +
    tm_polygons(
      fill        = variable,
      fill.scale  = scale_fill,
      fill.legend = tm_legend(title = titre_legende),
      id          = "nom_officiel",
      popup.vars  = popup_vars
    ) +
    tm_shape(departements) +
    tm_borders(col = "grey10", lwd = 1.3) +
    tm_shape(regions) +
    tm_borders(col = "grey5", lwd = 2.5) +
    tm_title(titre) +
    tm_credits("Source : OCS GE NG — IGN | Traitement : R",
               position = c("left", "bottom"))
  
  tmap_save(carte, fichier, selfcontained = FALSE)
  message("✅ Carte sauvegardée → ", fichier)
  
  return(invisible(carte))
}

# =============================================================================
# === CARTE 1 — ARTIFICIALISATION ===
# =============================================================================

faire_choroplèthe(
  data          = cantons_pilotes,
  variable      = "log_delta_artificiel",
  palette       = "brewer.reds",
  titre         = "Évolution de l'artificialisation des sols (échelle log)",
  titre_legende = "% de variation (log)",
  fichier       = here("data/outputs/cartes/carte_1_artificialisation.html"),
  popup_vars    = c("nom_officiel", "nom_departement", "nom_region",
                    "annee_A", "annee_B",
                    "aire_A_ha_artificiel", "aire_B_ha_artificiel",
                    "delta_ha_artificiel", "delta_pct_artificiel"),
  breaks        = bornes_artif_log,
  labels        = labels_artif
)

# =============================================================================
# === CARTE 2 — RENATURATION ===
# =============================================================================

faire_choroplèthe(
  data          = cantons_pilotes,
  variable      = "log_delta_naturel",
  palette       = "brewer.greens",
  titre         = "Évolution de la renaturation (échelle log)",
  titre_legende = "% de variation (log)",
  fichier       = here("data/outputs/cartes/carte_2_renaturation.html"),
  popup_vars    = c("nom_officiel", "nom_departement", "nom_region",
                    "annee_A", "annee_B",
                    "aire_A_ha_naturel", "aire_B_ha_naturel",
                    "delta_ha_naturel", "delta_pct_naturel"),
  breaks        = bornes_nat_log,
  labels        = labels_nat
)

# =============================================================================
# === CARTE 3 — RANG D'ARTIFICIALISATION PAR RÉGION ===
# =============================================================================

bornes_rang <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
labels_rang <- c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")

faire_choroplèthe(
  data          = cantons_pilotes,
  variable      = "rang_artif",
  palette       = "brewer.reds",
  titre         = "Rang d'artificialisation par région",
  titre_legende = "Rang régional (% de cantons\nmoins artificialisés)",
  fichier       = here("data/outputs/cartes/carte_3_rang_artif.html"),
  popup_vars    = c("nom_officiel", "nom_departement", "nom_region",
                    "annee_A", "annee_B",
                    "taux_artif", "rang_artif"),
  breaks        = bornes_rang,
  labels        = labels_rang
)