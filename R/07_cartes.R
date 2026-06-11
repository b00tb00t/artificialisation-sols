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
# === TRANSFORMATION LOGARITHMIQUE — CARTE 1 (artificialisation) ===
# =============================================================================

cantons_pilotes <- cantons_pilotes |>
  mutate(
    log_delta_artificiel = if_else(
      delta_pct_artificiel > 0 &
        !is.na(delta_pct_artificiel) &
        is.finite(delta_pct_artificiel),
      log10(delta_pct_artificiel),
      NA_real_
    )
  )

# Bornes Jenks sur échelle log — back-transformées pour affichage
bornes_artif_log <- classIntervals(
  cantons_pilotes$log_delta_artificiel |> na.omit(),
  n     = 5,
  style = "jenks"
)$brks

labels_artif <- paste0(round(10^bornes_artif_log, 3), "%")

message("✅ Bornes artificialisation calculées (log)")

# =============================================================================
# === BORNES DIVERGENTES — CARTE 2 (surface naturelle) ===
# Approche : bornes calculées dans l'espace log séparément pour chaque côté
# Les valeurs 0 tombent naturellement dans la classe entre la dernière
# borne négative et la première borne positive — pas de traitement spécial
# =============================================================================

# Extraire les deux populations (sans les zéros)
pertes_nat <- cantons_pilotes$delta_pct_naturel |>
  na.omit() |>
  keep(\(x) x < 0)

gains_nat <- cantons_pilotes$delta_pct_naturel |>
  na.omit() |>
  keep(\(x) x > 0)

# Jenks dans l'espace log sur valeurs absolues
bornes_pertes_log <- classIntervals(
  log10(abs(pertes_nat)),
  n     = 3,
  style = "jenks"
)$brks

bornes_gains_log <- classIntervals(
  log10(gains_nat),
  n     = 2,
  style = "jenks"
)$brks

# Reconvertir vers valeurs originales avec signe
bornes_pertes_orig <- -round(10^rev(bornes_pertes_log), 3)
bornes_gains_orig  <-  round(10^bornes_gains_log,       3)

# Bornes finales — les valeurs 0 tomberont dans l'intervalle
# [max(bornes_pertes_orig), min(bornes_gains_orig)]
bornes_nat_finales <- unique(sort(c(
  bornes_pertes_orig,
  bornes_gains_orig
)))

# Labels lisibles en valeurs originales
labels_nat_finales <- sapply(bornes_nat_finales, \(x) {
  if (x >  0)  return(paste0("+", x, "%"))
  if (x <  0)  return(paste0(x,       "%"))
})

message("✅ Bornes surface naturelle calculées")
message("   Plage bornes : ", min(bornes_nat_finales), "% → +",
        max(bornes_nat_finales), "%")

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
# show_na : TRUE = afficher "0 — pas de changement" en blanc (cartes 1 et 3)
#           FALSE = pas de classe NA dans la légende (carte 2)
# =============================================================================

faire_choroplèthe <- function(data, variable, palette, titre,
                              titre_legende, fichier, popup_vars,
                              breaks = NULL, labels = NULL,
                              show_na = TRUE) {
  
  scale_fill <- if (!is.null(breaks)) {
    if (show_na) {
      tm_scale_intervals(
        values       = palette,
        values.range = c(0.25, 1),
        breaks       = breaks,
        labels       = labels,
        value.na     = "white",
        label.na     = "0 — pas de changement"
      )
    } else {
      tm_scale_intervals(
        values       = palette,
        values.range = c(0.25, 1),
        breaks       = breaks,
        labels       = labels
      )
    }
  } else {
    if (show_na) {
      tm_scale_intervals(
        values       = palette,
        values.range = c(0.25, 1),
        style        = "jenks",
        n            = 5,
        value.na     = "white",
        label.na     = "0 — pas de changement"
      )
    } else {
      tm_scale_intervals(
        values       = palette,
        values.range = c(0.25, 1),
        style        = "jenks",
        n            = 5
      )
    }
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
    tm_borders(col = "grey15", lwd = 1.5) +
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
  labels        = labels_artif,
  show_na       = TRUE
)

# =============================================================================
# === CARTE 2 — ÉVOLUTION DE LA SURFACE NATURELLE ===
# delta_pct_naturel utilisé directement — les 0 tombent dans l'intervalle
# [max(bornes_pertes), min(bornes_gains)] sans traitement spécial
# show_na = FALSE — pas de classe "Missing" dans la légende
# =============================================================================

faire_choroplèthe(
  data          = cantons_pilotes,
  variable      = "delta_pct_naturel",
  palette       = "brewer.br_bg",
  titre         = "Évolution de la surface naturelle",
  titre_legende = "% de variation\n+ gain · - perte",
  fichier       = here("data/outputs/cartes/carte_2_renaturation.html"),
  popup_vars    = c("nom_officiel", "nom_departement", "nom_region",
                    "annee_A", "annee_B",
                    "aire_A_ha_naturel", "aire_B_ha_naturel",
                    "delta_ha_naturel", "delta_pct_naturel"),
  breaks        = bornes_nat_finales,
  labels        = labels_nat_finales,
  show_na       = FALSE
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
  labels        = labels_rang,
  show_na       = TRUE
)