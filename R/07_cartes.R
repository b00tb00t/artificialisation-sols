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
    con <- dbConnect(RSQLite::SQLite(),
                     str_glue("data/processed/agregats/agregats_dep{dept}.gpkg"))
    df  <- dbReadTable(con, "niveau_1")
    dbDisconnect(con)
    df
  })

# Pivot wide — une ligne par canton
agregats_wide <- tous_agregats |>
  select(-aire_totale_m2, -aire_A, -aire_B, -delta) |>
  pivot_wider(
    names_from  = type_cs,
    values_from = c(aire_A_ha, aire_B_ha, delta_ha, delta_pct)
  )

message("✅ Données assemblées — ", nrow(cantons_pilotes), " cantons")

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

# Jointure sur cantons_carte — version finale avec tout
cantons_carte <- cantons_complets |>
  select(code_insee, code_insee_du_departement, nom_officiel, geom) |>
  left_join(agregats_wide, by = "code_insee") |>
  left_join(ref_dept_region, by = "code_insee_du_departement")

# Filtrer APRÈS la jointure complète
cantons_pilotes <- cantons_carte |>
  filter(code_insee_du_departement %in% dpt_pilotes)

# === FONCTION CHOROPLÈTHE GÉNÉRIQUE ===

faire_choroplèthe <- function(data, variable, palette, titre,
                              titre_legende, fichier, popup_vars) {
  carte <- tm_shape(data) +
    tm_polygons(
      fill        = variable,
      fill.scale  = tm_scale(
        values = palette,
        style  = "jenks",
        n      = 5
      ),
      fill.legend = tm_legend(title = titre_legende),
      id          = "nom_officiel",
      popup.vars  = popup_vars
    ) +
    tm_shape(departements) +
    tm_borders(col = "grey40", lwd = 1.5) +
    tm_shape(regions) +
    tm_borders(col = "grey20", lwd = 2.5) +
    tm_title(titre) +
    tm_credits("Source : OCS GE NG — IGN | Traitement : R",
               position = c("left", "bottom"))
  
  tmap_save(carte, fichier, selfcontained = FALSE)
  message("✅ Carte sauvegardée → ", fichier)
  
  return(invisible(carte))
}

# === CARTES 1 ET 2 — CHOROPLÈTHES ===

faire_choroplèthe(
  data          = cantons_pilotes,
  variable      = "delta_pct_artificiel",
  palette       = "brewer.reds",
  titre         = "Évolution de l'artificialisation des sols",
  titre_legende = "% de variation",
  fichier       = here("data/outputs/cartes/carte_1_artificialisation.html"),
  popup_vars    = c("nom_officiel",
                    "nom_departement",
                    "nom_region",
                    "aire_A_ha_artificiel",
                    "aire_B_ha_artificiel", 
                    "delta_ha_artificiel",
                    "delta_pct_artificiel")
)

faire_choroplèthe(
  data          = cantons_pilotes,
  variable      = "delta_pct_naturel",
  palette       = "brewer.greens",
  titre         = "Évolution de la renaturation",
  titre_legende = "% de variation",
  fichier       = here("data/outputs/cartes/carte_2_renaturation.html"),
  popup_vars    = c("nom_officiel",
                    "nom_departement",
                    "nom_region",
                    "aire_A_ha_naturel",
                    "aire_B_ha_naturel",
                    "delta_ha_naturel",
                    "delta_pct_naturel")
)

# === CARTE 3 — CERCLES PROPORTIONNELS ===

# Centroïdes garantis à l'intérieur des polygones
centroides <- cantons_pilotes |> st_point_on_surface()

# Décalage propre en conservant le CRS Lambert-93
coords <- st_coordinates(centroides)

# Créer un df avec les coordonnées décalées + attributs
df_artif <- centroides |> 
  st_drop_geometry() |>
  mutate(
    X = coords[, 1] - 2000,
    Y = coords[, 2]
  )

centroides_artif <- st_as_sf(df_artif, coords = c("X", "Y"), crs = crs_travail)

df_renat <- centroides |>
  st_drop_geometry() |>
  mutate(
    X = coords[, 1] + 2000,
    Y = coords[, 2]
  )

centroides_renat <- st_as_sf(df_renat, coords = c("X", "Y"), crs = crs_travail)

# Séparer gains et pertes
artif_pos <- centroides_artif |> filter(delta_ha_artificiel >  0)
artif_neg <- centroides_artif |>
  filter(delta_ha_artificiel <  0) |>
  mutate(delta_ha_artificiel = abs(delta_ha_artificiel))

renat_pos <- centroides_renat |> filter(delta_ha_naturel >  0)
renat_neg <- centroides_renat |>
  filter(delta_ha_naturel <  0) |>
  mutate(delta_ha_naturel = abs(delta_ha_naturel))

# Carte 3 — symboles en premier, fonds de carte en dessous
# (en mode view Leaflet, dernière couche = en dessous)
carte_3 <-
  # Gains artificialisation — cercles jaunes
  tm_shape(artif_pos) +
  tm_symbols(
    size        = "delta_ha_artificiel",
    fill        = "#FFD700",
    shape       = 21,
    size.scale = tm_scale_continuous(limits = c(0, 170), values.scale = 2.4),  
    size.legend = tm_legend(title = "Artificialisation (ha)")
  ) +
  # Pertes artificialisation — carrés jaunes
  tm_shape(artif_neg) +
  tm_symbols(
    size        = "delta_ha_artificiel",
    fill        = "#FFD700",
    shape       = 22,
    size.scale = tm_scale_continuous(limits = c(0, 170), values.scale = 2.4),  
    size.legend = tm_legend_hide()
  ) +
  # Gains renaturation — cercles verts
  tm_shape(renat_pos) +
  tm_symbols(
    size        = "delta_ha_naturel",
    fill        = "#228B22",
    shape       = 21,
    size.scale = tm_scale_continuous(limits = c(0, 170), values.scale = 2.4), 
    size.legend = tm_legend(title = "Renaturation (ha)")
  ) +
  # Pertes renaturation — carrés verts
  tm_shape(renat_neg) +
  tm_symbols(
    size        = "delta_ha_naturel",
    fill        = "#228B22",
    shape       = 22,
    size.scale = tm_scale_continuous(limits = c(0, 170), values.scale = 2.4),  
    size.legend = tm_legend_hide()
  ) +
  # Fonds de carte — déclarés en dernier = en dessous en mode view
  tm_shape(cantons_pilotes) +
  tm_borders(col = "black", lwd = 0.85) +
  tm_shape(departements) +
  tm_borders(col = "grey40", lwd = 1.5) +
  tm_shape(regions) +
  tm_borders(col = "grey20", lwd = 2.5) +
  tm_title("Volumes d'artificialisation et renaturation par canton") +
  tm_credits("Source : OCS GE NG — IGN | Traitement : R",
             position = c("left", "bottom"))

tmap_save(carte_3,
          here("data/outputs/cartes/carte_3_cercles.html"),
          selfcontained = FALSE)
message("✅ Carte 3 sauvegardée")