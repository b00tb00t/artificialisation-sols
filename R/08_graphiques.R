# =============================================================================
# 08_graphiques.R — Production des graphiques analytiques
# Niveaux : national / régional / départemental
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggalluvial)
library(stringr)
library(purrr)
library(readr)
library(DBI)
library(RSQLite)
library(sf)

# =============================================================================
# === CHARGEMENT DES DONNÉES ===
# =============================================================================

# Référentiel département → région (avec noms officiels)
ref_dept_region <- st_read(chemin_ade, layer = "departement", quiet = TRUE) |>
  st_drop_geometry() |>
  select(
    code_dept   = code_insee,
    code_region = code_insee_de_la_region
  ) |>
  left_join(
    st_read(chemin_ade, layer = "region", quiet = TRUE) |>
      st_drop_geometry() |>
      select(code_region = code_insee, nom_region = nom_officiel),
    by = "code_region"
  )

message("✅ Référentiel département-région chargé")

# Niveau 1 — artificialisation/renaturation par canton
tous_niveau_1 <- dpt_pilotes |>
  map_df(\(dept) {
    chemin <- str_glue("data/processed/agregats/agregats_dep{dept}.gpkg")
    if (!file.exists(chemin)) return(NULL)   # ← chemin défini avant
    con <- dbConnect(RSQLite::SQLite(), chemin)
    df  <- dbReadTable(con, "niveau_1")
    dbDisconnect(con)
    df |> mutate(code_dept = dept)
  }) |>
  left_join(ref_dept_region, by = "code_dept")

# Niveau 2 — détail par canton × code_cs
tous_niveau_2 <- dpt_pilotes |>
  map_df(\(dept) {
    chemin <- str_glue("data/processed/agregats/agregats_dep{dept}.gpkg")
    if (!file.exists(chemin)) return(NULL)   # ← chemin défini avant
    con <- dbConnect(RSQLite::SQLite(), chemin)
    df  <- dbReadTable(con, "niveau_2")
    dbDisconnect(con)
    df |> mutate(code_dept = dept)
  }) |>
  left_join(ref_dept_region, by = "code_dept")

# CS×US
tous_5a <- dpt_pilotes |>
  map_df(\(dept) {
    chemin <- str_glue("data/processed/agregats/cs_us_dep{dept}.gpkg")
    if (!file.exists(chemin)) return(NULL)   # ← chemin défini avant
    con <- dbConnect(RSQLite::SQLite(), chemin)
    df  <- dbReadTable(con, "analyse_5a") |> mutate(code_dept = dept)
    dbDisconnect(con)
    df
  }) |>
  left_join(ref_dept_region, by = "code_dept")

tous_5b <- dpt_pilotes |>
  map_df(\(dept) {
    con <- dbConnect(RSQLite::SQLite(),
                     str_glue("data/processed/agregats/cs_us_dep{dept}.gpkg"))
    df  <- dbReadTable(con, "analyse_5b") |> mutate(code_dept = dept)
    dbDisconnect(con)
    df
  }) |>
  left_join(ref_dept_region, by = "code_dept")

# Population
population_wide <- read_csv(
  "data/processed/agregats/population_cantons.csv",
  show_col_types = FALSE
)

message("✅ Toutes les données chargées")

# =============================================================================
# === FONCTIONS UTILITAIRES ===
# =============================================================================

# Créer dossier si inexistant et sauvegarder widget plotly
sauvegarder_plotly <- function(widget, chemin) {
  dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(widget, chemin, selfcontained = FALSE)
  message("✅ Sauvegardé → ", chemin)
}

# Labels lisibles pour les codes CS
labeller_cs <- function(code_cs) {
  case_when(
    code_cs == "CS1.1.1.1" ~ "Bâti",
    code_cs == "CS1.1.1.2" ~ "Imperméable non bâti",
    code_cs == "CS1.1.2.1" ~ "Matériaux minéraux",
    code_cs == "CS1.1.2.2" ~ "Matériaux composites",
    TRUE                   ~ code_cs
  )
}

# Seuil flux 5%
seuil_flux <- quantile(
  abs(tous_5b$aire_ha[tous_5b$aire_ha > 0]),
  0.05,
  na.rm = TRUE
)
message("📏 Seuil flux 5% : ", round(seuil_flux, 2), " ha")

# =============================================================================
# === NIVEAU DÉPARTEMENTAL ===
# Module 2 : Typologies d'artificialisation par canton
# Module 3 : Devenir des espaces naturels et forestiers
# =============================================================================

produire_graphiques_dept <- function(code_dept) {
  
  message("🔄 Graphiques département : ", code_dept)
  
  # --- Module 2 — Typologies par canton ---
  
  typo_canton <- tous_niveau_2 |>
    filter(
      code_dept == !!code_dept,
      code_cs   %in% terrain_artificiel,
      delta     > 0
    ) |>
    mutate(
      libelle_cs = labeller_cs(code_cs),
      delta_ha   = delta / 10000
    ) |>
    group_by(code_insee, libelle_cs) |>
    summarise(delta_ha = sum(delta_ha, na.rm = TRUE), .groups = "drop")
  
  gg <- ggplot(
    typo_canton,
    aes(x = reorder(code_insee, -delta_ha), y = delta_ha, fill = libelle_cs)
  ) +
    geom_col() +
    scale_fill_brewer(palette = "OrRd", name = "Type de surface") +
    labs(
      title   = str_glue("Typologies d'artificialisation — Département {code_dept}"),
      x       = "Canton",
      y       = "Gain (ha)",
      caption = "Source : OCS GE NG — IGN"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, size = 6))
  
  sauvegarder_plotly(
    ggplotly(gg),
    str_glue("data/outputs/graphiques/departemental/dep{code_dept}/module2_typo_canton.html")
  )
  
  # --- Module 3 — Devenir des forêts par département ---
  
  devenir_dept <- tous_5b |>
    filter(
      code_dept == !!code_dept,
      aire_ha   >= seuil_flux
    ) |>
    mutate(
      source = case_when(
        CS_2018 == "CS2.1.1.1" ~ "Feuillus",
        CS_2018 == "CS2.1.1.2" ~ "Conifères",
        CS_2018 == "CS2.1.1.3" ~ "Mixte",
        CS_2018 == "CS2.2.1"   ~ "Herbacé",
        TRUE                   ~ CS_2018
      )
    ) |>
    group_by(source, destination) |>
    summarise(aire_ha = sum(aire_ha, na.rm = TRUE), .groups = "drop")
  
  gg_flux <- ggplot(
    devenir_dept,
    aes(axis1 = source, axis2 = destination, y = aire_ha)
  ) +
    geom_alluvium(aes(fill = source), width = 0.3, alpha = 0.7) +
    geom_stratum(width = 0.3, fill = "white", color = "grey40") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_fill_brewer(palette = "Set2", name = "Type de nature") +
    labs(
      title   = str_glue("Devenir des espaces naturels — Département {code_dept}"),
      x       = NULL,
      y       = "Surface (ha)",
      caption = "Source : OCS GE NG — IGN"
    ) +
    theme_minimal()
  
  sauvegarder_plotly(
    ggplotly(gg_flux),
    str_glue("data/outputs/graphiques/departemental/dep{code_dept}/module3_devenir_forets.html")
  )
  
  return(paste("✅ Département", code_dept, "graphiques produits"))
}

# Exécution sur tous les départements pilotes
resultats_dept <- map(dpt_pilotes, produire_graphiques_dept)
walk(resultats_dept, message)

# =============================================================================
# === NIVEAU RÉGIONAL ===
# Module 2 : Typologies d'artificialisation par département
# Module 4a : Résidentiel vs Économique
# Module 4b : Que deviennent les espaces naturels
# =============================================================================

codes_regions <- unique(tous_niveau_1$code_region)

produire_graphiques_region <- function(code_reg) {
  
  message("🔄 Graphiques région : ", code_reg)
  
  nom_reg <- tous_niveau_1 |>
    filter(code_region == code_reg) |>
    pull(nom_region) |>
    first()
  
  # --- Module 2 — Typologies par département ---
  
  typo_dept <- tous_niveau_2 |>
    filter(
      code_region == !!code_reg,
      code_cs     %in% terrain_artificiel,
      delta       > 0
    ) |>
    mutate(
      libelle_cs = labeller_cs(code_cs),
      delta_ha   = delta / 10000
    ) |>
    group_by(code_dept, libelle_cs) |>
    summarise(delta_ha = sum(delta_ha, na.rm = TRUE), .groups = "drop")
  
  gg_typo <- ggplot(
    typo_dept,
    aes(x = code_dept, y = delta_ha, fill = libelle_cs)
  ) +
    geom_col() +
    scale_fill_brewer(palette = "OrRd", name = "Type de surface") +
    labs(
      title   = str_glue("Typologies d'artificialisation — {nom_reg}"),
      x       = "Département",
      y       = "Gain (ha)",
      caption = "Source : OCS GE NG — IGN"
    ) +
    theme_minimal()
  
  sauvegarder_plotly(
    ggplotly(gg_typo),
    str_glue("data/outputs/graphiques/regional/reg{code_reg}/module2_typo_dept.html")
  )
  
  # --- Module 4a — Résidentiel vs Économique ---
  
  reseco_reg <- tous_5a |>
    filter(
      code_region == !!code_reg,
      type_us     %in% c("résidentiel", "économique")
    ) |>
    group_by(code_dept, type_us) |>
    summarise(delta_ha = sum(delta_ha, na.rm = TRUE), .groups = "drop")
  
  gg_reseco <- ggplot(
    reseco_reg,
    aes(x = code_dept, y = delta_ha, fill = type_us)
  ) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      values = c("résidentiel" = "#E07B54", "économique" = "#5B8DB8"),
      name   = "Type d'usage"
    ) +
    labs(
      title   = str_glue("Artificialisation résidentiel vs économique — {nom_reg}"),
      x       = "Département",
      y       = "Gain (ha)",
      caption = "Source : OCS GE NG — IGN"
    ) +
    theme_minimal()
  
  sauvegarder_plotly(
    ggplotly(gg_reseco),
    str_glue("data/outputs/graphiques/regional/reg{code_reg}/module4a_residentiel_eco.html")
  )
  
  # --- Module 4b — Devenir espaces naturels ---
  
  devenir_reg <- tous_5b |>
    filter(code_region == !!code_reg) |>
    group_by(destination) |>
    summarise(aire_ha = sum(aire_ha, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(aire_ha))
  
  gg_devenir <- ggplot(
    devenir_reg,
    aes(x = reorder(destination, aire_ha), y = aire_ha, fill = destination)
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title   = str_glue("Que deviennent les espaces naturels ? — {nom_reg}"),
      x       = NULL,
      y       = "Surface (ha)",
      caption = "Source : OCS GE NG — IGN"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  sauvegarder_plotly(
    ggplotly(gg_devenir),
    str_glue("data/outputs/graphiques/regional/reg{code_reg}/module4b_devenir_nature.html")
  )
  
  return(paste("✅ Région", code_reg, "graphiques produits"))
}

resultats_reg <- map(codes_regions, produire_graphiques_region)
walk(resultats_reg, message)

# =============================================================================
# === NIVEAU NATIONAL ===
# Module 4 scatter : artificialisation × démographie
# =============================================================================

message("🔄 Graphique national — scatter démographie")

artif_canton <- tous_niveau_1 |>
  filter(type_cs == "artificiel") |>
  select(code_canton = code_insee, code_dept, delta_ha_artificiel = delta_ha)

profil_canton <- tous_5a |>
  filter(type_us %in% c("résidentiel", "économique")) |>
  group_by(code_insee) |>
  mutate(total = sum(delta_ha)) |>
  filter(type_us == "résidentiel") |>
  mutate(pct_residentiel = round(delta_ha / total * 100, 1)) |>
  select(code_canton = code_insee, pct_residentiel)

scatter_data <- artif_canton |>
  left_join(
    population_wide |> rename(code_canton = code_canton),
    by = "code_canton"
  ) |>
  left_join(profil_canton, by = "code_canton") |>
  filter(!is.na(croissance_pct), !is.na(delta_ha_artificiel))

gg_scatter <- ggplot(
  scatter_data,
  aes(
    x    = croissance_pct,
    y    = delta_ha_artificiel,
    color = pct_residentiel,
    text  = paste0(
      "Canton : ",      code_canton,
      "<br>Artif. : ",  round(delta_ha_artificiel, 1), " ha",
      "<br>Croissance : ", round(croissance_pct, 1),   "%",
      "<br>% résidentiel : ", pct_residentiel,          "%"
    )
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", linetype = "dashed") +
  scale_color_gradient(
    low  = "#5B8DB8",
    high = "#E07B54",
    name = "% résidentiel"
  ) +
  labs(
    title   = "Artificialisation et croissance démographique par canton",
    x       = "Croissance démographique 2018-2021 (%)",
    y       = "Artificialisation nette (ha)",
    caption = "Source : OCS GE NG — IGN | INSEE Populations légales"
  ) +
  theme_minimal()

sauvegarder_plotly(
  ggplotly(gg_scatter, tooltip = "text"),
  "data/outputs/graphiques/national/module4_scatter_demo.html"
)

message("🎉 Tous les graphiques produits")