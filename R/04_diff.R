# =============================================================================
# 04_diff.R — Calcul du différentiel CS entre millésimes A et B
# =============================================================================

source("R/config.R")

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(DBI)
library(RSQLite)

# === FONCTION PRINCIPALE ===

calculer_diff <- function(code_dept) {
  
  message("🔄 Calcul différentiel département : ", code_dept)
  
  # 1. Chargement
  inter_A <- st_read(
    str_glue("data/processed/intersections/intersection_dep{code_dept}_A.gpkg"),
    quiet = TRUE
  )
  message("   📂 Intersection A chargée — ", nrow(inter_A), " features")
  
  inter_B <- st_read(
    str_glue("data/processed/intersections/intersection_dep{code_dept}_B.gpkg"),
    quiet = TRUE
  )
  message("   📂 Intersection B chargée — ", nrow(inter_B), " features")
  
  # 2. Niveau 2 — détail par canton × code_cs
  niveau_2 <- bind_rows(
    inter_A |> st_drop_geometry() |> mutate(millesime = "A"),
    inter_B |> st_drop_geometry() |> mutate(millesime = "B")
  ) |>
    group_by(code_insee, code_cs, millesime) |>
    summarise(aire_m2 = sum(aire_m2), .groups = "drop") |>
    pivot_wider(
      names_from   = millesime,
      values_from  = aire_m2,
      names_prefix = "aire_"
    ) |>
    mutate(delta = aire_B - aire_A)
  
  message("   📊 Niveau 2 calculé — ", nrow(niveau_2), " lignes")
  
  # 3. Niveau 1 — agrégat artificiel/naturel par canton
  niveau_1 <- niveau_2 |>
    mutate(type_cs = case_when(
      code_cs %in% terrain_artificiel ~ "artificiel",
      code_cs %in% terrain_nature     ~ "naturel",
      TRUE                            ~ "autre"
    )) |>
    group_by(code_insee, type_cs) |>
    summarise(
      aire_A = sum(aire_A, na.rm = TRUE),
      aire_B = sum(aire_B, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(delta = aire_B - aire_A)
  
  message("   📊 Niveau 1 calculé — ", nrow(niveau_1), " lignes")
  
  # 4. Sauvegarde
  chemin_sortie <- str_glue("data/processed/diff/diff_dep{code_dept}.gpkg")
  
  con <- dbConnect(RSQLite::SQLite(), chemin_sortie)
  
  dbWriteTable(con, "niveau_2", niveau_2, overwrite = TRUE)
  message("   💾 Niveau 2 sauvegardé")
  
  dbWriteTable(con, "niveau_1", niveau_1, overwrite = TRUE)
  message("   💾 Niveau 1 sauvegardé")
  
  dbDisconnect(con)
  
  return(paste("✅ Département", code_dept, "différentiel calculé"))
}

# === EXÉCUTION SUR LES DÉPARTEMENTS PILOTES ===

resultats <- map(dpt_pilotes, calculer_diff)
walk(resultats, message)