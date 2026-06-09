# =============================================================================
# 00_fonctions.R — Fonctions réutilisables du projet
# =============================================================================

library(stringr)

# -----------------------------------------------------------------------------
# trouver_millesimes()
# Trouve automatiquement les fichiers OCCUPATION_SOL.gpkg dans un dossier
# département et identifie les millésimes A (plus ancien) et B (plus récent)
#
# Argument : chemin_dept (character) — chemin vers le dossier du département
# Retourne : liste nommée avec millesime_A, millesime_B, annee_A, annee_B
# -----------------------------------------------------------------------------

trouver_millesimes <- function(chemin_dept) {
  
  if (!dir.exists(chemin_dept)) {
    stop("Le dossier département n'existe pas : ", chemin_dept)
  }
  
  fichiers <- list.files(
    chemin_dept,
    pattern    = "OCCUPATION_SOL\\.gpkg$",
    recursive  = TRUE,
    full.names = TRUE
  )
  
  if (length(fichiers) == 0) {
    stop("Aucun fichier OCCUPATION_SOL.gpkg trouvé dans : ", chemin_dept)
  }
  
  if (length(fichiers) != 2) {
    stop("Nombre de millésimes inattendu (", length(fichiers), " trouvés, 2 attendus) dans : ", chemin_dept)
  }
  
  annees   <- str_extract(fichiers, "(?<=D\\d{2,3}-)\\d{4}")
  ordre    <- order(annees)
  fichiers <- fichiers[ordre]
  annees   <- annees[ordre]
  
  return(list(
    millesime_A = fichiers[1],
    millesime_B = fichiers[2],
    annee_A     = annees[1],
    annee_B     = annees[2]
  ))
}


# -----------------------------------------------------------------------------
# trouver_diff_ign()
# Trouve automatiquement le fichier différentiel IGN dans un dossier département
#
# Argument : chemin_dept (character) — chemin vers le dossier du département
# Retourne : chemin complet vers le fichier DIFF .gpkg
# -----------------------------------------------------------------------------

trouver_diff_ign <- function(chemin_dept) {
  fichiers <- list.files(
    chemin_dept,
    pattern    = "\\.gpkg$",
    recursive  = TRUE,
    full.names = TRUE
  ) |>
    str_subset("DIFF")
  
  if (length(fichiers) == 0) {
    stop("Aucun fichier DIFF trouvé dans : ", chemin_dept)
  }
  
  if (length(fichiers) != 1) {
    stop("Plusieurs fichiers DIFF trouvés dans : ", chemin_dept)
  }
  
  return(fichiers[1])
}


# Convertit code département vers format Admin Express (zéro initial)
code_dept_vers_ade <- function(code_dept) {
  case_when(
    code_dept %in% c("2A", "2B") ~ code_dept,  # Corse — format ADE sans zéro
    nchar(code_dept) == 1        ~ paste0("0", code_dept),
    TRUE                         ~ code_dept
  )
}