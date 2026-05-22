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