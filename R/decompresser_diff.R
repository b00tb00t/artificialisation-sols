# =============================================================================
# decompresser_diff.R — Décompression et placement des fichiers DIFF
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")

library(archive)
library(stringr)
library(purrr)

# Trouver tous les .7z DIFF dans data/raw/ocsge/
archives_diff <- list.files(
  "data/raw/ocsge",
  pattern    = "^OCS-GE_2-0_DIFF.*\\.7z$",
  full.names = TRUE,
  recursive  = FALSE  # uniquement à la racine
)

message("📦 ", length(archives_diff), " archives DIFF trouvées")

# Fonction de décompression et placement
decompresser_diff <- function(chemin_archive) {
  
  nom_archive <- basename(chemin_archive)
  message("\n🔄 Traitement : ", nom_archive)
  
  # Extraire le code département depuis le nom — ex: D032 → 32
  code_dept_raw <- str_extract(nom_archive, "(?<=_D)\\d{2,3}[AB]|(?<=_D)\\d{2,3}")
  
  if (is.na(code_dept_raw)) {
    message("   ⚠️ Code département introuvable dans : ", nom_archive)
    return(invisible(NULL))
  }
  
  # Supprimer le zéro initial pour correspondre aux noms de dossiers
  # D032 → 32, D006 → 6, D02A → 2A
  code_dept <- str_remove(code_dept_raw, "^0+(?=[1-9A-Z])")
  
  message("   📍 Département détecté : ", code_dept)
  
  # Dossier destination
  dossier_dest <- file.path("data/raw/ocsge", paste0("dep_", code_dept))
  
  if (!dir.exists(dossier_dest)) {
    message("   ⚠️ Dossier inexistant : ", dossier_dest)
    return(invisible(NULL))
  }
  
  # Vérifier si DIFF déjà présent
  diff_existant <- list.files(
    dossier_dest,
    pattern    = "DIFF.*\\.gpkg$",
    recursive  = TRUE
  )
  
  if (length(diff_existant) > 0) {
    message("   ⏭️ DIFF déjà présent — ignoré")
    return(invisible(NULL))
  }
  
  # Décompresser dans le dossier département
  resultat <- tryCatch({
    archive_extract(chemin_archive, dir = dossier_dest)
    TRUE
  }, error = \(e) {
    message("   ❌ Erreur décompression : ", e$message)
    FALSE
  })
  
  if (resultat) {
    # Supprimer l'archive après extraction
    file.remove(chemin_archive)
    message("   ✅ Extrait → ", dossier_dest)
  }
  
  return(invisible(NULL))
}

# Exécution
walk(archives_diff, decompresser_diff)

message("\n🎉 Décompression terminée")

# Vérification finale
message("\n📊 Vérification — départements avec DIFF :")
depts_avec_diff <- dpt_pilotes[map_lgl(dpt_pilotes, \(dept) {
  length(list.files(
    file.path("data/raw/ocsge", paste0("dep_", dept)),
    pattern   = "DIFF.*\\.gpkg$",
    recursive = TRUE
  )) > 0
})]

message(length(depts_avec_diff), "/", length(dpt_pilotes), " départements ont un DIFF")