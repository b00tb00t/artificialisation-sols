# =============================================================================
# telecharger_ocsge.R — Téléchargement automatique des données OCS GE
# Tente chaque année de 2016 à 2023 par département
# S'arrête après 2 millésimes trouvés
# =============================================================================

source("R/config.R")

library(httr)
library(archive)
library(stringr)
library(purrr)
library(readr)
library(dplyr)

# =============================================================================
# === PARAMÈTRES ===
# =============================================================================

# Liste complète des départements métropole
depts_numeriques <- as.character(c(1:19, 21:95))
tous_depts       <- c(depts_numeriques, "2A", "2B")

# Années à tester
annees_a_tester  <- 2016:2023

# URL pattern
url_pattern <- paste0(
  "https://data.geopf.fr/telechargement/download/OCSGE/",
  "OCS-GE_2-0__GPKG_LAMB93_D{dept}_{annee}-01-01/",
  "OCS-GE_2-0__GPKG_LAMB93_D{dept}_{annee}-01-01.7z"
)

# Log téléchargements
log_dl_path <- "data/outputs/log_telechargements.csv"

log_dl <- tibble(
  code_dept = character(),
  annee     = integer(),
  statut    = character(),
  fichier   = character(),
  timestamp = character()
)

if (file.exists(log_dl_path)) {
  log_dl <- read_csv(log_dl_path, 
                     show_col_types = FALSE,
                     col_types = cols(
                       code_dept = col_character(),
                       annee     = col_integer(),
                       statut    = col_character(),
                       fichier   = col_character(),
                       timestamp = col_character()
                     ))
}

logger_dl <- function(code_dept, annee, statut, fichier = "") {
  log_dl <<- bind_rows(log_dl, tibble(
    code_dept = as.character(code_dept),
    annee     = as.integer(annee),
    statut    = as.character(statut),
    fichier   = as.character(fichier),
    timestamp = as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ))
  write_csv(log_dl, log_dl_path)
}

# =============================================================================
# === FONCTION TÉLÉCHARGEMENT D'UN DÉPARTEMENT ===
# =============================================================================

telecharger_departement <- function(code_dept) {
  
  message("\n🔄 Département : ", code_dept)
  
  # Dossier destination
  dossier_dest <- file.path(chemin_ocsge, paste0("dep_", code_dept))
  dir.create(dossier_dest, recursive = TRUE, showWarnings = FALSE)
  
  # Vérifier si déjà téléchargé (2 millésimes présents)
  gpkg_existants <- list.files(
    dossier_dest,
    pattern    = "OCCUPATION_SOL\\.gpkg$",
    recursive  = TRUE,
    full.names = FALSE
  )
  
  if (length(gpkg_existants) >= 2) {
    message("   ⏭️ Déjà téléchargé — ", length(gpkg_existants), " millésimes présents")
    return(invisible(NULL))
  }
  
  # Tenter chaque année
  millesimes_trouves <- c()
  
  for (annee in annees_a_tester) {
    
    url <- str_glue(url_pattern, dept = code_dept, annee = annee)
    
    # Vérifier si l'URL existe (HEAD request — léger)
    reponse <- tryCatch(
      HEAD(url, timeout(10)),
      error = \(e) NULL
    )
    
    if (is.null(reponse) || status_code(reponse) != 200) {
      message("   ⏭️ ", annee, " — non disponible")
      next
    }
    
    # Télécharger le fichier
    fichier_7z <- file.path(
      dossier_dest,
      str_glue("OCS-GE_2-0__GPKG_LAMB93_D{code_dept}_{annee}-01-01.7z")
    )
    
    message("   📥 Téléchargement ", annee, "...")
    
    resultat_dl <- tryCatch({
      GET(url, write_disk(fichier_7z, overwrite = TRUE), timeout(300),
          progress())
      TRUE
    }, error = \(e) {
      message("   ❌ Erreur téléchargement : ", e$message)
      FALSE
    })
    
    if (!resultat_dl) {
      logger_dl(code_dept, annee, "échec_téléchargement")
      next
    }
    
    # Décompresser
    message("   📦 Décompression ", annee, "...")
    
    resultat_decomp <- tryCatch({
      archive_extract(fichier_7z, dir = dossier_dest)
      file.remove(fichier_7z)  # supprimer le .7z après extraction
      TRUE
    }, error = \(e) {
      message("   ❌ Erreur décompression : ", e$message)
      FALSE
    })
    
    if (!resultat_decomp) {
      logger_dl(code_dept, annee, "échec_décompression", fichier_7z)
      next
    }
    
    logger_dl(code_dept, annee, "succès", fichier_7z)
    millesimes_trouves <- c(millesimes_trouves, annee)
    message("   ✅ Millésime ", annee, " téléchargé et extrait")
    
    # Arrêter après 2 millésimes
    if (length(millesimes_trouves) == 2) {
      message("   🎯 2 millésimes trouvés — passage au département suivant")
      break
    }
  }
  
  if (length(millesimes_trouves) == 0) {
    message("   ⚠️ Aucun millésime trouvé pour le département ", code_dept)
    logger_dl(code_dept, NA, "aucun_millésime")
  }
  
  return(invisible(NULL))
}

# =============================================================================
# === EXÉCUTION ===
# =============================================================================

# Pour les pilotes uniquement — tester avant de lancer le national
# walk(dpt_pilotes, telecharger_departement)

# Pour tous les départements — décommenter quand prêt
walk(tous_depts, telecharger_departement)

message("\n🎉 Téléchargements terminés")
message("📋 Log → ", log_dl_path)