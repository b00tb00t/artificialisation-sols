# =============================================================================
# telecharger_ocsge.R — Téléchargement automatique des données OCS GE
# Tente chaque année de 2016 à 2023 par département
# S'arrête après 2 millésimes trouvés
# Télécharge également les fichiers différentiels DIFF
# =============================================================================

source("R/config.R")
source("R/00_fonctions.R")

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

# Années à tester pour les millésimes
annees_a_tester <- 2016:2023

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
# === FONCTION TÉLÉCHARGEMENT MILLÉSIMES ===
# =============================================================================

telecharger_departement <- function(code_dept) {
  
  message("\n🔄 Département : ", code_dept)
  
  dossier_dest <- file.path("data/raw/ocsge", paste0("dep_", code_dept))
  dir.create(dossier_dest, recursive = TRUE, showWarnings = FALSE)
  
  # Vérifier si déjà téléchargé
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
  
  # Formater le code département sur 3 caractères pour l'URL
  dept_url <- str_pad(code_dept, 3, "left", "0")
  
  millesimes_trouves <- c()
  
  for (annee in annees_a_tester) {
    
    url <- str_glue(
      "https://data.geopf.fr/telechargement/download/OCSGE/",
      "OCS-GE_2-0__GPKG_LAMB93_D{dept_url}_{annee}-01-01/",
      "OCS-GE_2-0__GPKG_LAMB93_D{dept_url}_{annee}-01-01.7z"
    )
    
    reponse <- tryCatch(HEAD(url, timeout(10)), error = \(e) NULL)
    
    if (is.null(reponse) || status_code(reponse) != 200) {
      message("   ⏭️ ", annee, " — non disponible")
      next
    }
    
    fichier_7z <- file.path(
      dossier_dest,
      str_glue("OCS-GE_2-0__GPKG_LAMB93_D{dept_url}_{annee}-01-01.7z")
    )
    
    message("   📥 Téléchargement ", annee, "...")
    
    resultat_dl <- tryCatch({
      GET(url, write_disk(fichier_7z, overwrite = TRUE), timeout(300), progress())
      TRUE
    }, error = \(e) {
      message("   ❌ Erreur téléchargement : ", e$message)
      FALSE
    })
    
    if (!resultat_dl) {
      logger_dl(code_dept, annee, "échec_téléchargement")
      next
    }
    
    message("   📦 Décompression ", annee, "...")
    
    resultat_decomp <- tryCatch({
      archive_extract(fichier_7z, dir = dossier_dest)
      file.remove(fichier_7z)
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
# === FONCTION TÉLÉCHARGEMENT DIFF ===
# =============================================================================

telecharger_diff_departement <- function(code_dept) {
  
  message("📥 Recherche DIFF pour département : ", code_dept)
  
  chemin_dept <- file.path(chemin_ocsge, paste0("dep_", code_dept))
  millesimes  <- trouver_millesimes(chemin_dept)
  annee_A     <- millesimes$annee_A
  annee_B     <- millesimes$annee_B
  
  # Vérifier si déjà téléchargé
  diff_existant <- list.files(
    chemin_dept,
    pattern    = "DIFF.*\\.gpkg$",
    recursive  = TRUE,
    full.names = TRUE
  )
  
  if (length(diff_existant) > 0) {
    message("   ⏭️ DIFF déjà présent")
    return(invisible(NULL))
  }
  
  # Formater le code département sur 3 caractères pour l'URL
  dept_url <- str_pad(code_dept, 3, "left", "0")
  
  # Dates de publication à tester
  dates_a_tester <- c(
    "2024-01-31", "2024-02-28", "2024-03-31", "2024-04-30",
    "2024-05-31", "2024-06-30", "2024-07-31", "2024-08-31",
    "2024-09-30", "2024-10-31", "2024-11-30", "2024-12-31",
    "2025-01-31", "2025-02-28", "2025-03-31", "2025-04-30",
    "2025-05-31", "2025-06-30"
  )
  
  url_trouvee <- NULL
  
  for (date_pub in dates_a_tester) {
    
    nom_fichier <- str_glue(
      "OCS-GE_2-0_DIFF-{annee_A}-{annee_B}_GPKG_LAMB93_D{dept_url}_{date_pub}"
    )
    url <- str_glue(
      "https://data.geopf.fr/telechargement/download/OCSGE/{nom_fichier}/{nom_fichier}.7z"
    )
    
    Sys.sleep(0.5)  # ← éviter le rate limiting IGN
    
    reponse <- tryCatch(
      HEAD(url, timeout(10)),
      error = \(e) NULL
    )
    
    if (!is.null(reponse) && status_code(reponse) == 200) {
      message("   ✅ URL trouvée : ", date_pub)
      url_trouvee <- url
      nom_trouve  <- nom_fichier
      break
    }
  }
  
  if (is.null(url_trouvee)) {
    message("   ⚠️ DIFF introuvable pour dep", code_dept,
            " (millésimes ", annee_A, "-", annee_B, ")")
    return(invisible(NULL))
  }
  
  # Télécharger
  fichier_7z <- file.path(chemin_dept, paste0(nom_trouve, ".7z"))
  
  message("   📥 Téléchargement DIFF...")
  GET(url_trouvee, write_disk(fichier_7z, overwrite = TRUE),
      timeout(300), progress())
  
  message("   📦 Décompression DIFF...")
  archive::archive_extract(fichier_7z, dir = chemin_dept)
  file.remove(fichier_7z)
  
  message("   ✅ DIFF téléchargé — dep", code_dept)
  return(invisible(NULL))
}

# =============================================================================
# === EXÉCUTION ===
# =============================================================================

# Test millésimes sur un département
# telecharger_departement("1")

# Test DIFF sur un département
# telecharger_diff_departement("3")

# National — millésimes
# walk(tous_depts, telecharger_departement)

# National — DIFF
#walk(tous_depts, telecharger_diff_departement)