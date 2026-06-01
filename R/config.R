# =============================================================================
# config.R — Constantes et paramètres du projet
# À sourcer en début de chaque script : source("R/config.R")
# =============================================================================

# === CHEMINS FICHIERS SOURCES ===

chemin_ade    <- "data/raw/admin_express/ADE-COG_4-0_GPKG_LAMB93_FXX-ED2026-01-01.gpkg"
chemin_ocsge  <- "data/raw/ocsge"

# Départements pilotes — temporaire
chemin_ocsge_dep_34 <- "data/raw/ocsge/dep_34"
chemin_ocsge_dep_33 <- "data/raw/ocsge/dep_33"

# INSEE
chemin_insee          <- "data/raw/insee/DS_POPULATIONS_HISTORIQUES_data.csv"
chemin_insee_metadata <- "data/raw/insee/DS_POPULATIONS_HISTORIQUES_metadata.csv"


# === CODES CS ===

# Artificialisation — tout passage VERS ces classes
terrain_artificiel <- c("CS1.1.1.1", "CS1.1.1.2", "CS1.1.2.1", "CS1.1.2.2")

# Renaturation — tout passage DEPUIS CS1.1.x VERS ces classes
terrain_nature <- c("CS1.2.2",   "CS2.1.1.1", "CS2.1.1.2", "CS2.1.1.3",
                    "CS2.1.2",   "CS2.1.3",   "CS2.2.1",   "CS2.2.2")

# Codes CS — forêts et herbacés (pour analyse 5b)
cs_forets <- c("CS2.1.1.1", "CS2.1.1.2", "CS2.1.1.3")
cs_herbaces <- c("CS2.2.1")
cs_nature_5b <- c(cs_forets, cs_herbaces)


# === PARAMÈTRES D'ANALYSE ===

# Départements pilotes
dpt_pilotes <- c("33", "34")

# CRS de travail — Lambert-93
crs_travail <- 2154

# Cas particuliers — territoires sans canton dans Admin Express
code_dept_paris          <- "75"
code_pseudo_canton_lyon  <- "9999"

# Codes cantons créés pour notre couche canton complète
code_canton_paris <- "7500"
code_canton_lyon  <- "6999"

# Label pour polygones sans correspondance dans le différentiel IGN
label_disparu <- "fusionné/redécoupé"

# === COLONNES CLÉS ===

# OCS GE
ocsge_col_cs        <- "code_cs"
ocsge_col_us        <- "code_us"
ocsge_col_millesime <- "millesime"

# Admin Express — canton
ade_canton_col_code <- "code_insee"
ade_canton_col_nom  <- "nom_officiel"
ade_canton_col_dept <- "code_insee_du_departement"

# Admin Express — commune
ade_commune_col_code   <- "code_insee"
ade_commune_col_canton <- "code_insee_du_canton"
ade_commune_col_dept   <- "code_insee_du_departement"