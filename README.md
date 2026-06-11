---
type: 'Page'
title: readme
aliases: null
description: null
icon: null
createdAt: '2026-06-11T08:24:14.921Z'
lastUpdated: '2026-06-11T08:43:19.471Z'
tags: []
coverImage: null
---

# Artificialisation des Sols en France

Analyse de l'évolution de l'artificialisation et de la renaturation des sols en France métropolitaine à l'échelle des cantons, à partir des données OCS GE de l'IGN.

> **Projet d'entraînement R** — GIS, cartographie thématique, publication Quarto. Cette analyse n'a pas vocation à constituer une étude officielle.

---

## Structure des dossiers

```text
artificialisation-sols/
│
├── data/
│   ├── raw/                        # Données brutes — non versionnées (~100 Go)
│   │   ├── ocsge/
│   │   │   └── dep_{X}/            # Un dossier par département
│   │   │       ├── OCCUPATION_SOL_{annee_A}.gpkg
│   │   │       ├── OCCUPATION_SOL_{annee_B}.gpkg
│   │   │       └── DIFF_D{X}_{annee_B}_{annee_A}.gpkg
│   │   ├── admin_express/
│   │   │   └── admin_express_2026.gpkg
│   │   └── insee/
│   │       └── DS_POPULATIONS_HISTORIQUES_data.csv
│   │
│   ├── processed/                  # Données traitées — non versionnées (~110 Go)
│   │   ├── cantons_complets/       # Couche canton enrichie (Paris + Lyon)
│   │   ├── intersections/          # OCS GE découpé par canton (par département)
│   │   ├── diff/                   # Différentiels CS par canton (par département)
│   │   └── agregats/               # Indicateurs finaux et analyses CS×US
│   │
│   └── outputs/                    # Résultats — non versionnés
│       ├── cartes/                 # Cartes HTML interactives
│       ├── graphiques/             # Graphiques HTML interactifs (national/régional/départemental)
│       ├── log_etl.csv             # Journal d'exécution ETL
│       └── log_telechargements.csv # Journal de téléchargement OCS GE
│
├── R/                              # Scripts d'analyse
│   ├── config.R
│   ├── 00_fonctions.R
│   ├── 02_cantons_complets.R
│   ├── 03_masking.R
│   ├── 04_diff.R
│   ├── 05_agregation.R
│   ├── 05b_population.R
│   ├── 06_cs_us.R
│   ├── 07_cartes.R
│   ├── 07b_cartes_nonlog.R
│   ├── 08_graphiques.R
│   ├── run_all.R
│   ├── telecharger_ocsge.R
│   └── decompresser_diff.R
│
├── docs/                           # Site Quarto compilé → GitHub Pages
├── _quarto.yml                     # Configuration du site
├── index.qmd
├── methodologie.qmd
├── resultats.qmd
├── apropos.qmd
└── artificialisation_sols.Rproj
```

> **Note** : Les dossiers `data/raw/`, `data/processed/` et `data/outputs/` ne sont pas versionnés (trop volumineux). Seuls les scripts R et les fichiers Quarto sont dans Git. L'espace disque nécessaire pour reproduire l'analyse complète est d'environ **210 Go**.

---

## Exécution des scripts et reproduction de l'analyse

### Prérequis

- R ≥ 4.4

- ~210 Go d'espace disque disponible

### Étape 1 — Téléchargement des données OCS GE

```r
source("R/telecharger_ocsge.R")
```

Télécharge automatiquement les deux millésimes OCS GE pour les 96 départements métropolitains depuis la [page OCS GE de cartes.gouv](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_OCS-GE) . Les fichiers sont décompressés et placés dans `data/raw/ocsge/dep_{X}/`. X étant le numéro de département correspondant. 
​

### Étape 2 — Fichiers différentiels DIFF IGN

Les fichiers DIFF (polygones ayant changé entre les deux millésimes) doivent être téléchargés manuellement depuis la [page OCS GE de cartes.gouv](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_OCS-GE) pour chaque département et placés dans `data/raw/ocsge/`. Le téléchargement a été manuel pour cette première version de l'analyse dû aux spécificités de nomenclature des fichiers qui rendait un téléchargement automatisé plus complexe. Cependant, un script automatisable, plus complexe the telecharger_ocsge pourrait être créé.

Une fois téléchargés :

```r
source("R/decompresser_diff.R")
```

Décompresse et place chaque fichier DIFF dans le sous-dossier département correspondant.
​

### Étape 3 — Analyse complète

```r
source("R/run_all.R")
```

Lance l'intégralité de la chaîne ETL dans l'ordre :

| Ordre | Script                  | Description                                                                                                                                                                     |
| :---- | :---------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| 1     | `config.R`              | Constantes, chemins, codes CS, liste des départements                                                                                                                           |
| 2     | `00_fonctions.R`        | Fonctions réutilisables pour le projet                                                                                                                                          |
| 2.1   | `01_load.R`             | Fonction optionnelle pour une exécution manuelle pour charger les données nécessaires en mémoire et faire un premier test sur des départements pilotes/test.                    |
| 3     | `02_cantons_complets.R` | Construction de la couche canton complète (la couche canton d'Admin Express étant incomplète, il a fallu join d'autres couches pour la métropole de Lyon et la ville de Paris)  |
| 4     | `03_masking.R`          | Intersection OCS GE × cantons par département                                                                                                                                   |
| 5     | `04_diff.R`             | Calcul du différentiel CS par canton (millésime B - millésime A)                                                                                                                |
| 6     | `05_agregation.R`       | Indicateurs finaux (ha gagnés/perdus, % par millésimes)                                                                                                                         |
| 7     | `05b_population.R`      | Agrégation populations INSEE au niveau canton                                                                                                                                   |
| 8     | `06_cs_us.R`            | Analyses croisées couverture × usage                                                                                                                                            |
| 9     | `07_cartes.R`           | Production des cartes interactives                                                                                                                                              |
| 10    | `07b_cartes_nonlog.R`   | Cartes annexes sans transformation logarithmique (optionnel)                                                                                                                    |
| 11    | `08_graphiques.R`       | Production des graphiques d'analyse                                                                                                                                             |

> Les scripts peuvent également être lancés individuellement dans l'ordre numérique depuis RStudio.

Un log d'exécution est généré automatiquement dans `data/outputs/log_etl.csv`.

---

## Dépendances R

### Packages principaux

| Package           | Usage                                                                                                                    |
| :---------------- | :----------------------------------------------------------------------------------------------------------------------- |
| `sf`              | Données spatiales vectorielles : intersection, masking, st_area                                                          |
| `dplyr`           | Manipulation et transformation des données tabulaires                                                                    |
| `tidyr`           | Pivot et restructuration des tableaux                                                                                    |
| `purrr`           | Loop des scripts sur tous les départements                                                                               |
| `stringr`         | Gestion des strings : extraction des années et millésimes des fichiers sources et naming corrects des fichiers processed |
| `tmap` (v4)       | Création des cartes                                                                                                      |
| `ggplot2`         | Création des graphiques                                                                                                  |
| `plotly`          | Conversion des graphiques ggplot2 en interactif                                                                          |
| `DBI` / `RSQLite` | Lecture/écriture des GeoPackages via SQLite                                                                              |

### Packages secondaires

| Package       | Usage                                                 |
| :------------ | :---------------------------------------------------- |
| `classInt`    | Calcul des bornes de discrétisation Jenks             |
| `archive`     | Décompression des archives .7z OCS GE                 |
| `httr`        | Requêtes HTTP pour le téléchargement automatique      |
| `htmlwidgets` | Sauvegarde des widgets plotly en HTML                 |
| `ggalluvial`  | Diagrammes de flux alluviaux                          |
| `readr`       | Lecture des fichiers CSV INSEE                        |
| `lubridate`   | Gestion des timestamp dans les logs                   |
| `here`        | Chemins de fichiers relatifs à la racine du projet    |
| `moments`     | Calcul de la skewness pour le choix de discrétisation |

---

## Données sources

| Source | Produit                 | Millésimes                    | Format     |
| :----- | :---------------------- | :---------------------------- | :--------- |
| IGN    | OCS GE NG v2.0          | 2 par département (variables) | GeoPackage |
| IGN    | Admin Express COG 2026  | 2026                          | GeoPackage |
| INSEE  | Populations historiques | 1968–2023                     | CSV        |

