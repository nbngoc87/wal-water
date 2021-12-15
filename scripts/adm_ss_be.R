# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

### new functions ------
loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}


loadpackage("here")
loadpackage("rgdal")
loadpackage("sf")
loadpackage("dplyr")
loadpackage("stringi")

### packages ---------

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"


### statistical sectors


stst_rg <-
  st_read(here(
    rdir,
    "admin_border_Statbel_Be/sh_statbel_statistical_sectors.geojson"
  ))

# 2. Walloon statistical sectors -------------

wal_stst <-
  stst_rg[stst_rg$CD_RGN_REFNIS %in% 3000, c(2, 3, 5:7, 9, 12, 14, 17, 19)]

rm(stst_rg)


colnames(wal_stst)[2:10] <-
  c(
    "ststcd",
    "ststnm",
    "smuncd",
    "municd",
    "muninm",
    "dstrcd",
    "dstrnm",
    "prvncd",
    "prvnnm"
  ) # rename some columns # e.g muninm = municipalities name


wal_stst$ststnm <- stri_trans_general(wal_stst$ststnm, "Latin-ASCII")

wal_stst$muninm <-
  stri_trans_general(wal_stst$muninm, "Latin-ASCII; title")
wal_stst$muninm <- gsub("\\s\\(.*\\)", "", wal_stst$muninm)

wal_stst$dstrnm <- stri_trans_general(wal_stst$dstrnm, "Latin-ASCII")
wal_stst$dstrnm <- gsub("Arrondissement d'", "", wal_stst$dstrnm)
wal_stst$dstrnm <- gsub("Arrondissement de ", "", wal_stst$dstrnm)

wal_stst$prvnnm <-
  gsub("Province de ",
       "",
       stri_trans_general(wal_stst$prvnnm, "Latin-ASCII"))

# 3. save output -----------

save(wal_stst, file = here(pdir, "admin_border_Wal/admin_ss_Wal.Rdata"))
