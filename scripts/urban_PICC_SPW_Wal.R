# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

### new functions ----------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

### packages -----------

loadpackage("here")
loadpackage("rgdal")
loadpackage("sf")
loadpackage("stringi")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder ------
rdir <- "data/raw"
pdir <- "data/processed"
### PICC data -----------


ads_point_liege <-
  st_read(here(rdir, "urban_PICC_SPW_Liege", "ADRESS_POINT.shp"))

ads_point_luxembourg <-
  st_read(here(rdir, "urban_PICC_SPW_Luxembourg", "ADRESS_POINT.shp"))

ads_point_walbrabant <-
  st_read(here(rdir, "urban_PICC_SPW_WalloonBrabant", "ADRESS_POINT.shp"))

ads_point_hainaut <-
  st_read(here(rdir, "urban_PICC_SPW_Hainaut", "ADRESS_POINT.shp"))

ads_point_namur <-
  st_read(here(rdir, "urban_PICC_SPW_Namur", "ADRESS_POINT.shp"))

crs_picc <- st_crs(ads_point_walbrabant)


# 2.  format PICC ads ---------------

obs_ls <- grep("ads_point", names(.GlobalEnv), value = TRUE)

for (a in obs_ls) {
  tmp <- get(a, envir = .GlobalEnv)
  tmp <- tmp[, c(1, 7:15, 17)]
  colnames(tmp)[4:10] <-
    c("munitx", "municd", "pc", "zone", "street", "strcd", "nb")
  tmp$munitx <- stri_trans_general(tmp$munitx, "Latin-ASCII; upper")
  tmp$zone <- stri_trans_general(tmp$zone, "Latin-ASCII; upper")
  tmp$street <- stri_trans_general(tmp$street, "Latin-ASCII; upper")
  tmp$street <- gsub(" \\(.*", "", tmp$street)
  tmp$street <- gsub("-", " ", tmp$street)
  assign(a, tmp, envir = .GlobalEnv)
}


ads_point_wal <- do.call(rbind, mget(obs_ls, envir = .GlobalEnv))



# 3. save outputs -----------------

save(ads_point_wal,
     file = here(pdir, "urban_PICC_SPW_Wal/urban_PICC_SPW_Wal.Rdata"))
