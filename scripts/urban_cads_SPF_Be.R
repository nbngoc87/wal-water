# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------
### functions -----

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}
### packages --------

loadpackage("here")
loadpackage("rgdal")
loadpackage("sf")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"

### parcel data ---------------

system.time(parcel <-
              st_read(here(rdir, "urban_cads_SPF_Be/Bpn_CaPa.shp")))

save(parcel, file =  here(pdir, "urban_cads_SPF_Be/parcel_Be.Rdata"))

### building data ---------------

system.time(buildings <-
              st_read(here(rdir, "urban_cads_SPF_Be/Bpn_CaBu.shp")))

save(buildings,
     file =  here(pdir, "urban_cads_SPF_Be/buildings_Be.Rdata"))
