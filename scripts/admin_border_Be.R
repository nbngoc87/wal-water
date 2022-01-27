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
loadpackage("ggplot2")

### packages ---------

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"

### world -----------

world <- 
  st_read(
    here(
      rdir,
      "admin_border_Eurostat_World/CNTR_RG_01M_2016_3035.shp/CNTR_RG_01M_2016_3035.shp"
    )
  )

### statistical sectors -----------


stst <-
  st_read(here(
    rdir,
    "admin_border_Statbel_Be/sh_statbel_statistical_sectors.geojson"
  ))

### submunicipality ---------

smun <-
  st_read(here(
    rdir,
    "admin_border_Statbel_Be/sh_statbel_sub_municipalities.geojson"
  )) 

### municipality --------------

muni <-
  st_read(here(
    rdir,
    "admin_border_Statbel_Be/sh_statbel_municipalities.geojson"
  )) 

### district --------------

dstr <-
  st_read(here(
    rdir,
    "admin_border_Statbel_Be/sh_statbel_districts.geojson"
  )) 

### province --------------

prvn <-
  st_read(here(
    rdir,
    "admin_border_Statbel_Be/sh_statbel_province.geojson"
  )) 

# 2. processing ----------
## 2.1. Statistical sectors -------------

stst <-
  stst[, c(2, 3, 5:7, 11, 12, 16, 17, 21, 22, 26)]


colnames(stst)[2:12] <-
  c(
    "ststcd",
    "ststnm",
    "smuncd",
    "municd",
    "muninm",
    "dstrcd",
    "dstrnm",
    "prvncd",
    "prvnnm",
    "regicd",
    "reginm"
  ) # rename some columns # e.g muninm = municipalities name

stst$ststnm <- stri_trans_general(stst$ststnm, "Latin-ASCII")

stst$muninm <-
  stri_trans_general(stst$muninm, "Latin-ASCII; title")
stst$muninm <- gsub("\\s\\(.*\\)", "", stst$muninm)

stst$dstrnm <- stri_trans_general(stst$dstrnm, "Latin-ASCII")
stst$dstrnm <- gsub("District of ", "", stst$dstrnm)
stst$dstrnm <- gsub("d'", "", stst$dstrnm)


stst$prvnnm <-
  gsub("Province of ",
       "",
       stri_trans_general(stst$prvnnm, "Latin-ASCII"))


## 2.2. sub-mun -------------------

smun <- smun[, 1]
colnames(smun)[1] <- "smuncd"

smun_name <- st_drop_geometry(stst)[,-(1:3)]
smun_name <- smun_name[!duplicated(smun_name),]

smun <- full_join(smun, smun_name)

## 2.3. muni -------------------

muni <- muni[, 1]
colnames(muni)[1] <- "municd"

muni_name <- st_drop_geometry(stst)[,-(1:4)]
muni_name <- muni_name[!duplicated(muni_name),]

muni <- full_join(muni, muni_name)


## 2.4. district -------------------

dstr <- dstr[, 1]
colnames(dstr)[1] <- "dstrcd"

dstr_name <- st_drop_geometry(stst)[,-(1:6)]
dstr_name <- dstr_name[!duplicated(dstr_name),]

dstr <- full_join(dstr, dstr_name)

## 2.5. province -------------------

prvn <- prvn[, 1]
colnames(prvn)[1] <- "prvncd"

prvn_name <- st_drop_geometry(stst)[,-(1:8)]
prvn_name <- prvn_name[!duplicated(prvn_name),]

prvn <- full_join(prvn, prvn_name)

## 2.6. neighbors --------------------

world <- st_transform(world, crs = st_crs(stst))

bbox <- st_bbox(stst[stst$regicd %in% 3000,])
bbox["xmin"] <- bbox["xmin"] - 5000
bbox["xmax"] <- bbox["xmax"] + 5000
bbox["ymin"] <- bbox["ymin"] - 15000
bbox["ymax"] <- bbox["ymax"] + 15000

neighbor <- st_crop(world, bbox)

neighbor <- neighbor[2:6,]


## 2.7. main cities ----------------------

maincity <- muni[muni$muninm %in% c("Brussels", "Mons", "Namur", "Charleroi", "Liege"),]

maincity <- st_centroid(maincity)

ggplot() +
  geom_sf(data = neighbor, fill = c("white", rep("grey", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  geom_sf(data = muni[muni$regicd %in% 3000, ]) +
  geom_sf(data = maincity) +
  geom_sf_text(data = maincity, aes(label = muninm), fontface = "bold", size = 4, nudge_y = -2500)+
  theme_void() 

# 3. save output -----------

save(stst, smun, muni, dstr, prvn, neighbor, maincity, file = here(pdir, "admin_border_Be/admin_Be.Rdata"))
