# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

### functions ---------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

builtup_f <- function(a) {
  raster <- raster(here(rdir, "urban_5cat_Ahmed_Wal", a))
  raster[raster == -9999] <- NA
  crs(raster) <- st_crs(surv14_coord)$proj4string
  pointsp <- extract(raster, surv14_coord)
  
  buf <- st_buffer(surv14_coord, dist = 1000)
  buf1k_mean <- exact_extract(raster, buf, fun = "mean", weights = "area")
  buf1k_mode <- exact_extract(raster, buf, fun = "mode", weights = "area")
  
  buf <- st_buffer(surv14_coord, dist = 300)
  buf300_mean <- exact_extract(raster, buf, fun = "mean", weights = "area")
  buf300_mode <- exact_extract(raster, buf, fun = "mode", weights = "area")
  res <- data.frame(id = surv14_coord$id, pointsp, buf1k_mean, buf1k_mode, buf300_mean, buf300_mode)

}



### packages -----

loadpackage("here")
loadpackage("rgdal")
loadpackage("sf")
loadpackage("raster")
loadpackage("exactextractr")

source(here("scripts", "general_functions.R"))




## 1.2 load data --------

### data folder ---------
rdir <- "data/raw"
pdir <- "data/processed"

### survey coordinates ----------

surv14_coord <-
  st_read(
    here(
      pdir,
      "utilities_survey_Aquawal_CEHD_Wal/addresses/coordinates/surv14_coordinates/surv14_coordinates.shp"
    )
  )

### built-up density ------------

file.ls <-
  list.files(path = here(rdir, "urban_5cat_Ahmed_Wal"),
             pattern = ".flt")

names(file.ls) <- gsub(".flt", "", file.ls)

# 2. load built-up and merge with survey --------

builtup_ls <- lapply(file.ls, builtup_f)



# 3. save data --------

save(
  builtup_ls,
  file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_builtup.Rdata"
  )
)
