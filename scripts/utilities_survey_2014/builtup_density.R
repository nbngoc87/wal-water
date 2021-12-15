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
  crs(raster) <- st_crs(surv14_coord)
  builtup <- extract(raster, surv14_coord)
}

### packages -----

loadpackage("here")
loadpackage("rgdal")
loadpackage("sf")
loadpackage("raster")

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

builtup <- do.call(cbind.data.frame, builtup_ls)

builtup <- cbind.data.frame(id = surv14_coord$id, builtup)

# 3. save data --------

write.csv(
  builtup,
  file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_builtup.csv"
  ),
  row.names = F
)
