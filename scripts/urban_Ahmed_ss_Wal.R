# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

### functions ---------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}


bltupss_f <- function(a = file_ls[[1]]) {
  raster <- raster(here(rdir, "urban_5cat_Ahmed_Wal", a))
  raster[raster == -9999] <- NA
  crs(raster) <- st_crs(stst_wal)$proj4string
  
  ss_mode <- exact_extract(raster, stst_wal, fun = "mode", weights = "area")
  # ss_mean <- exact_extract(raster, stst_wal, fun = "mean", weights = "area")
  # ss_max <- exact_extract(raster, stst_wal, fun = "max")
  # 
  # ggplot(data = neighbor) +
  #   geom_sf(fill = c("white", rep("grey90", 4))) +
  #   geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  #   geom_sf(data = stst_wal, aes(fill = ss_mean)) +
  #   scale_fill_scico(palette = "oslo", direction = -1, na.value = "grey90")
  # ggplot() +
  #   geom_histogram(aes(x=ss_max, fill = "ss_max")) +
  #   geom_histogram(aes(x = ss_mean, fill = "ss_mean"))+
  #   geom_histogram(aes(x = ss_mode, fill = "ss_mode"))
  # 
  # ggplot() +
  #   geom_histogram(aes(x = values(raster)))
} 

### packages -----

loadpackage("here")
loadpackage("sf")
loadpackage("raster")
loadpackage("exactextractr")
loadpackage("ggspatial")
loadpackage("ggplot2")

source(here("scripts", "general_functions.R"))




## 1.2 load data --------

### data folder ---------
rdir <- "data/raw"
pdir <- "data/processed"

### administrative borders -----------

load(file = here(
  pdir,
  "admin_border_Be/admin_Be.Rdata"
))

stst_wal <- stst[stst$regicd %in% 3000, ]

### built-up density ------------

file_ls <-
  list.files(path = here(rdir, "urban_5cat_Ahmed_Wal"),
             pattern = ".flt")

names(file_ls) <- gsub(".flt", "", file_ls)

# 2. calculate mode of builtup dens--------


bltupss_ls <- lapply(file_ls, bltupss_f)

bltupss <- data.frame(ststcd = stst_wal$ststcd, bltupss_ls)

ggplot(data = neighbor) +
  geom_sf(fill = c("white", rep("grey90", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  geom_sf(data = stst_wal, aes(fill = bltupss$simul_2020_BLR_23_f)) +
  scale_fill_scico(palette = "oslo", direction = -1, na.value = "grey90")


# 3. save data ---------------------

save(
  bltupss,
  file = here(
    pdir,
    "urban_5cat_Ahmed_Wal/urban_5cat_ss_Wal.Rdata"
  )
)


