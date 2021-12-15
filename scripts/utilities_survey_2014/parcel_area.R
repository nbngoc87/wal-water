# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

### functions ------
loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}
### packages ----------

loadpackage("here")
loadpackage("rgdal")
loadpackage("sf")
loadpackage("dplyr")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"

### survey data sf -----------


load(
  file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/addresses/coordinates/surv14_coord_PICC.Rdata"
  )
)



### parcel data ---------------


system.time(load(file =  here(
  pdir, "urban_cads_SPF_Be/parcel_Be.Rdata"
)))

### building data ---------------


system.time(load(file =  here(
  pdir, "urban_cads_SPF_Be/buildings_Be.Rdata"
)))

# 2. Parcel & build area ----------------



surv14_coord_tf <- st_transform(surv14_coords, st_crs(parcel))

bbox_surv <- st_bbox(surv14_coord_tf)

xmin <- floor(bbox_surv[1])
xmax <- ceiling(bbox_surv[3])
ymin <- floor(bbox_surv[2])
ymax <- ceiling(bbox_surv[4])

# ceiling((xmax - xmin)/10000)
# ceiling((ymax - ymin)/10000)
# ceiling((xmax - xmin)/50000)
# ceiling((ymax - ymin)/50000)

res <-
  data.frame(id = integer(),
             parcarea = numeric(),
             bldarea = numeric())


system.time({
  for (i in 1:5) {
    for (j in 1:3) {
      xmin_tmp <- as.numeric(xmin + (i - 1) * 50000)
      xmax_tmp <- as.numeric(xmin_tmp + 50000)
      ymin_tmp <- as.numeric(ymin + (j - 1) * 50000)
      ymax_tmp <- as.numeric(ymin_tmp + 50000)
      
      
      sub_surv14 <-
        st_crop(
          surv14_coord_tf,
          xmin = xmin_tmp,
          ymin = ymin_tmp,
          xmax = xmax_tmp,
          ymax = ymax_tmp
        )
      if (nrow(sub_surv14) > 0) {
        sub_parc <-
          st_crop(
            parcel,
            xmin = (xmin_tmp - 100),
            ymin = (ymin_tmp - 100),
            xmax = (xmax_tmp + 100),
            ymax = (ymax_tmp + 100)
          )
        sub_bld <-
          st_crop(
            buildings,
            xmin = (xmin_tmp - 100),
            ymin = (ymin_tmp - 100),
            xmax = (xmax_tmp + 100),
            ymax = (ymax_tmp + 100)
          )
        mat <- st_intersects(sub_surv14, sub_parc, sparse = F)
        for (k in 1:nrow(sub_surv14)) {
          slt_parc <- sub_parc[mat[k, ],]
          sub_surv14[k, "parcarea"] <- slt_parc$Shape_area
          slt <- st_intersection(sub_bld, slt_parc)
          slt$area <- st_area(slt)
          sub_surv14[k, "bldarea"] <-
            as.numeric(sum(slt$area, na.rm = T))
        }
        
        res <-
          rbind(res, as.data.frame(sub_surv14[, c("id", "parcarea", "bldarea")]))
      }
    }
  }
})

# 3. save results ------------


res <- res[match(surv14_coords$id, res$id), ]

write.csv(
  as.data.frame(res[, 1:3]),
  file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_parcel.csv"
  ),
  row.names = F
)
