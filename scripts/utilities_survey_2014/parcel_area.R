# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

source('3 Scripts/general_functions.R')


loadpackage('rgdal')
loadpackage('sf')
loadpackage('dplyr')



## 1.2 load data --------

### data folder
rdir <- '2 Data/1 Raw'
pdir <- '2 Data/2 Processed'

### survey data sf -----------


load(file = file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/surv14_coord_PICC.Rdata'))



### parcel data ---------------

# system.time(parcel <- st_read(file.path(rdir, 'Urban_cads_FiscSit_Belgium/Bpn_CaPa.shp')))

# save(parcel, file =  file.path(pdir, 'Urban_cads_FiscSit_Belgium/parcel_BE.Rdata'))

system.time(load(file =  file.path(pdir, 'Urban_cads_FiscSit_Belgium/parcel_BE.Rdata')))

### building data ---------------

# system.time(buildings <- st_read(file.path(rdir, 'Urban_cads_FiscSit_Belgium/Bpn_CaBu.shp')))
# 
# save(buildings, file =  file.path(pdir, 'Urban_cads_FiscSit_Belgium/buildings_BE.Rdata'))

system.time(load(file =  file.path(pdir, 'Urban_cads_FiscSit_Belgium/buildings_BE.Rdata')))

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

res <- data.frame(id = integer(), parcarea = numeric(), bldarea = numeric())


system.time({
for (i in 1:5) {
  for (j in 1:3) {
   
    xmin_tmp <- as.numeric(xmin + (i-1)*50000)
    xmax_tmp <- as.numeric(xmin_tmp + 50000)
    ymin_tmp <- as.numeric(ymin + (j-1)*50000)
    ymax_tmp <- as.numeric(ymin_tmp + 50000)


    sub_surv14 <- st_crop(surv14_coord_tf, xmin = xmin_tmp, ymin = ymin_tmp, xmax = xmax_tmp, ymax = ymax_tmp)
    if(nrow(sub_surv14) > 0) {
      sub_parc <- st_crop(parcel, xmin = (xmin_tmp - 100), ymin = (ymin_tmp - 100), xmax = (xmax_tmp + 100), ymax = (ymax_tmp + 100))
      sub_bld <- st_crop(buildings, xmin = (xmin_tmp - 100), ymin = (ymin_tmp - 100), xmax = (xmax_tmp + 100), ymax = (ymax_tmp + 100))
      mat <- st_intersects(sub_surv14, sub_parc, sparse = F)
      for (k in 1:nrow(sub_surv14)) {
        slt_parc <- sub_parc[mat[k,], ]
        sub_surv14[k, 'parcarea'] <- slt_parc$Shape_area
        slt <- st_intersection(sub_bld, slt_parc)
        slt$area <- st_area(slt)
        sub_surv14[k, 'bldarea'] <- as.numeric(sum(slt$area, na.rm = T))
      }
      
      res <- rbind(res, as.data.frame(sub_surv14[, c('id', 'parcarea','bldarea')]))
    }
  }
}
})

# 3. save results ------------


res <- res[match(surv14_coords$id, res$id),]

write.csv(as.data.frame(res[,1:3]), file= file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_parcel.csv'), row.names = F)

# surv14_coords <- cbind(surv14_coords, as.data.frame(res[, c('bldarea', 'parcarea')]))
# save(surv14_coords, file= file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_parcelarea.Rdata'))
