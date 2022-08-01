# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

### functions ---------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}


bltupss_f <- function(raster = raster_ls[[4]]) {
  
  max <- max(values(raster), na.rm = T)
  
  if (max == 11) {
    
    raster[raster == 0] <- NA
    raster[raster == 1] <- 0
    raster[raster == 2 | raster == 3] <- 1
    raster[raster == 4 | raster == 5] <- 2
    raster[raster == 6 | raster == 7] <- 3
    raster[raster == 8 | raster == 9] <- 4
    raster[raster == 10 | raster == 11] <- 5
    
   
  } else {
    
    raster[raster == -9999] <- NA     
  }

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
loadpackage("scico")
loadpackage("reshape2")
loadpackage("dplyr")
loadpackage("nnet")
loadpackage("doParallel")
loadpackage("MASS")

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
             pattern = ".flt$")

names(file_ls) <- gsub(".flt", "", file_ls)

raster_ls <- lapply(file_ls, function(a) raster(here(rdir, "urban_5cat_Ahmed_Wal", a)))

# 2. calculate mode of builtup dens--------


bltupss_ls <- lapply(raster_ls, bltupss_f)

bltupss <- data.frame(ststcd = stst_wal$ststcd, municd = stst_wal$municd, bltupss_ls)


bltupss <- bltupss[order(bltupss$municd, bltupss$ststcd), ]

ggplot(data = neighbor) +
  geom_sf(fill = c("white", rep("grey90", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  geom_sf(data = stst_wal, aes(fill = bltupss$simul_2100_DH_f), color = NA) +
  scale_fill_scico(palette = "bilbao", direction = 1, na.value = "grey90")

# 3. scenarios from Ahmed --------------

scenarios <- data.frame(simul = colnames(bltupss)[-(1:5)])

scenarios$year <- sapply(scenarios$simul, function(x) as.numeric(unlist(strsplit(x, split = "_"))[2]))


scenarios$snro <- sapply(scenarios$simul, function(x) unlist(strsplit(x, split = "_"))[3])

scenarios$flzr <- sapply(scenarios$simul, function(x) unlist(strsplit(x, split = "_"))[4])
scenarios$flzr[scenarios$flzr == "f"] <- NA

scenarios$snros <- apply(scenarios[, c("snro", "flzr")], 1, function(x) paste(x[!is.na(x)], collapse = "_"))





# 4. multinom to predict for other years -----------

snros <- lvls(scenarios$snros)
municd <- lvls(bltupss$municd)
year <- c(seq(2011, 2019, 1), seq(2020, 2040, 5))

res <- bltupss[!duplicated(bltupss[, 1:2]), 1:2]

res <- res[rep(1:nrow(res), each = length(year)),]
rownames(res) <- 1:nrow(res)
res$year <- rep(year, nrow(bltupss))

# a <- "BL"
# a <- snros[2]


system.time(for (a in snros) {
    df <-
      bltupss[, c(1:5, grep(paste0("_", a, "_"), colnames(bltupss)))]
    colnames(df)[3:5] <- paste("LU", c(1990, 2000, 2010), sep = "_")
    
    colnames(df)[6:ncol(df)] <-
      paste("sim", sapply(colnames(df)[6:ncol(df)], function(x)
        unlist(strsplit(x, split = "_"))[2]), sep = "_")
    
    df_lg <- melt(df, id.vars = c("ststcd", "municd"))
    df_lg$year <- as.numeric(gsub(".*_", "", df_lg$variable))
    
    df_lg <- df_lg[order(df_lg$municd, df_lg$ststcd, df_lg$year), ]
    
    # x <- 25005
    
    ncores <- detectCores(logical = T)
    cl <- makeCluster(ncores - 1)
    clusterEvalQ(cl, c(library(nnet)))
    clusterExport(cl, list("df_lg", "res", "nlvls"))
    pred <- unlist(parSapply(cl, municd, function(x) {
      tmp <- df_lg[df_lg$municd == x, ]
      tmp$value <- factor(tmp$value, levels = 0:5, ordered = T)
      
      newdf <- res[res$municd == x,]
      
      
      fit <-
        multinom(
          value ~ year * ststcd,
          tmp,
          na.action = na.exclude,
          MaxNWts = 10000000,
          maxit = 1000
        )
      pred <- predict(fit, newdata = newdf)
      
      
    }))
    
    stopCluster(cl)
    
    res <- cbind.data.frame(res, pred)
    colnames(res)[ncol(res)] <- a
  })

bltupss_sim <- res


# 3. save data ---------------------

save(
  bltupss, bltupss_sim,
  file = here(
    pdir,
    "urban_5cat_Ahmed_Wal/urban_5cat_ss_Wal.Rdata"
  )
)

# *. exploration ------------------
 



prop.table(table(bltupss$LU2010_5cls_x25))

cuts <- c(0, 1, 2, 3, 4, 5)

pal <- colorRampPalette(c("yellow", "green"))

lu90 <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "LU1990_5cls_x25.flt"))
lu00 <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "LU2000_5cls_x25.flt"))
lu10 <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "LU2010_5cls_x25.flt"))



bl30 <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "simul_2030_BL_f.flt"))

bl50 <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "simul_2050_BL_f.flt"))

bl70 <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "simul_2070_BL_f.flt"))

bl100 <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "simul_2100_BL_f.flt"))


test <- lapply(list(lu90, lu00, lu10, bl30, bl50, bl70, bl100), values)


test <- Reduce(cbind.data.frame, test)
colnames(test) <- c("lu90", "lu00", "lu10", "bl30", "bl50", "bl70", "bl100")

head(test[test$lu10 > -1,])

table(test$lu10)
table(test$bl30)

tail(test[test$bl100 == 8 & test$lu10 == 4,])

lvls(test$bl30[test$lu10 == 5])

raster <- raster(here(rdir, "urban_5cat_Ahmed_Wal", "simul_2030_BH_f.flt"))



t <- bltupss$simul_2030_BH_f
loadpackage("car")
lvls(mtcars$cyl)
