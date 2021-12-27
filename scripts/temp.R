geom_jitter


library(ggplot2)
library(ggsignif)
ggplot(df, aes(Classifier, Accuracy))+
  geom_boxplot(aes(fill = Classifier)) + scale_fill_manual(values=c("orange", "pink", "purple")) + # using `ggsignif` to display comparison of interest
  geom_signif(
    comparisons = list(c("GA_SVM", "GA_XGB")),
    y_position = 0.925, tip_length = 0.01, vjust = 0.2, map_signif_level = TRUE)+
  geom_signif(
    comparisons = list(c("GA_RF", "GA_XGB")),
    y_position = 0.95,tip_length = 0.01, vjust = 0.2, map_signif_level = TRUE)+
  geom_signif(
    comparisons = list(c("GA_RF", "GA_SVM")),
    y_position = 0.935,tip_length = 0.01, vjust = 0.2, map_signif_level = TRUE)
ggsave("comparison_plot.png")



loadpackage("stringi")
# loadpackage("car")
# loadpackage("rgdal")
loadpackage('sf')
# loadpackage("spdep")
# loadpackage("ggplot2")
# loadpackage("dplyr")
# loadpackage('reshape2')

# # check to see if it's possible to use POIDS_Geo_Stat_AgeP1_AgeL

# weight <- org[,55:62] # select columns has weight info
# 
# df <- org[, c(75, 77, 80, 78, 174)]
# 
# pc.table <- function(x) {
#   v <- tapply(org$POIDS_Geo_Stat_AgeP1_AgeL,x,sum) # to make frequencies table with weights not using library(questionr)
# # v <- wtd.table(x, weights = org$POIDS_Geo_Stat_AgeP1_AgeL) # to make frequencies table with weights using library(questionr)
#   vprc <- v*100/sum(v)
# }
# l1 <- apply(df, 2, pc.table)
# l2 <- apply(df, 2, function(x) table(x)*100/sum(table(x)) )
# 
# write.table(cbind.data.frame(l1[5],l2[5]), 'clipboard', sep = '\t')

# # summary stats of weights 

# hist(general$weight, breaks = 100)
# write.table(org[general$weight >4, c(1, 75, 77, 80, 78, 174, 61)], 'clipboard', sep = '\t', row.names = F)


### coordinates --------

coordbg <- st_read(file.path(pdir, "UtiSurv_2014_AWalCEHD_Wal/Addresses/Coordinates/Surv14_coordinates/Surv14_coordinates.shp"))

load(file = file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/surv14_coord_PICC.Rdata'))

coordpi <- st_transform(surv14_coords, st_crs(coordbg))

rm(surv14_coords)

# plot(st_geometry(coordgb))
# plot(st_geometry(coordpi), add = T, pch = 16, col = 'red')
# st_crs(coordpi) == st_crs(coordgb)



### parcel area ----------------

parcel <- read.csv(file.path(pdir,'UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_parcel.csv'))


## 1.3. price -----------

# # water price by distributer
price <- read.csv(file.path(dir, 'Water_price_AWal_Wal/water price 2014.csv'),  na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = "UTF-8")[264,c(2,4:12)]


price <- as.data.frame(t(price))
price$dtbtor <- rownames(price)

colnames(price)[1] <- 'CVD'
price$CVA <- 1.745
price$CVD <- as.numeric(as.character(price$CVD))

# summary(price$CVD)

price_commune <- read.csv(file.path(dir, 'Water_price_AWal_Wal/water price 2014.csv'),  na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = "UTF-8")[1:262,c(1,15,17)]

colnames(price_commune) <- c('dtbtor', 'CVD', 'CVA')

# df <- price_commune[price_commune$CVA <1,] # CVA of Erezee, Manhay, Stoumont, and Amel = 0

price_commune$dtbtor <- stri_trans_general(price_commune$dtbtor,"Latin-ASCII; title")

# 4. spatial -----------------

# #  1979 obs mapped using batchgeo.com

# # 1802 obs mapped using PICC data
## 4.2. from batchgeo.com ------------
# # including xy coordinates, id, statistical sectors, submunicipality, municipality, district, province
# # geobatch data after intersect with statistical sectors shape file from statbel


sptbg <- st_intersection(coordbg[,'id'], wal_stst)


sptbg$id <- as.integer(as.character(sptbg$id))

sptbg <- sptbg[order(sptbg$id), ]

colnames(sptbg)[3:11] <- c('sscdbg', 'ssnmbg', 'smcdbg', 'mncdbg', 'mnnmbg', 'dtcdbg', 'dtnmbg', 'pvcdbg', 'pvnmbg') # rename some columns, bg means from batchgeo.com

## 4.3. from merging with PICC ---------


sptpi <- st_intersection(coordpi[,'id'], wal_stst)

sptpi <- sptpi[order(sptpi$id), ]

colnames(sptpi)[3:11] <- c('sscdpi', 'ssnmpi', 'smcdpi', 'mncdpi', 'mnnmpi', 'dtcdpi', 'dtnmpi', 'pvcdpi', 'pvnmpi')


## 6.9. parcel area ---------------
dup <- unique(parcel$id[duplicated(parcel$id)])

parcel <- parcel[!(parcel$id %in% dup),]

dwelling <- left_join(dwelling, parcel)


# 10. economic ---------------

## 10.1. average price ---------------

## there are 3 types of prices which can be considered: average price, marginal prices, and bill for averages households.
# # price by divide facture/consumption - average price

ecotemp <- consumption[, c('id','csptdo')]
ecotemp$csmptv <- general$csmptv
ecotemp$dtbtor <- sptdf$dtbtor

ecotemp$bill <- org$Factureeaucalculée

# plot(org$Factureeaucalculée, org$Factureeau1an)
# abline(a = 0, b = 1)

ecotemp$av_price <- ecotemp$bill/ecotemp$csmptv
ecotemp$av_price[ecotemp$av_price %in% Inf] <- NA


# summary(ecotemp$av_price)
# hist(ecotemp$av_price,breaks = 1000)

# av_price received high values due to low consumption since all families pay averagely 100eur/meter as fixed fee, hence with similar consumption habit, small family would have to pay higher price for water than big family. 
## 10.2. bill for an average household -------------





price <- rbind.data.frame(price, price_commune[price_commune$CVD > 0,])

# summary(price$CVD)

# hist(price$CVD)

# # merge with economic data

economic <- left_join(ecotemp, price)
economic$bill70 <- 1.06*(20*economic$CVD + 30*economic$CVA) + 1.06*0.0125*70 + 1.06*0.5*30*economic$CVD + 1.06*40*(economic$CVD+economic$CVA) # total bill calculated for an avearge household who consume 70m3/year

# summary(economic$bill70)

# df <- economic[economic$bill70 < 300, ]



## 10.3. marginal price -----------------

small_consume <- which(economic$csmptv < 30)

economic$marg_price <- 1.06*(0.0125 + economic$CVD + economic$CVA)

economic$marg_price[small_consume] <- 1.06*(0.0125 + 0.5*economic$CVD[small_consume])

# plot(economic$marg_price)

## 10.4. save data ---------
economic <- economic[, c(1,5:10)]
economic <- economic[order(economic$id),]