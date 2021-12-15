# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------

source('3 Scripts/general_functions.R')

# loadpackage('dplyr')
# loadpackage('stringi')

## 1.2 load data --------

### data folder
rdir <- '2 Data/1 Raw'
pdir <- '2 Data/2 Processed'

### survey data CEHD 
# Location information: postcode, municipalities, district, address, submunicipalities, utilities

CEHDads <- read.csv(file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_CEHD_nopassword.csv'), na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = 'Latin-1')[, c(1, 256:262)]

### survey data AquaWal 
# Location information: postcode, municipalities, district, submunicipalities, utilities without address

AquaWobs <- read.csv(file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_AquaWal.csv'),sep = ',', na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = 'Latin-1')[, c(1,5,17,477:482)]


### survey addresses Aquawal
# Seperate file for addresses from AquaWal for all contacted households (non-responded also)

fullads <- read.csv(file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/Survey2014_address_AquaWal.csv'),sep = ',', na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = 'Latin-1') # there are duplicated rows in the data >>> check for whether same questionnaire number different address (it's ok to have different questionnaire number for the same address, since it's can be several housholds share a same house or appartement building)


# 2. processes--------

## 2.1. merge survey and addresses from Aquawal -------

fullads_nodup <- fullads[!duplicated(fullads[,1:2]),]

###!!!! conclusion: no numero_questionnaire with more than 1 address >>> data is good



# merge using numero_questionnaire ('qnb')

colnames(AquaWobs)[2] <- 'qnb'
colnames(fullads_nodup)[1] <- 'qnb'

AquaWads <- dplyr::left_join(AquaWobs, fullads_nodup , by = 'qnb')

# summary(AquaWads[!is.na(AquaWads$Adresse), 'qnb'])

# => all weird numero_questionnaire will have missing addresses, so DON'T HAVE TO CARE, model without spatial can still use other information and obs with weird nq will be removed automatically when add spatial into model

## 2.2. compare with CEHD ----------
# # did try some but no problems found
# # not necessary since address in CEHD was created by the same merging procedure I did

# compare <- merge(AquaWads, CEHDads , by = 'id')
# 
# colnames(compare) <- gsub('.x', '.aw', colnames(compare))
# colnames(compare) <- gsub('.y', '.ce', colnames(compare))
# compare$Adresse.aw <- trimws(compare$Adresse.aw)
# compare$Adresse.ce <- trimws(compare$Adresse.ce)
# 
# mismatch  <- stri_cmp(compare$Adresse.aw, compare$Adresse.ce)
# 
# mmdf <- compare[mismatch !=0 &!is.na(mismatch), c('Adresse.aw', 'Adresse.ce')]

# only 1 different due to Rue and R (nothing to worry about)


## 2.3. format AquaWads ------------



colnames(AquaWads)[3:12] <- c('csmptv', 'pcrp', 'munitx', 'disttx', 'distgr', 'provtx', 'dtbtor', 'address', 'pc', 'local')

AquaWads[,c(5:10,12)] <- apply(AquaWads[,c(5:10,12)], 2, trimws)

AquaWads$munitx <- stringi::stri_trans_general(AquaWads$munitx,"Latin-ASCII; upper")
AquaWads$munitx[AquaWads$munitx %in% 'ECAUSSINES'] <- 'ECAUSSINNES'

AquaWads$local <- stringi::stri_trans_general(AquaWads$local,"Latin-ASCII; upper")

AquaWads$street <- trimws(gsub(',.*', '',AquaWads$address))
AquaWads$street <- stringi::stri_trans_general(AquaWads$street,"Latin-ASCII; upper")
AquaWads$street <- gsub('^QUART. ', 'QUARTIER ', AquaWads$street)
AquaWads$street <- gsub('^R. ', 'RUE ', AquaWads$street)
AquaWads$street <- gsub('^R ', 'RUE ', AquaWads$street)
AquaWads$street <- gsub('^AV ', 'AVENUE ', AquaWads$street)
AquaWads$street <- gsub('^CM ', 'CHEMIN ', AquaWads$street)
AquaWads$street <- gsub('^CL ', 'CLOS ', AquaWads$street)
AquaWads$street <- gsub('^CR ', 'COUR ', AquaWads$street)
AquaWads$street <- gsub('^QU ', 'QUAI ', AquaWads$street)
AquaWads$street <- gsub('^PL ', 'PLACE ', AquaWads$street)
AquaWads$street <- gsub('^AL ', 'ALLEE ', AquaWads$street)
AquaWads$street <- gsub('^CH ', 'CHAUSSEE ', AquaWads$street)
AquaWads$street <- gsub('^SQ ', 'SQUARE ', AquaWads$street)
AquaWads$street <- gsub('^RT ', 'ROUTE ', AquaWads$street)
AquaWads$street <- gsub('^V ', 'VOIE ', AquaWads$street)
AquaWads$street <- gsub('^IM ', 'IMPASSE ', AquaWads$street)
AquaWads$street <- gsub('^RS ', 'RESIDENCE ', AquaWads$street)
AquaWads$street <- gsub('^RU ', 'RUELLE ', AquaWads$street)
AquaWads$street <- gsub('^TH ', 'THIER ', AquaWads$street)
AquaWads$street <- gsub('^BD ', 'BOULEVARD ', AquaWads$street)
AquaWads$street <- gsub('-', ' ', AquaWads$street)
AquaWads$street <- gsub('10E', 'DIXIEME', AquaWads$street)
AquaWads$street[AquaWads$id %in% 851] <- 'GRAND\'ROUTE'

AquaWads$nb <- trimws(gsub('.*,', '',AquaWads$address))
AquaWads$nb <- gsub(' ', '', AquaWads$nb)
AquaWads$nb <- gsub('-.*', '', AquaWads$nb)
AquaWads$nb <- gsub('/.*', '', AquaWads$nb)


# 3. save output ----------


write.csv(AquaWads, file = file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/Surv14_ads.csv'), row.names = F)




