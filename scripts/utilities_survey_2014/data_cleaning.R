# notes from the last run -----------------

# 1. setup ---------

## 1.1. load functions -----------------

source('3 Scripts/general_functions.R')


loadpackage("stringi")
loadpackage('sf')
loadpackage("dplyr")


hhsize <- function(year, birth_year, age_brks = c(0, 20, 65),id, ocp = T, ocp_mat) {
  age <- year - birth_year
  age[age < 0] <- NA
  
  res <- cbind.data.frame(id, 'year' = rep(year, length(id)))
  age_brks <- c(age_brks, max(age, na.rm = T) +1)
  n <- length(age_brks)
  
  if (ocp) {
    for (i in seq_len(n-1)) {
      agr_mat <- age >= age_brks[i] & age < age_brks[i+1]
      npers <- apply(agr_mat*ocp_mat, 1, sum, na.rm = T)
      res <- cbind(res, npers)
      colnames(res)[ncol(res)] <- paste('hhspo', age_brks[i], age_brks[i+1] - 1, sep = '_')
    }
    res$hhsttpo <- apply(res[, 3:ncol(res)], 1, sum, na.rm = T)
    res[res$hhsttpo %in% 0, 3:ncol(res)] <-NA 
  } else {
    for (i in seq_len(n-1)) {
      agr_mat <- age >= age_brks[i] & age < age_brks[i+1]
      npers <- apply(agr_mat, 1, sum, na.rm = T)
      res <- cbind(res, npers)
      colnames(res)[ncol(res)] <- paste('hhs', age_brks[i], age_brks[i+1] - 1, sep = '_')
    }
    res$hhstt <- apply(res[, 3:ncol(res)], 1, sum, na.rm = T)
    res[res$hhstt %in% 0, 3:ncol(res)] <-NA
  }
  res
}  



nb_room <- function(char) {
  df <- dweltemp[, c(1,grep(pattern = paste0('Q18_.*_', char), names(dweltemp)))]
  df1 <- as.data.frame(df[,1])
  for (i in 2:length(df)) {
    v <- as.numeric(factor(df[,i])) - 1
    df1 <- cbind.data.frame(df1,v)
  }
  colnames(df1) <- c('id', paste(char, c('0', '1', '2', '3 and more'), sep = '_'))
  sumdf1 <- apply(df1[,2:length(df)], 1, sum)
  maxpos <- apply(df1[,2:length(df)], 1, function(x) which.max(rev(x)))
  res <- factor(maxpos, levels = 4:1, labels = c('0', '1', '2', '3 or more'))
  res[sumdf1 %in% '0'] <- NA
  res
}

tech3lvs <- function(x, y) {
  res <- as.character(x)
  res[x %in% 'yes' & y %in% 'yes'] <- 'recent_replaced'
  res <- factor(res, levels = c('no', 'yes', 'recent_replaced'))
}


## 1.2 load data --------

### data folder ----------
rdir <- '2 Data/1 Raw'
pdir <- '2 Data/2 Processed'

### survey data ------------

var <- read.csv(file.path(pdir, "UtiSurv_2014_AWalCEHD_Wal/Survey2014_var_AquaWal_slt.csv"), na.strings = c("", "#NULL!"), strip.white = TRUE) # import csv file with variables information

slvar <- var$Location # columnes number of needed variables

org <- read.csv(file.path(pdir, "UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_AquaWal.csv"), na.strings = c("", "#NULL!"), strip.white = TRUE)[,slvar] # import original data Cedric sent, select only needed columns using slvar

org <- org[order(org$id),]

# write.csv(org, file = file.path(dir, "UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_AquaWal_slt.csv"), row.names = F)

### statistical sectors -----------

load(file = file.path(pdir, 'Admin_border_Wal/Admin_stst_Wal.Rdata'))


# 2. general --------------------

## 2.1. merging/filtering ---------

### 2.1.1. id 

general <- org[,1:2] # take id and numero_questionnaire columes
# summary(general)

### 2.1.2. qnb

colnames(general)[2] <- 'qnb' # rename numero_questionnaire = nq


# table(general[general$nq < 1 | general$nq > 30000, 'nq'])

# table(general[as.factor(general$nq) %in% factor(general[duplicated(general$nq), 'nq']), 'nq'])

# # numero_questionnaire 13685 was double

### 2.1.3. mvinyr 

general$mvinyr <- org$Q20_ann_occup # mvinyr = year the household moved in the current dwellings, will be use later for merging with historical consumption from company

# summary(general$mvinyr[general$mvinyr <= 2014])

# summary(as.factor(general$mvinyr[general$mvinyr > 2014]))
# df <- org[general$mvinyr > 2014 & complete.cases(general$mvinyr), c('id', 'Q20_ann_occup', 'Q1_conso_eau', 'Consoeauverif' )]

# # some people move in 2015 => cannot say about consumption in 2014

### 2.1.4 prfuse 
general$prfuse <- factor(org$Q11_prof_ville, labels = c('no', 'yes')) # whether they use distribution water for professional purpose at house

# summary(general$prfuse)

# hist(org[org$Q11_prof_ville %in% 'Oui',]$Consoeauverif)
# hist(org[org$Q11_prof_ville %in% 'Non',]$Consoeauverif)


## 2.2. verified consumption -------------

general$csmptv <- org[,65]

# # remove household who moved in after 2014 or use distribution water for professional purpose

general$csmptv[general$prfuse %in% 'yes' | general$mvinyr >2014] <- NA


# summary(general$csmptv)


## 2.3. weight -------------------
# # use weight of Cedric, there is good documentation of weights in the rapport-final-aquawal-cehd-v8 pages 21-27
# # after using weight, the proportion of observations per class of province, owner/renter, age of preference person, age of dwelling, dwelling type (4-facade, ..., appartement) seems similar to observed in the whole region from other reports

general$weight <- org$POIDS_Geo_Stat_AgeP1_AgeL

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

## 2.4 reorder ---------------

general <- general[order(general$id),]


# 3. other water sources ---------
# # information based mainly on question 11 in the questionnaire, except for the rain water tank was taken from the question 24


altsrc <- org[,c('id', 'Puits')]
altsrc <- altsrc[order(altsrc$id),]


colnames(altsrc)[2] <- 'pvwlbn'

q11 <- org[,7:54]

## 3.1. private wel ------------------  
# # private well binary 
# # Cedric created this variable based on whether that house use well water for any purpose (both indoor and outdoor)
# # did check again using original information from question 11, the count of using well water and not using well water are the same
# # pvwlbn = private well binary: yes for using well water (either indoor or outdoor or both), no for not using well water at all

altsrc$pvwlbn <- factor(altsrc$pvwlbn, labels = c('no', 'yes'))

# summary(altsrc$pvwlbn)

# # private well indoor 

q11wel <- q11[,grep( "puit" , names(q11))]
q11welidr <- q11wel[, 1:8]

mat <- apply(q11welidr, 2, function(x) x == 'Oui')
v <- apply(mat, 1, sum)
# summary(as.factor(v))

altsrc$pvwlid <- rep(NA, nrow(altsrc))
altsrc$pvwlid[v ==0] <- 'no'
altsrc$pvwlid[v > 0] <- 'yes'
altsrc$pvwlid <- factor(altsrc$pvwlid)


# # private well outdoor 

q11welodr <- q11wel[, 9:12]

mat <- apply(q11welodr, 2, function(x) x == 'Oui')
v <- apply(mat, 1, sum)
# summary(as.factor(v))

altsrc$pvwlod <- rep(NA, nrow(altsrc))
altsrc$pvwlod[v ==0] <- 'no'
altsrc$pvwlod[v > 0] <- 'yes'
altsrc$pvwlod <- factor(altsrc$pvwlod)


# # private well 4 level: no, indoor, outdoor, both

altsrc$prvwel <- paste0(altsrc$pvwlid, altsrc$pvwlod)

altsrc$prvwel <- factor(altsrc$prvwel, labels = c('no', 'outdoor', 'indoor', 'both')) # 2018 no, 40 outdoor, 6 indoor, 55 both


## 3.2. rain water tank ------------


altsrc$rwtank <- factor(org$Q24_cit_pres, labels = c('no', 'yes')) # whether they have rain water tank

# summary(altsrc$rwtank)

# # rain water tank replace 

altsrc$rwtkrp <- factor(org$Q24_cit_rempl, labels = c('no', 'yes')) # whether the rain water tank replace after 2009

# summary(altsrc$rwtkrp)
# 
# df <- altsrc[altsrc$rwtank %in% 'no' & altsrc$rwtkrp %in% 'yes',]
# write.table(t(df$id), 'clipboard', sep = '\t', row.names = F, col.names = F) # id of household without rain water tank but saying that they replaced rain water tank since 2009

# # Rain water use binary

q11rwuse <- q11[,grep( "pluie" , names(q11))]

mat <- apply(q11rwuse, 2, function(x) x == 'Oui')
v <- apply(mat, 1, sum)
# summary(as.factor(v))

altsrc$rwusbn <- rep(NA, nrow(altsrc))
altsrc$rwusbn[v ==0] <- 'no'
altsrc$rwusbn[v > 0] <- 'yes'
altsrc$rwusbn <- factor(altsrc$rwusbn)

# write.table(table(altsrc$rwtank, altsrc$rwusbn), 'clipboard', sep = '\t', row.names = T, col.names = T)
# chisq.test(altsrc$rwtank, altsrc$rwusbn, correct = F)


# # rwusid rain water use indoor

q11rwtidr <- q11rwuse[, 1:8]

mat <- apply(q11rwtidr, 2, function(x) x == 'Oui')
v <- apply(mat, 1, sum)
# summary(as.factor(v))

altsrc$rwusid <- rep(NA, nrow(altsrc))
altsrc$rwusid[v ==0] <- 'no'
altsrc$rwusid[v > 0] <- 'yes'
altsrc$rwusid <- factor(altsrc$rwusid)

# summary(altsrc$rwusid)


# # rwusod rain water use outdoor

q11rwtodr <- q11rwuse[, 9:12]

mat <- apply(q11rwtodr, 2, function(x) x == 'Oui')
v <- apply(mat, 1, sum)
# summary(as.factor(v))

altsrc$rwusod <- rep(NA, nrow(altsrc))
altsrc$rwusod[v ==0] <- 'no'
altsrc$rwusod[v > 0] <- 'yes'
altsrc$rwusod <- factor(altsrc$rwusod)

# summary(altsrc$rwusod)

altsrc$rwtuse <- paste0(altsrc$rwusid, altsrc$rwusod)

altsrc$rwtuse <- factor(altsrc$rwtuse, labels = c('no', 'outdoor', 'indoor', 'both')) # 1111 no, 45 outdoor, 24 indoor, 519 both

# summary(altsrc$rwtuse)

## 3.3. other sources Cedric ------
# # taken from Cedric, other sources including pluie, puits, autre, exclude bout
# # for at least 1 purpose indoor

altsrc$otsrid <- factor(org$Ressaltint, labels = c('no', 'yes'))

# summary(altsrc$otsrid)

# # other sources outdoor 
# # I made, other sources including pluie, puits, autre, exclude bout
# # for at least 1 purpose outdoor

q11oth <- q11[, -grep("bout", names(q11))]

q11othod <- q11oth[, 25:36]

matod <- apply(q11othod, 2, function(x) x == 'Oui')
vod <- apply(matod, 1, sum)

altsrc$otsrod <- factor(rep('no', length(vod)), levels = c('no', 'yes'))
altsrc$otsrod[vod > 0] <- 'yes'

# summary(as.factor(vod))
# summary(altsrc$otsrod)

# # other sources outdoor Cedric 
# # taken from Cedric, other sources including pluie, puits, autre, exclude bout
# # use other sources of water for at least 2 purposes outdoor, and must not use for indoor
# # absolutely stupid and shouldn't be used

altsrc$otsodc <- factor(org$Ressaltext, labels = c('no', 'yes'))

# summary(altsrc$otsrod)
 
# # check again for both otsrid and otsodc 
# 
# q11othid <- q11oth[, 1:24]
# 
# matid <- apply(q11othid, 2, function(x) x == 'Oui')
# vid <- apply(matid, 1, sum)
# summary(as.factor(vid))
# 
# v <- rep(0, length(vid))
# v[vid == 0 & vod >1] <- 1
# 
# summary(as.factor(v)) # same with other sources to use only outdoor (at least 2 purposes, or 2 sources for 1 purpose) of Cedric


## 3.4 reorder ---------------


altsrc <- altsrc[order(altsrc$id),]


# 4. spatial -----------------

## 4.1 administrative names/codes ----------


sptsv <- data.frame(org[, c(1, 72, 76)])


colnames(sptsv)[2:3] <- c('munisv','dtbtor') # rename distributer columns (CILE/SWDE ...) = dtbtor

sptsv$munisv <- stri_trans_general(sptsv$munisv ,"Latin-ASCII; title") # reformat text

sptsv$munisv <- car::recode(sptsv$munisv, "'Ecaussines' = 'Ecaussinnes'; 'Amel' = 'Ambleve' ")

wal_stst_df <- as.data.frame(wal_stst)
municode <- wal_stst_df[!duplicated(wal_stst_df[,c('muninm', 'municd')]), 5:10]

sptsv <- dplyr::left_join(sptsv, municode, by = c('munisv'= 'muninm'))

sptsv <- sptsv[order(sptsv$id), c(1,4,2,5:8,3)]

colnames(sptsv)[c(2:7)] <- c('mncdsv', 'mnnmsv', 'dtcdsv', 'dtnmsv', 'pvcdsv', 'pvnmsv') # sv means from the survey

sptsv[,2:7] <- lapply(sptsv[,2:7], factor)

# summary(sptsv)

## 4.2. correct distributors --------

# summary(factor(org$Distributeur))

# # distributer has very few missings - which is a good thing
# # only 4 distributers: CILE, IECBW (inBW), INASEP, SWDE
# # 3 houses get their water from commune distributer (Vresse-Sur-Semois, Amel, Theux) although actually only the one in Theux can be mapped.

sptsv$dtbtor[sptsv$mnnmsv %in% 'Vresse-Sur-Semois'] <- 'Vresse-Sur-Semois'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Ambleve'] <- 'Ambleve'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Theux'] <- 'Theux'


# # although some family with distributor = SWDE but the addresses do not belong to SWDE service area.
# # the obs with probably wrong distributors are in Erezee, Etalle, Gouvy, Mettet, Ouffet, Rouvroy, Stoumont, Villers-La-Ville/Genappe. 
# # all these communes are supplied by one distributor save Ouffet which is supplied by CIESAC and CILE which have rather different CVD in 2014 (1.935 & 2.6366 respectively)
# # all obs from these commune are not mappable save the obs in Ouffet, however obs in Ouffet cannot be find in historical consumption data of CILE anyway
# # so it's clearly unimportant to spend time to really care about these obs since they will be removed anyway in future analysis.
# # although to make sure that they don't influence some analysis without spatial factors => set their distributors to the distributors of that commune, and Ouffet to NA

sptsv$dtbtor[sptsv$mnnmsv %in% 'Ouffet'] <- NA
sptsv$dtbtor[sptsv$mnnmsv %in% 'Erezee'] <- 'Erezee'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Etalle'] <- 'Etalle'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Gouvy'] <- 'Gouvy'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Mettet'] <- 'AIEM'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Rouvroy'] <- 'Rouvroy'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Stoumont'] <- 'Stoumont'
sptsv$dtbtor[sptsv$mnnmsv %in% 'Villers-La-Ville'] <- 'IECBW'


sptsv$dtbtor <- as.factor(sptsv$dtbtor)

## 4.3. reorder ------------

sptsv <- sptsv[order(sptsv$id),]

# 5. socio-demographic ------------------
## 5.1. import data ----------

scdmtemp <- data.frame(org[order(org$id), c(1, 137:218)]) # also order by id

## 5.2. household size ---------------
# # Use only birthyear information for household size because it needed for calculate number of equivalent adults which inturn is used to calculate income per equivalent adult which is a very important (and always significant variable) => even other variable may provide more info for householdsize, these info will be removed anyway when fitting model with income

birth_year <- scdmtemp[,grepl('naiss', names(scdmtemp))]
colnames(birth_year) <- paste0('byp', 1:ncol(birth_year))

id <- scdmtemp$id

# # personal occupation: whether they live in that house the whole week (true for nearly 90% of them) (ocp = occupancy)
ocp_mat <- scdmtemp[,grepl('_occ', names(scdmtemp))]

ocp_mat[ocp_mat == 'Toute la semaine'] <- 1
ocp_mat[ocp_mat == "La moitié de la semaine ou plus"] <- 0.75
ocp_mat[ocp_mat == "Moins de la moitié de la semaine"] <- 0.25

ocp_mat <- apply(ocp_mat, 2, as.numeric)
colnames(ocp_mat) <- paste0('ocpp', 1:ncol(ocp_mat))

socidemo <- cbind.data.frame(id, birth_year, ocp_mat)


hhs14 <- hhsize(2014, birth_year = birth_year, age_brks = c(0, 20,65), id = id, ocp = F, ocp_mat)

hhspo14 <- hhsize(2014, birth_year = birth_year, age_brks = c(0, 20,65), id = id, ocp = T, ocp_mat)

socidemo <- cbind.data.frame(socidemo, hhs14[3:ncol(hhs14)], hhspo14[3:ncol(hhspo14)])

socidemo$hhs_20_95 <- socidemo$hhs_20_64 + socidemo$hhs_65_95
socidemo$hhspo_20_95 <- socidemo$hhspo_20_64 + socidemo$hhspo_65_95

socidemo$eqadlt <- 1 + (socidemo$hhs_20_95 - 1)*0.5 + socidemo$hhs_0_19*0.3

socidemo$eqadpo <- ifelse(socidemo$hhspo_20_95 > 1 ,1 + (socidemo$hhspo_20_95 - 1)*0.5 + socidemo$hhspo_0_19*0.3, socidemo$hhspo_20_95 + 0.3* socidemo$hhspo_0_19)


  




## 5.3. household head ------------------------------------

# # select household head

age <- 2014 - birth_year

refper <- apply(age, 1, function(x) which.max(x)[1])


rprage <- sapply(1:nrow(age), function(x) ifelse(is.na(refper[x]), NA, age[x,refper[x]]))

refper[rfrage < 18] <-NA
rprage[rfrage < 18] <-NA


# # take gender/job/education info for reference person from org dataset
x <-2

rprgen <- sapply(1:nrow(age), function(x) ifelse(is.na(refper[x]), NA, as.character(scdmtemp[x, grep(paste0(refper[x],"_sex") , names(scdmtemp))])))

rprjob <- sapply(1:nrow(age), function(x) ifelse(is.na(refper[x]), NA, as.character(scdmtemp[x, grep(paste0(refper[x],"_act") , names(scdmtemp))])))

rprjot <- sapply(1:nrow(age), function(x) ifelse(is.na(refper[x]), NA, as.character(scdmtemp[x, grep(paste0(refper[x],"_inact") , names(scdmtemp))])))

rpredu <- sapply(1:nrow(age), function(x) ifelse(is.na(refper[x]), NA, as.character(scdmtemp[x, grep(paste0(refper[x],"_dipl") , names(scdmtemp))])))


socidemo$rprage <- rprage # merge to socidemo dataset
socidemo$rprgen <- factor(rprgen, labels = c('F', 'M')) # merge to socidemo dataset and rename levels to F and M

# ggplot(socidemo, aes(x = rprage)) + geom_histogram() + xlab('age of reference person') + theme_bw()

# # further format job

job <- as.character(factor(rprjob, labels = c('other', 'manager', 'private sector', 'state employee', 'independent', 'worker', 'freelancer', 'unemployed')))

jobot <- stri_trans_general(rprjot,"Latin-ASCII")

job[jobot %in% '(Pre)pensionne'] <- '(pre)retired'
job[jobot %in% 'Autre'] <- 'other'
job[jobot %in% 'Demandeur d\'emploi'] <- 'unemployed'
job[jobot %in% 'En incapacite maladie/ invaldite'] <- 'incapable'
job[jobot %in% 'Etudiant/Eleve'] <- 'student'
job[jobot %in% 'Femme/homme au foyer'] <- 'housewife/husband'

socidemo$rprjob <- as.factor(job)

# ggplot(socidemo) + geom_bar(aes(x= rprjob)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# # relevel education

rpredu <- stri_trans_general(rpredu,"Latin-ASCII")

socidemo$rped6c <- as.factor(car::recode(rpredu, "c('Primaire/pas de diplome','Secondaire inferieur (3eme secondaire)')='before highschool'; 'Secondaire superieur general (6eme secondaire)' = 'highschool'; 'Professionnel' = 'professional'; 'Technique' = 'technique'; 'Superieur non universitaire' = 'higher not university'; 'Universitaire' = 'university'"))

# summary(factor(socidemo$rped4c))

socidemo$rped4c <- as.factor(car::recode(socidemo$rped6c, "c('professional','technique') = 'vocation'; c('higher not university', 'university') = 'university'"))

# ggplot(socidemo) + geom_bar(aes(x= rped4c)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))



## 5.4. income ----------------

# # assign 125 for income < 250, midpoint for other groups, and 5250 for income > 5000

income <- factor(scdmtemp$Q28_rev, levels(factor(scdmtemp$Q28_rev))[c(12, 5, 11, 2:4, 6:10, 1)], labels = c(125,375,seq(750,5250, 500)))

# summary(factor(scdmtemp$Q28_rev))

socidemo$income <- as.numeric(as.character(income)) # convert to numeric

socidemo$inceqa <- socidemo$income*12/socidemo$eqadlt # income per equivalent adults


# # income equivalent cat 

# # categorize income base on Wallonia definition of logement moyen (0, 12900, 25700, 39900)

socidemo$iceqac1 <- cut(socidemo$inceqa, breaks = c(0, 12900, 25700, 39900, max(socidemo$inceqa, na.rm = T)), labels = c('precarious', 'modest', 'average', 'higher'))

# ggplot(socidemo, aes(x = inceqa)) + geom_histogram() + xlab('income per equivalent adult') + geom_vline(xintercept = c(12900, 25700, 39900), color = 'red', linetype = 'longdash')+ theme_bw()
#  
# ggplot(socidemo) + geom_bar(aes(x= iceqac1)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# # categorize income based on Wallonia definition but calculate after tax money (using online calculator and an average municipal tax 7%)
# # cutoff value is then 11200, 19600, 26800
# # can use postcode to change the cut off value - but then might be increase number of missing
# # also it will complicated my life unnecessary

socidemo$iceqac2 <- cut(socidemo$inceqa, breaks = c(0, 11200, 19600, 26800, max(socidemo$inceqa, na.rm = T)), labels = c('precarious', 'modest', 'average', 'higher'))

# ggplot(socidemo, aes(x = inceqa)) + geom_histogram(bins = 100, fill = 'white', color = 'black') + xlab('income per equivalent adult') + geom_vline(xintercept = c(11200, 19600, 27000), color = 'red', linetype = 'longdash')+ theme_bw()
# 
# ggplot(socidemo) + geom_bar(aes(x= iceqac2)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# levels(factor(org$Code_postal))


# # income cat base on single household or menage and number of children below 20 years old 

hhsca20 <- hhsize(year = 2014, birth_year = birth_year, age_brks = c(0, 20), id = id, ocp = F, ocp_mat = ocp_mat)



incomwc <- socidemo$income*12 - 2400*hhsca20$hhs_0_19

isolee <- hhsca20$hhs_20_95 < 2


inciso <- isolee*incomwc
inciso[inciso %in% 0] <- NA
incisoc <- cut(inciso, breaks = c(0, 12900, 25700, 39900, max(inciso, na.rm = T)), labels = c('precarious', 'modest', 'average', 'higher'))


incmen <- incomwc*(1-isolee)
incmen[incmen %in% 0] <- NA
incmenc <- cut(incmen, breaks = c(0, 17500, 32100, 48200, max(incmen, na.rm = T)), labels = c('precarious', 'modest', 'average', 'higher'))

socidemo$inccat <- incmenc

socidemo$inccat[is.na(incmenc)] <- incisoc[is.na(incmenc)]

# ggplot(socidemo) + geom_bar(aes(x= iceqac1)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(socidemo) + geom_bar(aes(x= inccat)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))


## 5.5. difficult paying bill -----

# # whether receive help from Fonds Social de l’Eau

socidemo$fseaid <- factor(scdmtemp$Q8_fd_eau, labels = c('no', 'yes'))

# # problem paying bill due to financial problem

socidemo$dfpay <- factor(scdmtemp$Q10_probl_fact_eau, labels = c('not user','no', 'yes'))

# # Taux effort hydrique - percentage of water bill/income (probably calculated by Cedric)

socidemo$TEH <- scdmtemp$TauxEffortHydrique # Average = 1.28, median = 1.03

## 5.6. reorder ---------------

socidemo <- socidemo[order(socidemo$id),]

# summary(socidemo)

# 6. dwelling --------------------

## 6.1. import data -----------

# # take data from org data + equivalent number of person from socidemo

dweltemp <- merge(org[, c(1, 77:118, 39:42, 119)], socidemo[,c('id', 'eqadlt')], by = 'id')
dweltemp <- dweltemp[order(dweltemp$id),]


## 6.2. Home-ownership --------

dwelling <- dweltemp[, 1:2]
colnames(dwelling)[2] <- 'dwowsh'

dwelling$dwowsh <- factor(dweltemp$Q14_stat_occup, levels(factor(dweltemp$Q14_stat_occup))[c(4,3,2,1)], labels = c('owner - mortgage loan', 'owner', 'renter - private sector', 'renter - social or public'))

# ggplot(dwelling) + geom_bar(aes(x= dwowsh)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))


## 6.3. type --------

dwelling$dwltyp <- factor(dweltemp$Q15_type_log, levels(factor(dweltemp$Q15_type_log))[c(3,4,5,1,2)], labels = c('4 facades', '3 facades', '2 facades', 'appartment/studio', 'other'))

# # manually add value for the one with missing Q15_type_log, but have info in Q15_autre_type_log

dwelling$dwltyp[dwelling$id %in% c(221, 1495)] <- '2 facades'
dwelling$dwltyp[dwelling$id %in% c(886, 1256, 2478, 2871, 2814, 2583, 2304, 2431)] <- '4 facades'
dwelling$dwltyp[dwelling$id %in% c(1952, 1399, 1884)] <- NA
dwelling$dwltyp[dwelling$id %in% 2557] <- 'appartment/studio'

# ggplot(dwelling) + geom_bar(aes(x= dwltyp)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))


## 6.4. construction year ------------


# # dwcsy9 = dwelling construction year 9 classes

dwelling$dwcsy9 <- factor(dweltemp$Q17_ann_constr, levels(factor(dweltemp$Q17_ann_constr))[c(2:9,1)], labels = c('Before 1875', '1875-1918', '1919-1945', '1946-1970', '1971-1980', '1981-1990', '1991-2000', '2001-2005', '2006 and after'))

# # dwcsy6 dwelling construction year 6 classes - based on Cedric advice + histogram
dwelling$dwcsy6 <- car::recode(dwelling$dwcsy9, "c('Before 1875', '1875-1918') = 'Before 1919'; c('1971-1980', '1981-1990') = '1971-1990'; c('2001-2005', '2006 and after') = '2001 and after'")

dwelling$dwcsy6 <- factor(dwelling$dwcsy6, levels = levels(dwelling$dwcsy6)[c(6,1:5)])

# # dwcsy5 dwelling construction year 5 classes - based on Cedric advice
dwelling$dwcsy5 <- car::recode(dwelling$dwcsy9, "c('Before 1875', '1875-1918', '1919-1945') = 'Before 1945'; c('1971-1980', '1981-1990') = '1971-1990'; c('2001-2005', '2006 and after') = '2001 and after'")

dwelling$dwcsy5 <- factor(dwelling$dwcsy5, levels = levels(dwelling$dwcsy5)[c(5,1:4)])

# ggplot(dwelling) + geom_bar(aes(x= dwcsy9)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(dwelling) + geom_bar(aes(x= dwcsy6)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(dwelling) + geom_bar(aes(x= dwcsy5)) +xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))


## 6.5. living area ---------

# # use corrected data from CEHD

dwelling$livara <- dweltemp$Q16_superf_hab_COR

# # dataset contain the observation which has the reported data != the corrected area

# df <- dweltemp[dweltemp$Q16_superf_hab != dweltemp$Q16_superf_hab_COR & !is.na(dweltemp$Q16_superf_hab_COR),]

# # calculate living area per equivalent adults: note that the coefficient 0.5 for an extra adults and 0.3 for an extra children was developped for income and might not be true for living area

dwelling$lareqa <- dwelling$livara/dweltemp$eqadlt

# ggplot(dwelling, aes(x = livara)) + geom_histogram() + xlab('living area') + theme_bw()
# 
# ggplot(dwelling, aes(x = lareqa)) + geom_histogram() + xlab('living area per equivalent adults') + theme_bw()

# # 6.6. number of room -------------

# # my function to calculate the number of room (0, 1, 2, 3 or more)
# # some household indicate yes for more than 1 options (e.g 0 room & 1 room) => select the first yes from the right
# # some household did not indicate anything => missing???
# # the function could not be used elsewhere, might think to make it more altsrc



dwelling$nbktch <- nb_room('cuis')
dwelling$nblvrm <- nb_room('sal')
dwelling$nbbdrm <- nb_room('cham')
dwelling$nbbtrm <- nb_room('sdb')
dwelling$nbtoil <- nb_room('toil')

# ggplot(dwelling) + geom_bar(aes(x= nbktch)) +xlab('number of kitchen') + theme_bw() + theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(dwelling) + geom_bar(aes(x= nblvrm)) +xlab('number of living room') + theme_bw() + theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(dwelling) + geom_bar(aes(x= nbbdrm)) +xlab('number of bedroom') + theme_bw() + theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(dwelling) + geom_bar(aes(x= nbbtrm)) +xlab('number of bathroom') + theme_bw() + theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(dwelling) + geom_bar(aes(x= nbtoil)) +xlab('number of WC') + theme_bw() + theme(axis.text.x = element_text(angle = 90))

## 6.7. pool ---------------

# # filling up pool and pond using distribution water

dwelling$pldisw <- factor(dweltemp$Q11_pisc_ville, labels = c('no', 'yes'))

# ggplot(dwelling) + geom_bar(aes(x= pldisw)) +xlab('using distribution water for pool') + theme_bw() + theme(axis.text.x = element_text(angle = 90))


## having permanant pool

dwelling$pmnpol <- factor(dweltemp$Q24_pisc_perm_pres, labels = c('no', 'yes'))

# ggplot(dwelling) + geom_bar(aes(x= pmnpol)) +xlab('having permanant pool') + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# # replace permenant pool since 2009

dwelling$pmnprp <- factor(dweltemp$Q24_pisc_perm_rempl, labels = c('no', 'yes'))

# ggplot(dwelling) + geom_bar(aes(x= pmnprp)) +xlab('replace permenant pool since 2009') + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# # check if no pool but yes replace - 10 houses 

# df <- dwelling[dwelling$pmnpol %in% 'no' & dwelling$pmnprp %in% 'yes',] 

# # having temporal/inflatable pool

dwelling$tmppol <- factor(dweltemp$Q24_pisc_temp_pres, labels = c('no', 'yes'))

# ggplot(dwelling) + geom_bar(aes(x= tmppol)) +xlab('having temporal/inflatable pool') + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# replace temporal/inflatable pool (probably not use)

dwelling$tmpprp <- factor(dweltemp$Q24_pisc_temp_rempl, labels = c('no', 'yes'))

# ggplot(dwelling) + geom_bar(aes(x= tmpprp)) +xlab('having temporal/inflatable pool replace') + theme_bw() + theme(axis.text.x = element_text(angle = 90))

# # check if no temp pool but yes replace - 10 houses 

# df <- dwelling[dwelling$tmppol %in% 'no' & dwelling$tmpprp %in% 'yes',] 

## 6.8. garden --------------

# using anykind of water for garden

garden <- dweltemp[,grep(pattern = 'jard', names(dweltemp))]

v <- apply(garden, 1, function(x) 'Oui' %in% x)

dwelling$garden <- factor(v, labels = c('no', 'yes'))




## 6.9. reorder ---------




dwelling <- dwelling[order(dwelling$id),]




# 7. technology -----------------

techtemp <- org[, c(1, 123:136)]

## 7.1. dishwasher -------------------

dshwas <- factor(techtemp$Q23_vais_pres, labels = c('no', 'yes'))

# replace after 2009

dshwrp <- factor(techtemp$Q23_vais_rempl, labels = c('no', 'yes'))



dshwas_3lvs <- tech3lvs(dshwas, dshwrp)

technology <-cbind.data.frame('id' = techtemp[,1], dshwas, dshwrp, dshwas_3lvs)


## 7.2. washing machine -------------------

technology$wasmch <- factor(techtemp$Q23_lav_pres, labels = c('no', 'yes'))

# replace after 2009

technology$wasmrp <- factor(techtemp$Q23_lav_rempl, labels = c('no', 'yes'))

technology$wasmch_3lvs <- tech3lvs(technology$wasmch, technology$wasmrp)

## 7.3. bath/shower (baignoire & douche separee de baignoire)----------------

# # bath and shower as separate variable and whter replace after 2009 or not

technology$bath <- factor(techtemp$Q24_baig_pres, labels =  c('no', 'yes'))

technology$bathrp <- factor(techtemp$Q24_baig_rempl, labels =  c('no', 'yes'))

technology$bath_3lvs <- tech3lvs(technology$bath, technology$bathrp)

technology$shower <- factor(techtemp$Q24_dou_pres, labels =  c('no', 'yes'))

technology$shwrrp <- factor(techtemp$Q24_dou_rempl, labels =  c('no', 'yes'))

technology$shower_3lvs <- tech3lvs(technology$shower, technology$shwrrp)

# # bath and shower as 1 variable with 4 levels none, shower, bath, both
# # same for whether they replace none, replace shower, replace bath, or replace both since 2009
                              
bthshw <- paste0(techtemp$Q24_baig_pres, techtemp$Q24_dou_pres)

technology$bthshw <- factor(bthshw, labels = c('none', 'shower', 'bathtub', 'both'))

btshrp <- paste0(techtemp$Q24_baig_rempl, techtemp$Q24_dou_rempl)

technology$btshrp <- factor(btshrp, labels = c('none', 'shower', 'bathtub', 'both'))

## 7.4. saving water appliance --------------

# # treat efficient shower head and efficient WC separately 

# # efficient shower head (Pommeau de douche economiseur d'eau - aerateur/mousseur)
technology$efshhd <- factor(techtemp$Q24_pom_pres, labels = c('no', 'yes'))

# # efficient shower head replace since 2009
technology$efshrp <- factor(techtemp$Q24_pom_rempl, labels = c('no', 'yes'))

technology$efshhd_3lvs <- tech3lvs(technology$efshhd, technology$efshrp)

# # efficient toilet (chasse WC avec economiseur - double bouton/ resevoir reduit/ eco-sac)
technology$eftoil <- factor(techtemp$Q24_wc_pres, labels = c('no', 'yes'))

# # efficient shower head replace since 2009
technology$eftlrp <- factor(techtemp$Q24_wc_rempl, labels = c('no', 'yes'))

technology$eftoil_3lvs <- tech3lvs(technology$eftoil, technology$eftlrp)

# # efficient tech as the combination of 2 original efficient shower head and efficient WC 

eftech <- paste0(techtemp$Q24_pom_pres, techtemp$Q24_wc_pres)

technology$eftech <- as.factor(car::recode(eftech, "'NonNon' = 'no'; c('NonOui', 'OuiNon', 'OuiOui') = 'yes'"))

# # replace tech since 2009

efterp <- paste0(techtemp$Q24_pom_rempl, techtemp$Q24_wc_rempl)

technology$efterp <- as.factor(car::recode(efterp, "'NonNon' = 'no'; c('NonOui', 'OuiNon', 'OuiOui') = 'yes'"))

## 7.5. dried toilet -------------

technology$drtoil <- factor(techtemp$Q24_toi_sech_pres, labels = c('no', 'yes'))

# replace dried toilet since 2009

technology$drtlrp <- factor(techtemp$Q24_toi_sech_rempl, labels = c('no', 'yes'))

technology$drtoil_3lvs <- tech3lvs(technology$drtoil, technology$drtlrp)

## 7.6. AOI -------------

technology$aoi <- ((as.numeric(technology$wasmch) - 1)*0.15 + 0.4*(1- (as.numeric(technology$efshhd)-1)*0.33) + 0.3*(1- (as.numeric(technology$eftoil)-1)*0.33))/0.85


## 7.7. reorder -------------

# pldata <- technology[, grep('3lvs', names(technology))]
# pldata$id <- 1:nrow(pldata)
# 
# pldata_melted <- melt(pldata, id.vars = 'id')
# 
# ggplot(pldata_melted) + 
#   geom_bar(aes(x= variable, fill = value,y = (..count..)/2119 )) + 
#   scale_color_discrete() +
#   xlab(NULL) + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 90))



technology <- technology[order(technology$id),]

# summary(technology)


# 8. behaviour --------------

bhvitemp <- org[, c(1,69,219:237)]

## 8.1. financial help to install rain water tank ----------------
# # [Placement d'une citerne d'eau de pluie] Vous bénéficiez pour votre habitation de quelle(s) prime(s) ou aide(s) versée(s) par la Région wallonne pour améliorer les éléments suivants ?

behavior <- bhvitemp[, 1:2]
colnames(behavior)[2] <- 'hlpwtk'

behavior$hlpwtk <- factor(bhvitemp$Q29_cit, labels =  c('no', 'yes')) # # only 5 yes - very very little

## 8.2. trust in water -------------

behavior$cfdiwq <- factor(bhvitemp$Q12_conf_eau, levels(factor(bhvitemp$Q12_conf_eau))[c(1,5,4,6,3,2)], labels = c('confident', 'rather confident', 'neither confident nor suspicious', 'rather suspicious', 'suspicious', 'no opinion'))

# ggplot(behavior) + geom_bar(aes(x= cfdiwq)) + xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))


## 8.3. pay per use -------------

ppusvl <- bhvitemp$Q7_paye_eau

behavior$ppusvl <- as.factor(car::recode(ppusvl, "c('Charges locatives', 'Pas usager') = 'no'; c('NSP', 'Par an', 'Tous les 2 mois', 'Tous les 3 mois', 'Tous les 6 mois', 'Tous les mois') = 'yes'"))

# summary(factor(behavior$ppusvl))

## 8.4. compteur type --------------
# # only 3 with compteur budget and 1 with limit eau

behavior$bdgmtr <- factor(bhvitemp$Q9_compt_budg_eau, labels =  c('no', 'yes'))

behavior$lmtmtr <- factor(bhvitemp$Q9_lim_eau, labels =  c('no', 'yes'))


# summary(bhvitemp[,19:21])


## 8.5.reorder -------------


behavior <- behavior[order(behavior$id),]


# 9. outcome/ water consumption --------------------

## 9.1. prepare data -------
cnspttemp <- org[, c(1, 161)]


## 9.2. water consumption household level m3/year ----

# # water consumption verified by Cedric: for all household, m3/year

# ggplot(consumption, aes(x = csmptv)) + geom_histogram() + xlab('consumption m3/year') + theme_bw()

# # water consumption of the household taken Q27 (nb of week not occupate)

consumption <- as.data.frame(cnspttemp$id)
colnames(consumption) <- 'id'

cnspttemp$Q27_inocc[cnspttemp$Q27_inocc <0] <- NA

consumption$dwelocp <- (52-cnspttemp$Q27_inocc)/52

consumption$csptdo <- general$csmptv / consumption$dwelocp # consumption with dwelling occupancy - higher value, higher number of missing value

# ggplot(consumption,aes(x= csptdo)) + geom_histogram() + xlab('consumption in 2014 with dwelling occupancy m3') + theme_bw() 

## 9.3. water consumption per person per day (L/day) using total number of person -------

# # without occupancy

consumption$cspppd <- (general$csmptv*1000/socidemo$hhstt)/365

# ggplot(consumption,aes(x= cspppd)) + geom_histogram() + xlab('consumption per person per day in 2014 l') + theme_bw() 

# # with personal and dwelling occupancy

consumption$cspdwo <- (consumption$csptdo*1000/socidemo$hhsttpo)/365

# ggplot(consumption,aes(x= cspdwo)) + geom_histogram() + xlab('consumption/day/person in 2014 with occupancy (l)') + theme_bw() 

## 9.4. water consumption per person per day (L/day) using equivalent adults -------

# # without occupancy

consumption$cspeqa <- (general$csmptv*1000/socidemo$eqadlt)/365

# ggplot(consumption,aes(x= cspeqa)) + geom_histogram() + xlab('consumption/day/equivalent adult in 2014 (l)') + theme_bw() 

# # with personal and dwelling occupancy

consumption$cspeqo <- (consumption$csptdo*1000/socidemo$eqadpo)/365

# ggplot(consumption,aes(x= cspeqo)) + geom_histogram() + xlab('consumption/day/equivalent adult in 2014 with occupancy (l)') + theme_bw() 


## 9.5. reorder ---------------


consumption <- consumption[order(consumption$id),]



# 10. save data ---------------

processed <- Reduce(dplyr::left_join, list(general, altsrc, sptsv, socidemo, dwelling, technology, behavior, consumption))

write.csv(processed, file = file.path(pdir, "UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_AquaWal_prd.csv"), row.names = F)

