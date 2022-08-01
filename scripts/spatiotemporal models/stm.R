#' ---
#' title: "Spatio-temporal models"
#' author: "Nguyen Bich Ngoc, Mario Cools, Jacques Teller"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---

#+ r setup, include = F, message = F
# notes from last run----------------------

# rmarkdown::render("scripts/hh_factors/hh_factors.R",output_file=paste0("hh_factors_", format(Sys.time(), "%y%m%d_%H%M"),".md"))

# 1. set up -------------------

## 1.1. load functions -------------

### new functions ------------


loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

mdeval1 <- function(model = m1, newdata = vald, data = train, re.form = ~ (1|municd)) {
  train_pred <- predict(model)
  train_pred_wore <- predict(model, re.form = re.form)
  train_rmse <- sqrt(mean((train_pred - data$cspd)^2, na.rm = T))
  train_rmse_wore <- sqrt(mean((train_pred_wore - data$cspd)^2, na.rm = T))
  vald_pred <- predict(model, newdata = newdata, allow.new.levels = T)
  vald_pred_wore <- predict(model, re.form = re.form, newdata = newdata, allow.new.levels = T)
  vald_rmse <- sqrt(mean((vald_pred - newdata$cspd)^2, na.rm = T))
  vald_rmse_wore <- sqrt(mean((vald_pred_wore - newdata$cspd)^2, na.rm = T))
  r2 <- 1 - sum((train_pred - data$cspd)^2, na.rm = T)/sum((mean(data$cspd, na.rm = T) - data$cspd)^2, na.rm = T)
  r2_wore <- 1 - sum((train_pred_wore - data$cspd)^2, na.rm = T)/sum((mean(data$cspd, na.rm = T) - data$cspd)^2, na.rm = T)
  c(train_rmse, vald_rmse, r2, train_rmse_wore, vald_rmse_wore, r2_wore)
}

mdeval2 <- function(formula = formula(m1), df = fitdf, k = 2) {
  qnb <- unique(df$qnb)
  n <- length(qnb)
  grp <- sample(1:k, n, replace = T)
  rmse <- as.numeric()
  for (i in 1:k) {
    train_tmp <- df[df$qnb %in% qnb[grp != i],]
    vald_tmp <- df[df$qnb %in% qnb[grp == i],]
    refit <- lmer(formula, data = train_tmp)
    pred <- predict(refit, newdata = vald_tmp, allow.new.levels = T)
    rmse_tmp <- sqrt(mean((pred - vald_tmp$cspd)^2, na.rm = T))
    rmse <- c(rmse, rmse_tmp)
  }
  rmse
}


### packages ----------


loadpackage("here")
loadpackage("ggplot2")
loadpackage("scico")
loadpackage("sf")
loadpackage("raster")
loadpackage("dplyr")
loadpackage("lme4")
loadpackage("lmerTest")
loadpackage("ggspatial")
loadpackage("reshape2")
loadpackage("car")
loadpackage("MuMIn")

source(here("scripts", "general_functions.R"))

### plot params -----------

col1_dark <- scico(1, palette = "batlow", begin = 0.2)

col1_light <- scico(1, palette = "batlow", begin = 0.4)

pal_div <- "roma"
pal_con <- "oslo"
pal_disc <- "batlow"

pal_bg <- 0.2
pal_end <- 0.8

fig_d1 <- 3.54331
fig_d2 <- 5.51181
fig_d3 <- 7.48031

mfn <- 2512

knitr::opts_chunk$set(fig.width = fig_d1,
                      fig.height = fig_d1,
                      dpi = 1000)

## 1.2. load data ------------------------

### data folder --------

rdir <- "data/raw"
pdir <- "data/processed"



### utility survey ----------

load(file = here(
  pdir,
  "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_AquaWal_prd.Rdata"
))


# qnb 13685 has two row and cannot know for sure which row can be correctedly connected with the histcons => remove

us <- us[us$qnb != 13685,]

### utility survey sf ----------------

us_coord <-
  st_read(
    here(
      pdir,
      "utilities_survey_Aquawal_CEHD_Wal/addresses/coordinates/surv14_coordinates/surv14_coordinates.shp"
    )
  )

### administrative borders -----------

load(file = here(
  pdir,
  "admin_border_Be/admin_Be.Rdata"
))

mun_wal <- muni[muni$regicd %in% 3000, ]


### historical consumption ----------

load(file = here(
  pdir,
  "water_histcons_Wal", "swde.Rdata"
))

load(file = here(
  pdir,
  "water_histcons_Wal", "CILE", "cile.Rdata"
))

load(file = here(
  pdir,
  "water_histcons_Wal", "inbw.Rdata"
))

### weather data -----------

load(file = here(pdir, "weather_general_NasaPower_Be", "weather_yearly_NasaPower_Be.Rdata"))

weather <- Reduce(rbind.data.frame, yearly_ls[names(yearly_ls) %in% unique(us$municd)])

### price data ---------------


price <- read.csv(
  here(pdir, "water_price_Aquawal_Wal/water_price_Wal_12_17.csv"))



### builtup density at ss -----------

load(file = here(
  pdir,
  "urban_5cat_Ahmed_Wal/urban_5cat_ss_Wal.Rdata"
))


# 2. STM using only family living during 2014 -----------------

## 2.1. pre-process data ----------

### historical consumption --------

swde2_df$dtbtor <- "SWDE"

swde2_df <- swde2_df[, c("qnb", "year", "csmpt", "ndays", "cspd", "dtbtor")]


cile2_df$dtbtor <- "CILE"

cile2_df <- cile2_df[, c("qnb", "year", "csmpt", "ndays", "cspd", "dtbtor")]

inbw2_df$dtbtor <- "inBW"

inbw2_df <- inbw2_df[, c("qnb", "year", "csmpt", "ndays", "cspd", "dtbtor")]


histcons <- Reduce(rbind.data.frame, list(swde2_df, cile2_df, inbw2_df))
histcons <- histcons[order(histcons$qnb, histcons$year),]



### survey data ---------


# select only necessary columns with family characteristics, rainwater use, urban density
## don't include dwelling characteristics since (1) can't have projection for housing stock, (2) they contribute marginally in explaining, (3) hopefully the changes in urban explain the change in housing stocks?

## with the same logic, should I care about climate? and price????



us_subs <- us[, c("qnb", grep("by_", colnames(us), value = T), "inccat", "rwtank", "rwtuse", "bath",  "pmnpol", "garden", "nbbdrm", "nbbtrm", "dwcsy5", "livara", "bldarea", "parcarea", "pointsp", grep("buf", colnames(us), value = T), "ststcd", "municd")]



### merging ------------

fitdf <- left_join(histcons, us_subs)

fitdf <- left_join(fitdf, weather)

fitdf <- left_join(fitdf, price)

fitdf <- left_join(fitdf, bltupss[, c("ststcd", "LU2010_5cls_x25")])

colnames(fitdf)[ncol(fitdf)] <- "bltupss"



### household size --------------

by_df <- fitdf[, grep("by_", colnames(fitdf))]

age_brks = c(0, 20)
age <- fitdf$year - by_df
age[age < 0] <- NA
    
hhsize <- data.frame(year = fitdf$year)
age_brks <- c(age_brks, max(age, na.rm = T) + 1)
n <- length(age_brks)
    
    
for (i in seq_len(n - 1)) {
  agr_mat <- age >= age_brks[i] & age < age_brks[i + 1]
  npers <- apply(agr_mat, 1, sum, na.rm = T)
  hhsize <- cbind(hhsize, npers)
  colnames(hhsize)[ncol(hhsize)] <-
    paste("hhs", age_brks[i], age_brks[i + 1] - 1, sep = "_")
}

hhsize$hhs_tot <-
  apply(hhsize[, grep("hhs_", colnames(hhsize))], 1, sum, na.rm = T)
hhsize[hhsize$hhs_tot %in% 0, grep("hhs_", colnames(hhsize))] <- NA
    
    
fitdf <- cbind.data.frame(fitdf, hhsize[,-1])

### other vars --------

fitdf$t <- fitdf$year - min(fitdf$year)
# fitdf$hhs_20_100 <- fitdf$hhs_18_65 + fitdf$hhs_66_100
fitdf$pointsp <- as.factor(fitdf$pointsp)

fitdf$buf1k_max <- as.factor(fitdf$buf1k_max)
fitdf$buf1k_mode <- as.factor(fitdf$buf1k_mode)
fitdf$buf300_max <- as.factor(fitdf$buf300_max)
fitdf$buf300_mode <- as.factor(fitdf$buf300_mode)
fitdf$bltupss <- as.factor(fitdf$bltupss)

### remove extreme data ------------

# remove based on expert advice 300m3/year ~ 822 L/day

fitdf <- fitdf[!is.na(fitdf$cspd),]

fitdf <- fitdf[fitdf$ndays > 365/2 & fitdf$cspd < 822,]

save(fitdf, file = here(pdir, "water_histcons_Wal", "water_histcons.Rdata"))

fitdf_narm <- fitdf[complete.cases(fitdf[, c("hhs_tot", "municd", "t", "pointsp", "mnt_max_sm", "bill7018")]),]

### divide to train and test --------

qnb <- unique(fitdf_narm$qnb)

sep_ls <- lapply(qnb, function(x) {
  tmp <- fitdf_narm[fitdf_narm$qnb %in% x,]
  tmp <- tmp[order(tmp$year),]
  train_tmp <- tmp[-nrow(tmp),]
  val_tmp <- tmp[nrow(tmp),]
  list(train_tmp, val_tmp)
})

train <- Reduce(rbind.data.frame, lapply(sep_ls, "[[", 1))


vald <-  Reduce(rbind.data.frame, lapply(sep_ls, "[[", 2))


## 2.2. lmer fit ----------

### year + random --------------

#### m1

m1 <-  lmer(cspd ~ t + (1 + t|municd:qnb) + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m1)

ranova(m1)

mdstat1 <- mdeval1(m1, re.form =  ~ (1 + t|municd))

crvld1 <- mdeval2(formula(m1), df = fitdf_narm, k = 100)



#### m2

m2 <-  lmer(cspd ~ t + (1|municd:qnb) + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m2)

ranova(m2)

mdstat2 <- mdeval1(m2, re.form =  ~ (1 + t|municd))

crvld2 <- mdeval2(formula(m2), df = fitdf_narm, k = 100)

anova(m1, m2)

#### m3

m3 <-  lmer(cspd ~ t  + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m3)

ranova(m3)

mdstat3 <- mdeval1(m3, re.form =  ~ (1 + t|municd))

crvld3 <- mdeval2(formula(m3), fitdf_narm, k = 100)

anova(m3, m2)



# CONCLUSION: model with random effects at household levels predict very well for the same household but worse for new household. so if I build a web app to predict your next year water consumption based on your historical data, then it's great. but for now, I want to generalize for other households in the same region so model with random effects at municipaltiy works better.


### plus hhs --------------

#### m4

m4 <-  lmer(cspd ~ t + hhs_tot  + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m4)

ranova(m4)

mdstat4 <- mdeval1(m4, re.form =  ~ (1 + t|municd))

crvld4 <- mdeval2(formula(m4), fitdf_narm, k = 100)

anova(m3, m4)

#### m5

m5 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m5)

ranova(m5)

mdstat5 <- mdeval1(m5, re.form =  ~ (1 + t|municd))

crvld5 <- mdeval2(formula(m5), fitdf_narm, k = 100)

anova(m3, m5)

#### m6

# m6 <-  lmer(cspd ~ t + hhs_0_19 + hhs_18_65 + hhs_66_100  + (1 + t|municd), data = train, REML = T, na.action = na.exclude)
# 
# summary(m6)
# 
# ranova(m6)
# 
# mdstat6 <- mdeval1(m6, re.form =  ~ (1 + t|municd))
# 
# crvld6 <- mdeval2(formula(m6), fitdf_narm, k = 100)
# 
# anova(m3, m6)

#### m7

m7 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + (1 + t  + hhs_0_19 + hhs_20_100|municd), data = train, REML = T, na.action = na.exclude)

summary(m7)

ranova(m7)

mdstat7 <- mdeval1(m7, re.form =  ~ (1 + t + hhs_0_19 + hhs_20_100|municd))

crvld7 <- mdeval2(formula(m7), fitdf_narm, k = 100)

anova(m7, m5)

#### m8

m8 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + (1 + t   + hhs_20_100|municd), data = train, REML = T, na.action = na.exclude)

summary(m8)

ranova(m8)

mdstat8 <- mdeval1(m8, re.form =  ~ (1 + t + hhs_20_100|municd))

crvld8 <- mdeval2(formula(m8), fitdf_narm, k = 100)

anova(m8, m7)

### CONCLUSION: m5 has lowest mean and var of crvld, also my life is much easier in explaining them

### plus rainwater  -------------

#### m9

m9 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m9)

ranova(m9)

mdstat9 <- mdeval1(m9, re.form =  ~ (1 + t|municd))

crvld9 <- mdeval2(formula(m9), fitdf_narm, k = 100)

anova(m9, m5)

#### m10
m10 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtuse + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m10)

ranova(m10)

mdstat10 <- mdeval1(m10, re.form =  ~ (1 + t|municd))

crvld10 <- mdeval2(formula(m10), fitdf_narm, k = 100)

anova(m10, m5)

## CONCLUSION rwtuse better but it's not easy to simulate for the whole population, too many assumptions and potential of not correctly representative???? Also can't add rwt as random coefficient due to zero cells

### plus urban -----------
#### m11
m11 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + pointsp + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m11)

ranova(m11)

mdstat11 <- mdeval1(m11, re.form =  ~ (1 + t|municd))

crvld11 <- mdeval2(formula(m11), fitdf_narm, k = 100)

anova(m9, m11)

#### m111
m111 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m111)

ranova(m111)

mdstat111 <- mdeval1(m111, re.form =  ~ (1 + t|municd))

crvld111 <- mdeval2(formula(m111), fitdf_narm, k = 100)

anova(m9, m111)

#### m12

m12 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + buf300_max + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m12)

ranova(m12)

mdstat12 <- mdeval1(m12, re.form =  ~ (1 + t|municd))

crvld12 <- mdeval2(formula(m12), fitdf_narm, k = 100)

anova(m9, m12)



#### m13

m13 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + buf300_mean + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m13)

ranova(m13)

mdstat13 <- mdeval1(m13, re.form =  ~ (1 + t|municd))

anova(m9, m13)

crvld13 <- mdeval2(formula(m13), fitdf_narm, k = 100)



#### m14

m14 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + buf300_mode + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m14)

ranova(m14)

mdstat14 <- mdeval1(m14, re.form =  ~ (1 + t|municd))

anova(m9, m14)

crvld14 <- mdeval2(formula(m14), fitdf_narm, k = 100)


#### m15

m15 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + buf1k_max + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m15)

ranova(m15)

mdstat15 <- mdeval1(m15, re.form =  ~ (1 + t|municd))

anova(m9, m15)

crvld15 <- mdeval2(formula(m15), fitdf_narm, k = 100)



#### m16

m16 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + buf1k_mean + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m16)

ranova(m16)

mdstat16 <- mdeval1(m16, re.form =  ~ (1 + t|municd))

crvld16 <- mdeval2(formula(m16), fitdf_narm, k = 100)

anova(m9, m16)


#### m17

m17 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + buf1k_mode + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m17)

ranova(m17)

mdstat17 <- mdeval1(m17, re.form =  ~ (1 + t|municd))

fitdf_narm1 <- fitdf_narm

fitdf_narm1$buf1k_mode <- recode(fitdf_narm1$buf1k_mode, "c('0', '1') = '1'")
crvld17 <- mdeval2(formula(m17), fitdf_narm1, k = 100)

anova(m9, m17)


## CONCLUSION:: believe me. I tried a lot, also logistic regression with rwtank, pmnpol as response variable. all urban variables are equally bad with pointsp is slightly better. Move on, PLEASE!!!!!!!!!!!!!

### plus weather ---------

#### test which weather var significant 



test <- sapply(colnames(fitdf_narm)[36:76], function(a) {
  tryCatch({
    form <- formula(paste0(". ~ . + ", a))
    newfit <- update(m11, form)
    anv <- anova(newfit, m11)
    res <- anv$`Pr(>Chisq)`[2]
  }, error = function(e) {
    NA
  })
})


test[test < 0.05]

#### m18
m18 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + mnt_max_sm + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m18)

ranova(m18)

mdstat18 <- mdeval1(m18, re.form =  ~ (1 + t|municd))

anova(m18, m11)

crvld18 <- mdeval2(formula(m18), fitdf_narm, k = 100)

#### m19

m19 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + mnt_min_sm + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m19)

ranova(m19)

mdstat19 <- mdeval1(m19, re.form =  ~ (1 + t|municd))

anova(m19, m11)

crvld19 <- mdeval2(formula(m19), fitdf_narm, k = 100)


#### m20

m20 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + mnt_sm + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m20)

ranova(m20)

mdstat20 <- mdeval1(m20, re.form =  ~ (1 + t|municd))

anova(m20, m11)

crvld20 <- mdeval2(formula(m20), fitdf_narm, k = 100)

#### m21
m21 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + mnrh + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m21)

ranova(m21)

mdstat21 <- mdeval1(m21, re.form =  ~ (1 + t|municd))

anova(m21, m11)

crvld21 <- mdeval2(formula(m21), fitdf_narm, k = 100)


### plus price -----------

#### m21
m22 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + bill7018 + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m22)

ranova(m22)

mdstat22 <- mdeval1(m22, re.form =  ~ (1 + t|municd))

anova(m22, m11)

crvld22 <- mdeval2(formula(m22), fitdf_narm, k = 100)

### random slopes -----------

#### m23

m23 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1|municd), data = train, REML = T, na.action = na.exclude)

summary(m23)

ranova(m23)
anova(m23, m11)
mdstat23 <- mdeval1(m23, re.form =  ~ (1 |municd))

crvld23 <- mdeval2(formula(m23), fitdf_narm, k = 100)



#### m24

m24 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1 + t  + hhs_20_100|municd), data = train, REML = T, na.action = na.exclude)

summary(m24)

ranova(m24)
anova(m11, m24)
mdstat24 <- mdeval1(m24, re.form =  ~ (1 + t + hhs_20_100|municd))

crvld24 <- mdeval2(formula(m24), fitdf_narm, k = 100)


#### m25

m25 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1 + t  + hhs_20_100 + hhs_0_19|municd), data = train, REML = T, na.action = na.exclude)

summary(m25)

ranova(m25)
anova(m11, m25)
mdstat25 <- mdeval1(m25, re.form =  ~ (1 + t + hhs_20_100 + hhs_0_19|municd))

crvld25 <- mdeval2(formula(m25), fitdf_narm, k = 100)

#### m26
fitdf_narm$rwtank_num <- as.numeric(fitdf_narm$rwtank)
train$rwtank_num <- as.numeric(train$rwtank)
vald$rwtank_num <- as.numeric(vald$rwtank)

m26 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank_num + bltupss + (1 + t  + rwtank_num|municd), data = train, REML = T, na.action = na.exclude)

summary(m26)

ranova(m26)
anova(m11, m26)

mdstat26 <- mdeval1(m26, re.form =  ~ (1 + t + rwtank_num|municd))

crvld26 <- mdeval2(formula(m26), fitdf_narm, k = 100)

#### m27
fitdf_narm$bltupss_num <- as.numeric(fitdf_narm$bltupss)
train$bltupss_num <- as.numeric(train$bltupss)
vald$bltupss_num <- as.numeric(vald$bltupss)

m27 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss_num + (1 + t  + bltupss_num|municd), data = train, REML = T, na.action = na.exclude)

summary(m27)

ranova(m27)
anova(m11, m27)

mdstat27 <- mdeval1(m27, re.form =  ~ (1 + t + bltupss_num|municd))


crvld27 <- mdeval2(formula(m27), fitdf_narm, k = 100)

#### m28 

m28 <- lm(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss, data = train, na.action = na.exclude)

summary(m28)

train_pred <- predict(m28)
train_rmse <- sqrt(mean((train_pred - train$cspd)^2, na.rm = T))
vald_pred <- predict(m28, newdata = vald)
vald_rmse <- sqrt(mean((vald_pred - vald$cspd)^2, na.rm = T))

r2 <- 1 - sum((train_pred - train$cspd)^2, na.rm = T)/sum((mean(train$cspd, na.rm = T) - train$cspd)^2, na.rm = T)

mdstat28 <- c(rep(NA, 3), train_rmse, vald_rmse, r2)

qnb <- unique(fitdf_narm$qnb)
n <- length(qnb)
grp <- sample(1:100, n, replace = T)
rmse <- as.numeric()
for (i in 1:100) {
  train_tmp <- fitdf_narm[fitdf_narm$qnb %in% qnb[grp != i],]
  vald_tmp <- fitdf_narm[fitdf_narm$qnb %in% qnb[grp == i],]
  refit <- lm(formula(m28), data = train_tmp)
  pred <- predict(refit, newdata = vald_tmp)
  rmse_tmp <- sqrt(mean((pred - vald_tmp$cspd)^2, na.rm = T))
  rmse <- c(rmse, rmse_tmp)
}

crvld28 <- rmse

#### m29

m29 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1|municd:qnb) + (1 + t|municd), data = train, REML = T, na.action = na.exclude)

summary(m29)

ranova(m29)
anova(m29, m11)
mdstat29 <- mdeval1(m29, re.form =  ~ (1 + t|municd))

crvld29 <- mdeval2(formula(m29), fitdf_narm, k = 100)

#### m30

m30 <-  lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1|municd:qnb) + (1|municd), data = train, REML = T, na.action = na.exclude)

summary(m30)

ranova(m30)
anova(m29, m30)
mdstat30 <- mdeval1(m30, re.form =  ~ (1|municd))

crvld30 <- mdeval2(formula(m30), fitdf_narm, k = 100)

### evaluation -------

eval1 <- data.frame(t(cbind.data.frame(mget(grep("mdstat", ls(), value = T)))))
colnames(eval1) <- c("rmse_train", "rmse_vald", "r2", "rmse_train_rem", "rmse_vald_rem", "r2_rem")

eval1$model <- as.numeric(gsub("mdstat", "", rownames(eval1)))

eval1 <- eval1[order(eval1$model),]

eval2 <- cbind.data.frame(mget(grep("crvld", ls(), value = T)))
eval2 <- melt(eval2, id.vars = NULL)
eval2$model <- as.numeric(gsub("crvld", "", eval2$variable))

ggplot(eval2, aes(x = as.factor(model), y = value)) + geom_boxplot() + geom_hline(yintercept = 94.70144, color = "blue", linetype = "dashed")

agg_eval2 <- eval2 %>%
  group_by(model) %>%
  summarise(mean = mean(value),
            sd = sd(value))

median(eval2[eval2$model == 111, "value"])


## model with price has lower RMSE but maybe because it remove data before 2012, to compare fairly => need to remove missing value and refit previous models


### final model -----------

# only care about variables which can be used to project, which means all variables which I don't have future projected values should not be considered
# including: income, all dwelling ones??

fn1 <- lm(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss, data = fitdf, na.action = na.exclude)

summary(fn1)

fn2 <- lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1|municd), data = fitdf, na.action = na.exclude)

summary(fn2)

r.squaredGLMM(fn2)

fn3 <- lmer(cspd ~ t + hhs_0_19 + hhs_20_100  + rwtank + bltupss + (1|municd) + (1|municd:qnb), data = fitdf, na.action = na.exclude)

summary(fn3)

r.squaredGLMM(fn3)


## 2.3. outputs ---------------

### aggregated by municipality ---------

agg_bmun <- fitdf %>%
  group_by(municd) %>%
  summarise(count = n())

agg_bmun <- agg_bmun[order(agg_bmun$count, decreasing = T),]

agg_bmun <- left_join(agg_bmun, st_drop_geometry(mun_wal[, c("municd", "muninm")]))



### cspd vs time, group by dtbtor ---------
 
ggplot(data = fitdf[fitdf$year < 2019 & fitdf$year > 2009,], aes(x = year, y = cspd))+
  geom_line(aes(group = qnb), color = "grey", alpha = 0.05) +
  stat_summary(geom = "line", aes(group = dtbtor, color = dtbtor), fun = mean, size = 1)+
  theme_kat() +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)


### cspd vs time, group by municd --------------


plotdf <- inner_join(fitdf, agg_bmun[agg_bmun$count > 100,])

ggplot(data = plotdf[plotdf$year < 2019 & plotdf$year > 2009,], aes(x = year, y = cspd, color = as.factor(municd))) +
  stat_summary(geom = "line", aes(group = as.factor(municd)), fun = mean, size = 0.5)+
  theme_kat(legend.position = "none") +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)



### hhsize through time ------------

hhs_by <- aggregate(hhs_tot ~ year, data = fitdf, FUN = mean)


plot(hhs_by)



## of course it increase after 2014 because I assume that adult kid does not move out, never mind this

### average trend by municd ---------

## should not use the belowing code but use the predicted values after calculating for synthesis population

## the map below depends heavily on whether the household in the survey reperesent the average of municipality which is questionable especially for municipalities with few recorded families

coef_qnb <- coef(final)[[1]]
coef_qnb$qnb <- gsub(".*:", "", row.names(coef_qnb))
coef_qnb$municd <- gsub(":.*", "", row.names(coef_qnb))

plotdf <- coef_qnb %>%
  group_by(municd) %>%
  summarise(mn_t = mean(t))


plotdf$municd <- as.integer(plotdf$municd)

plotdf <- left_join(mun_wal, plotdf)

plotdf$mn_t[plotdf$mn_t < -50] <- NA
plotdf$trend <- plotdf$mn_t*365/1000

plotdf <- inner_join(plotdf, agg_bmun)


ggplot(data = neighbor) +
  geom_sf(fill = c("white", rep("grey90", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) + 
  geom_sf(data = mun_wal, fill = "white")  +
  geom_sf(data = plotdf[plotdf$count > 20,], aes(fill = trend)) +
  scale_fill_scico(palette = pal_div, limits = c(-1, 1) * max(abs(plotdf$trend), na.rm = T), direction = -1, na.value ="white") +
  geom_sf(data = maincity) +
  geom_sf_text(data = maincity, aes(label = muninm), fontface = "bold", size = 3, nudge_y = -2500) +
  annotation_scale(location = "bl", width_hint = 0.15, pad_x = unit(2.3, "in"), pad_y = unit(0.5, "in")) +
  annotation_north_arrow(location = "bl", height = unit(0.5, "in"), width = unit(0.5, "in"), which_north = "true", pad_x = unit(2.65, "in"), pad_y = unit(0.7, "in"), style = north_arrow_fancy_orienteering) +
  theme_void() +
  labs(fill = expression(Consumption~changes~(m^3/year))) +
  theme(legend.position = c(0.18,0.25), plot.margin = unit(rep(-0.9,4), "cm"))

### others ------


hist(fitdf$cspd)

obs_per_fam <- sapply(qnb, function(x) nrow(fitdf[fitdf$qnb %in% x,]))

hist(obs_per_fam)
dup_year <- sapply(qnb, function(x) {
  tmp <- fitdf[fitdf$qnb %in% x,]
  nlvls(tmp$year) == nrow(tmp)
})

summary(dup_year)

# 3. STM using all data --------------

## 3.1. pre-process data ----------

### historical consumption --------


swde1_df$dtbtor <- "SWDE"

swde1_df <- swde1_df[, c("qnb", "year", "csmpt", "ndays", "cspd", "dtbtor")]

cile1_df$dtbtor <- "CILE"

cile1_df <- cile1_df[, c("qnb", "year", "csmpt", "ndays", "cspd", "dtbtor")]

inbw1_df$dtbtor <- "inBW"

inbw1_df <- inbw1_df[, c("qnb", "year", "csmpt", "ndays", "cspd", "dtbtor")]


histcons1 <- Reduce(rbind.data.frame, list(swde1_df, cile1_df, inbw1_df))

histcons1 <- histcons1[order(histcons1$qnb, histcons1$year),]

