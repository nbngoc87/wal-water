#' ---
#' title: "Redistribution effects of water tariffs"
#' author: "Nguyen Bich Ngoc, Jacques Teller"  
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---
  



#+ r setup, include = F, message = F
# notes from last run----------------------

# rmarkdown::render("3 Scripts/redistribution/redistribution.R",output_file=paste0('redistribution ', format(Sys.time(), "%y%m%d %H%M"),'.md'))

# 1. setup ---------


knitr::opts_chunk$set(fig.width=5, fig.height=5, dpi = 300) 


## 1.1. load functions -----------------

# install.packages("here")
library(here)


source(here("3 Scripts", "general_functions.R"))

loadpackage('dplyr')
loadpackage('ggplot2')
loadpackage('reshape2')
loadpackage('raster')
loadpackage('sf')
loadpackage('scico')
loadpackage("kableExtra")


vary_fixed_f <- function(dtbtorname = "SWDE", fixeds = seq (0, 200, 40)) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  
  total <- sum(tmpdf$bill_cur)
  avrprc <- sum(tmpdf$bill_cur)/sum(tmpdf$csmptv)
  cvd_v <- numeric()
  
  for (fixed in fixeds) {
    
    cvd <- (total  - fixed*nrow(tmpdf) - sum((tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30))/sum(0.5*tmpdf$csmptv + 0.5*(tmpdf$csmptv - 30)*tmpdf$ab30)
    cvd_v <- c(cvd_v, cvd)
    tmpdf$bill_new <- fixed  + 0.5*tmpdf$csmptv*cvd + 0.5*(tmpdf$csmptv - 30)*cvd*tmpdf$ab30 + (tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30
    tmpdf$diff <- tmpdf$bill_new - tmpdf$bill_cur
    tmpdf$difpct <- tmpdf$diff*100/tmpdf$bill_cur
    tmpdf$difpcinc <- tmpdf$diff*100/(tmpdf$income*12)
    tmpdf$TEH_new <- tmpdf$bill_new*100/(tmpdf$income*12)
    tmpdf$subs_new <- tmpdf$csmptv*avrprc - tmpdf$bill_new
    tmpdf$avrprc_new <- tmpdf$bill_new/tmpdf$csmptv
    colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <- paste(c("bill", "difab", "difpc", "difpcinc", "TEH", "subs", "avrprc"), "fixed", fixed, sep = "_")
  }
  
  tmpdf <- tmpdf[, c("id", grep("_fixed_", colnames(tmpdf), value = T))]
  
  list(tmpdf, cvd_v)
}



vary_rwtt_f <- function(dtbtorname = "SWDE", rwtts = seq (0, 200, 40)) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  
  total <- sum(tmpdf$bill_cur)
  avrprc <- sum(tmpdf$bill_cur)/sum(tmpdf$csmptv)
  cvd_v <- numeric()
  
  for (rwtt in rwtts) {
    
    rwtt_tot <- sum(rwtt*tmpdf$rwt_num)
    
    cvd <- (total - rwtt_tot - sum(30*tmpdf$CVA + (tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30))/sum(20 + 0.5*tmpdf$csmptv + 0.5*(tmpdf$csmptv - 30)*tmpdf$ab30)
    cvd_v <- c(cvd_v, cvd)
    tmpdf$bill_new <- rwtt*tmpdf$rwt_num + 30*tmpdf$CVA + 20*cvd + 0.5*tmpdf$csmptv*cvd + 0.5*(tmpdf$csmptv - 30)*cvd*tmpdf$ab30 + (tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30
    tmpdf$diff <- tmpdf$bill_new - tmpdf$bill_cur
    tmpdf$difpct <- tmpdf$diff*100/tmpdf$bill_cur
    tmpdf$difpcinc <- tmpdf$diff*100/(tmpdf$income*12)
    tmpdf$TEH_new <- tmpdf$bill_new*100/(tmpdf$income*12)
    tmpdf$subs_new <- tmpdf$csmptv*avrprc - tmpdf$bill_new
    tmpdf$avrprc_new <- tmpdf$bill_new/tmpdf$csmptv
    colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <- paste(c("bill", "difab", "difpc", "difpcinc", "TEH", "subs", "avrprc"), "rwtt", rwtt, sep = "_")   
    
  }
  
  tmpdf <- tmpdf[, c("id", grep("_rwtt_", colnames(tmpdf), value = T))]
  list(tmpdf, cvd_v)
}

up_f <- function(dtbtorname = "SWDE", fixeds = seq (0, 100, 50), revincr = 0) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  
  bill_tot <- (1 + revincr)*sum(tmpdf$bill_cur)
  cons_tot <- sum(tmpdf$csmptv)
  avrprc <- bill_tot/cons_tot
  up_v <- numeric()
  
  for (fixed in fixeds) {
    
    fixed_tot <- fixed*nrow(tmpdf)
    
    up <- (bill_tot - fixed_tot)/cons_tot
    up_v <- c(up_v, up)
    tmpdf$bill_new <- fixed + tmpdf$csmptv*up
    tmpdf$diff <- tmpdf$bill_new - tmpdf$bill_cur
    tmpdf$difpct <- tmpdf$diff*100/tmpdf$bill_cur
    tmpdf$difpcinc <- tmpdf$diff*100/(tmpdf$income*12)
    tmpdf$TEH_new <- tmpdf$bill_new*100/(tmpdf$income*12)
    tmpdf$subs_new <- tmpdf$csmptv*avrprc - tmpdf$bill_new
    tmpdf$avrprc_new <- tmpdf$bill_new/tmpdf$csmptv
    colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <- paste(c("bill", "difab", "difpc", "difpcinc", "TEH", "subs", "avrprc"), "up", fixed, sep = "_")   
    
  }
  tmpdf <- tmpdf[, c("id", grep("_up_", colnames(tmpdf), value = T))]
  list(tmpdf, up_v)
}

ibtcon_f <- function(dtbtorname = "SWDE", fixeds = seq (0, 100, 50), bl1size = 30, blratio = 3.5, revincr = 0) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  tmpdf$bl2 <- as.numeric(tmpdf$csmptv > bl1size)
  
  bill_tot <- (1 + revincr)*sum(tmpdf$bill_cur)
  cons_tot <- sum(tmpdf$csmptv)
  avrprc <- bill_tot/cons_tot
  bl1_v <- numeric()
  
  for (fixed in fixeds) {
    
    fixed_tot <- fixed*nrow(tmpdf)
    
    bl1 <- (bill_tot - fixed_tot)/sum(tmpdf$csmptv + (tmpdf$csmptv - bl1size)*(blratio - 1)*tmpdf$bl2)
    bl1_v <- c(bl1_v, bl1)
    tmpdf$bill_new <- fixed + tmpdf$csmptv*bl1 + (tmpdf$csmptv - bl1size)*(blratio - 1)*bl1*tmpdf$bl2
    tmpdf$diff <- tmpdf$bill_new - tmpdf$bill_cur
    tmpdf$difpct <- tmpdf$diff*100/tmpdf$bill_cur
    tmpdf$difpcinc <- tmpdf$diff*100/(tmpdf$income*12)
    tmpdf$TEH_new <- tmpdf$bill_new*100/(tmpdf$income*12)
    tmpdf$subs_new <- tmpdf$csmptv*avrprc - tmpdf$bill_new
    tmpdf$avrprc_new <- tmpdf$bill_new/tmpdf$csmptv
    colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <- paste(c("bill", "difab", "difpc", "difpcinc", "TEH", "subs", "avrprc"), "ibtcon", fixed, sep = "_")   
    
  }
  tmpdf <- tmpdf[, c("id", grep("_ibtcon_", colnames(tmpdf), value = T))]
  list(tmpdf, bl1_v)
}

ibtcap_f <- function(dtbtorname = "SWDE", fixeds = seq (0, 100, 50), bl1size = 12.5, blratio = 3.5, revincr = 0) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  tmpdf$bl2 <- as.numeric(tmpdf$cspc > bl1size)
  
  bill_tot <- (1 + revincr)*sum(tmpdf$bill_cur)
  cons_tot <- sum(tmpdf$csmptv)
  avrprc <- bill_tot/cons_tot
  bl1_v <- numeric()
  
  for (fixed in fixeds) {
    
    fixed_tot <- fixed*nrow(tmpdf)
    
    bl1 <- (bill_tot - fixed_tot)/sum(tmpdf$cspc*tmpdf$hhs + (tmpdf$cspc - bl1size)*tmpdf$hhs*(blratio - 1)*tmpdf$bl2)
    bl1_v <- c(bl1_v, bl1)
    tmpdf$bill_new <- fixed + tmpdf$cspc*tmpdf$hhs*bl1 + (tmpdf$cspc - bl1size)*tmpdf$hhs*(blratio - 1)*bl1*tmpdf$bl2
    tmpdf$diff <- tmpdf$bill_new - tmpdf$bill_cur
    tmpdf$difpct <- tmpdf$diff*100/tmpdf$bill_cur
    tmpdf$difpcinc <- tmpdf$diff*100/(tmpdf$income*12)
    tmpdf$TEH_new <- tmpdf$bill_new*100/(tmpdf$income*12)
    tmpdf$subs_new <- tmpdf$csmptv*avrprc - tmpdf$bill_new
    tmpdf$avrprc_new <- tmpdf$bill_new/tmpdf$csmptv
    colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <- paste(c("bill", "difab", "difpc", "difpcinc", "TEH", "subs", "avrprc"), "ibtcap", fixed, sep = "_")   
    
  }
  tmpdf <- tmpdf[, c("id", grep("_ibtcap_", colnames(tmpdf), value = T))]
  list(tmpdf, bl1_v)
}


## 1.2 load data --------

### data folder ----
rdir <- '2 Data/1 Raw'
pdir <- '2 Data/2 Processed'

### survey data ----------

surv14 <- read.csv(file = here(pdir, "UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_AquaWal_prd.csv"))

### price data ---------

price <- read.csv(file = here(pdir, 'Water_price_AWal_Wal/water_price_Wal_12_17.csv'))


### location  ---------
load(file= here(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/surv14_coord_PICC.Rdata'))

### urbanization -------------

urban_10 <- raster(here(pdir, "Urban_5cat_Ahmed_Wal/LU2010_5cls_x25.flt"))

crs(urban_10) <- st_crs(surv14_coords)

# 2. data processing ----------
## 2.1 merge data ----------

surv14$year <- 2014

df <- dplyr::left_join(surv14, price)[, c('id', 'CVD', 'CVA', 'csmptv', 'inceqa', 'rwtank', 'hhs_0_19', 'hhs_20_95', 'income', 'inccat', 'dtbtor', "rprage")]
df$hhs <- df$hhs_0_19 + df$hhs_20_95

df$csmptv[df$csmptv > 300 | df$csmptv < 5] <- NA



urban <- cbind.data.frame(id = surv14_coords$id, urban = extract(urban_10, surv14_coords))

urban <- urban[urban$urban > -1,]

df <- inner_join(df, urban)

df <- df[!is.na(df$csmptv) & !is.na(df$inceqa) & !is.na(df$CVD),]

df <- df[df$dtbtor %in% c("CILE", "SWDE", "IECBW"),]

## 2.2 variable processing -------

df$cspc <- df$csmptv/df$hhs

df$ab30 <- as.numeric(df$csmptv > 30)
df$cspc_ab125 <- as.numeric(df$cspc > 12.5)

df$bill_cur <- (20*df$CVD + 30*df$CVA)  + 0.5*df$csmptv*df$CVD + (df$csmptv - 30)*(0.5*df$CVD + df$CVA)*df$ab30
df$billpc <- df$bill_cur/df$hhs

df$FSA <- 0.0125*df$csmptv

summary(df$bill_cur*0.06)

df$incpc <- df$income/df$hhs

df <- df %>% mutate(incqnt = ntile(income, 5))
df$incqnt <- as.factor(df$incqnt)

df <- df %>% mutate(ieaqnt = ntile(inceqa, 5))
df$ieaqnt <- as.factor(df$ieaqnt)

df <- df %>% mutate(ipcqnt = ntile(incpc, 5))
df$ipcqnt <- as.factor(df$ipcqnt)

df <- df %>% mutate(poorest = ntile(income, 10))
df$poorest[df$poorest > 1] <- 0

df$TEH <- df$bill_cur*100/(df$income*12)

lvls(df$dtbtor)
df$dtbtor <- factor(df$dtbtor, levels = lvls(df$dtbtor)[c(3,1,2)])

df$avrprc <- df$bill_cur/df$csmptv
df$mgnprc <- 0.5*df$CVD + (0.5*df$CVD + df$CVA)*df$ab30
df$avrprcpc <- df$avrprc/df$hhs



df$urban <- factor(df$urban)
df$urban <- car::recode(df$urban, "c('0', '1') = 'low'; c('2','3') = 'medium'; c('4','5') = 'high' ")



df$urban <- factor(df$urban, levels = lvls(df$urban)[c(2,3,1)])

avr_price <- df %>%
  group_by(dtbtor) %>% 
  summarise(nfam = n(),
            CVD = unique(CVD),
            CVA = unique(CVA),
            utlt_avrprc = sum(bill_cur)/sum(csmptv),
            bl1 = mean(CVD)*0.5,
            bl2 = mean(CVD) + mean(CVA))

avr_price$blratio <- avr_price$bl2/avr_price$bl1


df <- left_join(df, avr_price[,c("dtbtor", "utlt_avrprc")])

df$subs <- df$utlt_avrprc*df$csmptv - df$bill_cur

df$hhscat <- df$hhs
df$hhscat[df$hhscat > 4] <- "5+"
df$hhscat <- factor(df$hhscat)

df$inccat <- factor(df$inccat, levels = lvls(df$inccat)[c(4, 3, 1, 2)])

df$rwt_num <- as.numeric(df$rwtank %in% "yes")

## 2.3. varying fixed --------
fixeds <- seq(0,200,50)

vary_fixed_ls <- lapply(c("SWDE", "CILE", "IECBW"),vary_fixed_f, fixeds = seq(0,200,50))

fixed_tariff <- as.data.frame(sapply(vary_fixed_ls, function(x) x[[2]]))

fixed_tariff <- rbind(t(avr_price$CVD
), fixed_tariff)

colnames(fixed_tariff) <- c("SWDE", "CILE", "IECBW")

fixed_tariff$fixed <- c(20*mean(df$CVD) + 30*mean(df$CVA), fixeds)

fixed_tariff$CVA <- unique(df$CVA)

fixed_tariff$scenario <- c("As in 2014", 1:(nrow(fixed_tariff)-1))



fixed_tariff <- fixed_tariff[, c(6,4,1:3,5)]


vary_fixed_df <- Reduce(rbind, lapply(vary_fixed_ls, function(x) x[[1]]))



## 2.4. rainwater tank tax ---------

rwtts <- seq(0,200,50)

vary_rwtt_ls <- lapply(c("SWDE", "CILE", "IECBW"),vary_rwtt_f, rwtts = rwtts)

rwtt_tariff <- sapply(vary_rwtt_ls, function(x) x[[2]])

ndtb <- matrix(table(df$dtbtor))

fixed_new <- (rwtt_tariff%*%ndtb*20 + unique(df$CVA)*30*1534)/1534

rwtt_tariff <- cbind.data.frame(fixed_new, rwtt_tariff)

colnames(rwtt_tariff) <- c("fixed", "SWDE", "CILE", "IECBW")

rwtt_tariff$rwtt <- rwtts

rwtt_tariff$CVA <- unique(df$CVA)

rwtt_tariff <- rwtt_tariff[,c(5, 1:4, 6)]


vary_rwtt_df <- Reduce(rbind, lapply(vary_rwtt_ls, function(x) x[[1]]))



## 2.5 UP vs IBTcap vs IBTcon ---------
# fixeds = seq(0, 100, 50)
# IBTcon 2 block 0-30 & 30+ price bl2 = 3.5*bl1
# IBTcap 2 block 0-12.5 & 12.5+ price bl2 = 3.5*bl1
# sumbill the same for each utilities

fixeds <- seq(0,100,50)
revincrs <- c(0, 0.2, 0.5)
utilities <- c("SWDE", "CILE", "IECBW")


### UP --------- 

up_ls <- lapply(revincrs, function(x) {
  res <- lapply(utilities, up_f, revincr = x, fixeds = fixeds)
  
  tariff <- as.data.frame(sapply(res, function(x) x[[2]]))
  colnames(tariff) <- c("SWDE", "CILE", "IECBW")
  tariff$fixed <- fixeds

  df <- Reduce(rbind, lapply(res, function(x) x[[1]]))
  colnames(df)[2:ncol(df)] <- paste(colnames(df)[2:ncol(df)], x, sep = "_")
  
  list(df, tariff)
  
})

up_df <- Reduce(left_join, lapply(up_ls, function(x) x[[1]]))

up_tariff <- Reduce(rbind, lapply(up_ls, function(x) x[[2]]))

up_tariff$revincr <- rep(revincrs, each = 3)


### ibtcon --------

ibtcon_ls <- lapply(revincrs, function(x) {
  res <- lapply(utilities, ibtcon_f, revincr = x, fixeds = fixeds)
  
  tariff <- as.data.frame(sapply(res, function(x) x[[2]]))
  colnames(tariff) <- c("SWDE", "CILE", "IECBW")
  tariff$fixed <- fixeds
  
  df <- Reduce(rbind, lapply(res, function(x) x[[1]]))
  colnames(df)[2:ncol(df)] <- paste(colnames(df)[2:ncol(df)], x, sep = "_")
  
  list(df, tariff)
  
})

ibtcon_df <- Reduce(left_join, lapply(ibtcon_ls, function(x) x[[1]]))

ibtcon_tariff <- Reduce(rbind, lapply(ibtcon_ls, function(x) x[[2]]))

ibtcon_tariff$revincr <- rep(revincrs, each = 3)


### ibtcap --------

ibtcap_ls <- lapply(revincrs, function(x) {
  res <- lapply(utilities, ibtcap_f, revincr = x, fixeds = fixeds)
  
  tariff <- as.data.frame(sapply(res, function(x) x[[2]]))
  colnames(tariff) <- c("SWDE", "CILE", "IECBW")
  tariff$fixed <- fixeds
  
  df <- Reduce(rbind, lapply(res, function(x) x[[1]]))
  colnames(df)[2:ncol(df)] <- paste(colnames(df)[2:ncol(df)], x, sep = "_")
  
  list(df, tariff)
  
})

ibtcap_df <- Reduce(left_join, lapply(ibtcap_ls, function(x) x[[1]]))

ibtcap_tariff <- Reduce(rbind, lapply(ibtcap_ls, function(x) x[[2]]))

ibtcap_tariff$revincr <- rep(revincrs, each = 3)

# 3. Outputs -------

## 3.1. univariate -----------

#+ inchist, fig.cap = "Household income histogram", echo = F, message = F


ggplot(df, aes(x = income)) +
  geom_histogram(fill = scico(1, palette = "batlow", begin = 0.2), binwidth = 500) +
  labs(x = "Household diposable income (Eur/month)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ inccat

inccatdf <- df[!(is.na(df$inccat)),] %>%
  group_by(inccat) %>%
  summarise(count = n(),
            prop = n()/nrow(df),
            income_avr = mean(income),
            income_min = min(income),
            income_max = max(income),
            inceqa_avr = mean(inceqa),
            inceqa_min = min(inceqa),
            inceqa_max = max(inceqa))

inccatdf

#+ hhsplot, fig.cap = "Household income histogram", echo = F, message = F


ggplot(df, aes(x = hhscat)) +
  geom_bar(fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ cspthist, fig.cap = "Household income histogram", echo = F, message = F


ggplot(df, aes(x = csmptv)) +
  geom_histogram(fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = expression(Consumption~(m^3))) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ tabavrprc, echo = F, message = F, results = 'asis'


cat("\n")
knitr::kable(avr_price, digits = 4, col.names = c("Utilities", "Number of households", "CVD", "CVA", "Average price", "Block 1 price", "Block 2 price", "Block2/Block1"))
cat("\n")


#+ avprcsm, echo = F, message = F

plotdf <- data.frame(csmptv = rep(1:200, 3))
plotdf$CVD <- rep(avr_price$CVD, each = 200)
plotdf$dtbtor <- rep (avr_price$dtbtor, each = 200)
plotdf$CVA <- rep(avr_price$CVA, each = 200)

plotdf$ab30 <- as.numeric(plotdf$csmptv > 30)

plotdf$bill <-  (20*plotdf$CVD + 30*plotdf$CVA)  + 0.5*plotdf$csmptv*plotdf$CVD + (plotdf$csmptv - 30)*(0.5*plotdf$CVD + plotdf$CVA)*plotdf$ab30

plotdf$avrprc <- plotdf$bill/plotdf$csmptv
plotdf$mgnprc <- plotdf$CVD + (0.5*plotdf$CVD + plotdf$CVA)*plotdf$ab30


ggplot(plotdf, aes(x = csmptv, y = avrprc, col = dtbtor)) +
  geom_line(size = 0.85) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(x = expression(Consumption~(m^3)), y = expression(Average~price~(EUR/m^3)), col = "Utilities") +
  xlim(15,150) + 
  ylim(3,9) +
  theme_bw() +
  theme(legend.position = "bottom") 

#+ mgprcsm, echo = F, message = F

ggplot(plotdf, aes(x = csmptv, y = mgnprc, col = dtbtor)) +
  geom_line(size = 0.85) +
  scale_color_scico_d(palette = "batlow", begin = 0.2, end = 0.8) +
  labs(x = expression(Consumption~(m^3)), y = expression(Average~price~(EUR/m^3)), col = "Utilities") +
  xlim(0,150)  + 
  theme_bw() +
  theme(legend.position = "bottom") 



## 3.2 by income quantiles -------

### income quantiles -------


#+ tabinc, echo = F, message = F, results = 'asis'

incqnt_tab <- df %>%
  group_by(incqnt) %>%
  summarise(count = n(),
            npp = sum(hhs),
            mininc = min(income),
            maxinc = max(income))
cat("\n")
knitr::kable(incqnt_tab, caption = "Household income quintile characteristics", digits = 2, col.names = c("Quintile", "Number of households", "Number of people", "Min income (EUR/month)", "Max income (EUR/month)"))

cat("\n")


#+ inceqa1, fig.cap = "Income per equivalent adults for different household income group", echo = F, message = F

ggplot(df, aes(x = incqnt, y = inceqa)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  labs(x = "Household income quintiles", y = "Income per equivalent adult (EUR/per/year)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ inceqa2, fig.cap = "Income per equivalent adults for different household income group", echo = F, message = F

ggplot(df, aes(x = incqnt, y = inceqa)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = "Average income per equivalent adult (EUR/per/year)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ incpc, echo = F, message = F

ggplot(df, aes(x = incqnt, y = incpc)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = "Average income per capita (EUR/per/year)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

### consumption vs income -------

#+ desstat1, warnings = F

# Correlation between water consumption and household income should use spearman?????

cor.test(df$csmptv, df$income, method = "pearson")
cor.test(df$csmptv, df$income, method = "spearman")

# Correlation between water consumption and income per equivalent adult should use spearman?????

cor.test(df$csmptv, df$inceqa, method = "pearson")
cor.test(df$csmptv, df$inceqa, method = "spearman")


#+ blprop1, fig.cap = "Proportion of household paying in which block by quantile", echo = F, message = F

plotdf <- df %>%
  group_by(incqnt) %>%
  summarise(prpbl1 = sum(ab30 < 1)*100/n(),
            prpbl2 = sum(ab30 > 0)*100/n())

plotdf <- melt(plotdf, id.vars = c("incqnt"))

ggplot(plotdf, aes(x=incqnt, y = value, fill = variable)) +
  geom_col() + 
  scale_fill_scico_d(palette =  "batlow", begin = 0.2, end = 0.8, direction = -1, labels = c("block 1", "block 2")) +
  labs(x = "Household income quintiles", y = "Proportion of household (%)", fill = "Tariff block") + 
  theme_bw() +
  theme(legend.position = "bottom") 



#+ blprop2, fig.cap = "Proportion of household paying in which block by income quintile and utilities", echo = F, message = F

plotdf <- df %>%
  group_by(incqnt, dtbtor) %>%
  summarise(prpbl1 = sum(ab30 < 1)*100/n(),
            prpbl2 = sum(ab30 > 0)*100/n())

plotdf <- melt(plotdf, id.vars = c("incqnt", "dtbtor"))

ggplot(plotdf, aes(x=incqnt, y = value, fill = variable)) +
  geom_col() +
  facet_wrap(.~dtbtor) +
  scale_fill_scico_d(palette =  "batlow", begin = 0.2, end = 0.8, direction = -1, labels = c("block 1", "block 2"))  +
  labs(x = "Household income quintiles", y = "Proportion of household (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ csinc1, echo = F, message = F

ggplot(df, aes(x = incqnt, y = csmptv)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Household income quintiles", y = expression(Average~consumption~(m^3)), color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ csinc2, echo = F, message = F

ggplot(df, aes(x = incqnt, y = csmptv)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = expression(Average~consumption~(m^3)) , color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

### hhs vs income --------

#+ hhsinc1, echo = F, message = F

ggplot(df, aes(x = incqnt, fill = hhscat)) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

#+ hhsinc2, echo = F, message = F

ggplot(df, aes(x = incqnt, fill = as.factor(hhs_20_95))) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Number of adults") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

#+ hhsieq, echo = F, message = F

ggplot(df, aes(x = ieaqnt, fill = hhscat)) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Income per equivalent adult quintiles", y = "Proportion of households" , fill = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

### rainwter tank vs income -----------
#+ rwtinc, echo = F, message = F

ggplot(df, aes(x = incqnt, fill = rwtank)) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Rainwater tank") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


### built-up density vs income -----------
#+ densinc, echo = F, message = F

ggplot(df, aes(x = incqnt, fill = as.factor(urban))) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Buit-up density") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

### water bill vs income -----------

#+ billinc1, echo = F, message = F

ggplot(df, aes(x = incqnt, y = bill_cur)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Household income quintiles", y = "Water bill in 2014 (EUR)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ billinc2, echo = F, message = F

ggplot(df, aes(x = incqnt, y = bill_cur)) +
  stat_summary(fun = sum, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = "Total water bills in 2014 (EUR)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

### TEH vs income ------------
#+ TEHinc, echo = F, message = F

ggplot(df, aes(x = incqnt, y = TEH)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Household income quintiles", y = "Ratio of water bill to income (%)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

### marginal price vs income -------------

#+ mgprinc1, echo = F, message = F

ggplot(df, aes(x = incqnt, y = mgnprc)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  labs(x = "Household income quintiles", y = expression(Marginal~price~(EUR/m^3)), color = "") + 
  ylim(1,15) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ mgprinc2, echo = F, message = F

ggplot(df, aes(x = incqnt, y = mgnprc)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = expression(Marginal~price~(EUR/m^3)) , color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ mgrprchhsinc, echo = F, message = F

ggplot(df, aes(x = incqnt, y = mgnprc, fill = hhscat)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Household income quintiles", y = expression(Marginal~price~(EUR/m^3)) , fill = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))



### average price vs income ---------


#+ avprinc1, echo = F, message = F

ggplot(df, aes(x = incqnt, y = avrprc)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  labs(x = "Household income quintiles", y = expression(Average~price~(EUR/m^3)), color = "") + 
  ylim(1,15) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ avprinc2, echo = F, message = F

ggplot(df, aes(x = incqnt, y = avrprc)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = expression(Average~price~(EUR/m^3)) , color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ avprinc3, echo = F, message = F

ggplot(df, aes(x = incqnt, y = avrprcpc)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = expression(Average~price~(EUR/person/m^3)) , color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ avrprchhsinc, echo = F, message = F

ggplot(df, aes(x = incqnt, y = avrprc, fill = hhscat)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Household income quintiles", y = expression(Average~price~(EUR/m^3)) , fill = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))





### subsidy vs income ----------------

#+ subsinc1, echo = F, message = F

ggplot(df, aes(x = incqnt, y = subs)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Household income quintiles", y = "Subsidy for water bill (EUR)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ subsinc2, echo = F, message = F

ggplot(df, aes(x = incqnt, y = subs)) +
  stat_summary(fun = sum, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Household income quintiles", y = "Subsidy for water bill (EUR)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

## 3.3. by built-up density ------

### consumption vs built-up -------

#+ csdens1, echo = F, message = F

ggplot(df, aes(x = urban, y = csmptv)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Built-up density", y = expression(Average~consumption~(m^3)), color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ csdens2, echo = F, message = F

ggplot(df, aes(x = urban, y = csmptv)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Built-up density", y = expression(Average~consumption~(m^3)) , color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

### consumption vs built-up -------

#+ incdens1, echo = F, message = F

ggplot(df, aes(x = urban, y = income)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Built-up density", y = "Household income (EUR/month)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ incdens2, echo = F, message = F

ggplot(df, aes(x = urban, y = income)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Built-up density", y = "Household income (EUR/month)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

### hhs vs builtup density --------

#+ hhsdens, echo = F, message = F

ggplot(df, aes(x = urban, fill = hhscat)) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Built-up density", y = "Proportion of households" , fill = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

### rainwter tank vs builtup density-----------
#+ rwtdens, echo = F, message = F

ggplot(df, aes(x = urban, fill = rwtank)) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Built-up density", y = "Proportion of households" , fill = "Rainwater tank") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

### water bill vs density -----------

#+ billdens, echo = F, message = F

ggplot(df, aes(x = urban, y = bill_cur)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Built-up density", y = "Water bill in 2014 (EUR)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 



### TEH vs density ------------
#+ TEHdens, echo = F, message = F

ggplot(df, aes(x = urban, y = TEH)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Built-up density", y = "Ratio of water bill to income (%)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

### average price vs density ---------


#+ avprdens1, echo = F, message = F

ggplot(df, aes(x = urban, y = avrprc)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  labs(x = "Built-up density", y = expression(Average~price~(EUR/m^3)), color = "") + 
  ylim(1,15) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ avprdens2, echo = F, message = F

ggplot(df, aes(x = urban, y = avrprc)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Built-up density", y = expression(Average~price~(EUR/m^3)) , color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ avprdens3, echo = F, message = F

ggplot(df, aes(x = urban, y = avrprcpc)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Built-up density", y = expression(Average~price~(EUR/person/m^3)) , color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ avrprchhsdens, echo = F, message = F

ggplot(df, aes(x = urban, y = avrprc, fill = hhscat)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Built-up density", y = expression(Average~price~(EUR/m^3)) , fill = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))



### subsidy vs density ----------------
#+ subsdens1, echo = F, message = F


ggplot(df, aes(x = urban, y = subs)) +
  geom_boxplot()  +
  stat_summary(fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black"))  +
  labs(x = "Built-up density", y = "Subsidy for water bill (EUR)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ subsdens2, echo = F, message = F

ggplot(df, aes(x = urban, y = subs)) +
  stat_summary(fun = mean, geom = "col", fill = scico(1, palette = "batlow", begin = 0.2)) +
  labs(x = "Built-up density", y = "Subsidy for water bill (EUR)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

## 3.4. precarious ------

#+ avprpoor

summary(df$avrprc)
summary(df$avrprc[df$poorest == 1])
summary(df$avrprc[df$inccat == "precarious"])
summary(df$subs[df$inccat == "precarious"])

summary(df$mgnprc)
summary(df$mgnprc[df$poorest == 1])
summary(df$mgnprc[df$inccat == "precarious"])

## 3.5. changing fixed  -----

### new cvd ------

#+ fixedtab, echo = F, message = F, results = "asis"

cat("\n")
knitr::kable(fixed_tariff, digits = 4, col.names = c(" ", " ", "SWDE", "CILE", "IECBW", " ")) %>%
  add_header_above(c("Scenarios" = 1, "Fixed" = 1, "CVD" = 3, "CVA" = 1)) %>%
  kable_styling(full_width = T)
cat("\n")

### by income ------

#+ fixpcinc, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "incqnt")], vary_fixed_df[,c("id", grep("difpc_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(.~ fixed,labeller = label_both) +
  geom_hline(yintercept = 0, col = "red" , linetype = "longdash") +
  labs(x = "Income quintiles", y = "Changes in water bill (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 



#+ fixpcincinc, echo = F, message = F, fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "incqnt")], vary_fixed_df[,c("id", grep("difpcinc_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red", linetype = "longdash") +
  labs(x = "Income quintiles", y = "Ratio of changes in bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ fixTEHinc, echo = F, message = F , fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "incqnt")], vary_fixed_df[,c("id", grep("TEH_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  labs(x = "Income quintiles", y = "Ratio of water bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ fixsubsinc, echo = F, message = F , fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "incqnt")], vary_fixed_df[,c("id", grep("subs_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  labs(x = "Income quintiles", y = "Subsidy for water bill (EUR)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ fixavprinc1, echo = F, message = F , fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "incqnt")], vary_fixed_df[,c("id", grep("avrprc_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  labs(x = "Income quintiles", y = expression(Average~price~(EUR/m^3))) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ fixavprinc2, echo = F, message = F , fig.width = 10, fig.height = 5



ggplot(plotdf, aes(x = fixed, y = value, fill = incqnt))  +
  annotate(geom = "rect", xmin = 75, xmax = 125, ymin = 0, ymax = 8, fill = scico(1, palette = 'batlow', begin = 0.35), alpha = 0.5) +
         stat_summary(geom  = "col", position = "dodge", fun = mean) +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end = 0.9, direction = -1)+
  labs(x = "Fixed fee (EUR)", y =  expression(Average~price~(EUR/m^3)), fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom") 


### by urban -----------

#+ fixpcdens, echo = F, message = F , fig.width = 10, fig.height = 5



plotdf <- left_join(df[,c("id", "urban")], vary_fixed_df[,c("id", grep("difpc_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red", linetype = "longdash") +
  labs(x = "Built-up density", y = "Changes in water bill (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ fixpcincdens, echo = F, message = F, fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "urban")], vary_fixed_df[,c("id", grep("difpcinc_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red", linetype = "longdash") +
  labs(x = "Built-up density", y = "Ratio of changes in bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 



#+ fixTEHdens, echo = F, message = F , fig.width = 10, fig.height = 5



plotdf <- left_join(df[,c("id", "urban")], vary_fixed_df[,c("id", grep("TEH_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))

plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  labs(x = "Built-up density", y = "Ratio of water bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ fixsubsdens, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "urban")], vary_fixed_df[,c("id", grep("subs_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))

plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  labs(x = "Built-up density", y = "Subsidy for water bill (EUR)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ fixavprdens1, echo = F, message = F , fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "urban")], vary_fixed_df[,c("id", grep("avrprc_", colnames(vary_fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$fixed == 100,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_grid(.~ fixed, labeller = label_both) +
  labs(x = "Built-up density", y = expression(Average~price~(EUR/m^3))) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ fixavprdens2, echo = F, message = F , fig.width = 10, fig.height = 5


ggplot(plotdf, aes(x = fixed, y = value, fill = urban))  +
  annotate(geom = "rect", xmin = 75, xmax = 125, ymin = 0, ymax = 8, fill = scico(1, palette = 'batlow', begin = 0.35), alpha = 0.5) +
  stat_summary(geom  = "col", position = "dodge", fun = mean) +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end = 0.9, direction = -1)+
  labs(x = "Fixed fee (EUR)", y =  expression(Average~price~(EUR/m^3)), fill = "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom") 




### precarious --------------

#+ fixdpcpreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_fixed_df[,c("id", grep("difpc_", colnames(vary_fixed_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf, aes(x = as.factor(fixed), y = value)) + 
  geom_boxplot()+
  geom_hline(yintercept = 0, col = scico(1, palette = "batlow", begin = 0.65), linetype = "longdash", size = 0.8) +
  labs(x = "Fixed fee (EUR)", y = "Changes in water bill (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ fixtehpreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_fixed_df[,c("id", grep("TEH_", colnames(vary_fixed_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf, aes(x = as.factor(fixed), y = value)) + 
  geom_boxplot() +
  labs(x = "Fixed fee (EUR)", y = "Ratio of water bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ fixsubspreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_fixed_df[,c("id", grep("subs_", colnames(vary_fixed_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf, aes(x = as.factor(fixed), y = value)) + 
  geom_boxplot() +
  labs(x = "Fixed fee (EUR)", y = "Subsidy for water bill (EUR)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ fixavprpreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_fixed_df[,c("id", grep("avrprc_", colnames(vary_fixed_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf, aes(x = as.factor(fixed), y = value)) + 
  geom_boxplot() +
  labs(x = "Fixed fee (EUR)", y = expression(Average~price~(EUR/m^3))) + 
  theme_bw() +
  theme(legend.position = "bottom") 



## 3.6. changing rwtt  -----

### new cvd ------

#+ rwtttab, echo = F, message = F, results = "asis"

cat("\n")

knitr::kable(rwtt_tariff, digits = 4, col.names = c(" ", " ", "SWDE", "CILE", "IECBW", " ")) %>%
  add_header_above(c("Rainwater tank tax" = 1, "Averaged Fixed" = 1, "CVD" = 3, "CVA" = 1)) %>%
  kable_styling(full_width = T)
cat("\n")

### by income ------

#+ rwttpcinc, echo = F, message = F , fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "incqnt")], vary_rwtt_df[,c("id", grep("difpc_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt == 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  stat_summary(fun = median, geom = "errorbar", aes(x = incqnt, y = value, ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(x = incqnt, y = value, ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  facet_grid(.~ rwtt, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red" , linetype = "longdash") +
  labs(x = "Income quintiles", y = "Changes in water bill (%)", color = "") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttpcincinc, echo = F, message = F, fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "incqnt")], vary_rwtt_df[,c("id", grep("difpcinc_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  stat_summary(fun = median, geom = "errorbar", aes(x = incqnt, y = value, ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(x = incqnt, y = value, ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  facet_grid(.~ rwtt, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red", linetype = "longdash") +
  labs(x = "Income quintiles", y = "Ratio of changes in bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 



#+ rwttTEHinc, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "incqnt")], vary_rwtt_df[,c("id", grep("TEH_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  stat_summary(fun = median, geom = "errorbar", aes(x = incqnt, y = value, ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(x = incqnt, y = value, ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  facet_grid(.~ rwtt, labeller = label_both) +
  labs(x = "Income quintiles", y = "Ratio of water bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttsubsinc, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "incqnt")], vary_rwtt_df[,c("id", grep("subs_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(.~ rwtt, labeller = label_both) +
  labs(x = "Income quintiles", y = "Subsidy for water bill (EUR)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttavprinc1, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "incqnt")], vary_rwtt_df[,c("id", grep("avrprc_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(.~ rwtt, labeller = label_both) +
  labs(x = "Income quintiles", y = expression(Average~price~(EUR/m^3))) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttavprinc2, echo = F, message = F , fig.width = 10, fig.height = 5


ggplot(plotdf, aes(x = rwtt, y = value, fill = incqnt))  +
  annotate(geom = "rect", xmin = -25, xmax = 25, ymin = 0, ymax = 8, fill = scico(1, palette = 'batlow', begin = 0.35), alpha = 0.5) +
  stat_summary(geom  = "col", position = "dodge", fun = mean) +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end = 0.9, direction = -1) +
  labs(x = "Fixed fee (EUR)", y =  expression(Average~price~(EUR/m^3)), fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom") 



### by urban -----------

#+ rwttpcdens, echo = F, message = F , fig.width = 10, fig.height = 5


plotdf <- left_join(df[,c("id", "urban")], vary_rwtt_df[,c("id", grep("difpc_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value))+
  stat_summary(fun = median, geom = "errorbar", aes(x = urban, y = value, ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(x = urban, y = value, ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  facet_grid(.~ rwtt, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red", linetype = "longdash") +
  labs(x = "Built-up density", y = "Changes in water bill (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttpcincdens, echo = F, message = F, fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "urban")], vary_rwtt_df[,c("id", grep("difpcinc_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value))+
  stat_summary(fun = median, geom = "errorbar", aes(x = urban, y = value, ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(x = urban, y = value, ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  facet_grid(.~ rwtt, labeller = label_both)  +
  geom_hline(yintercept = 0, col = "red", linetype = "longdash") +
  labs(x = "Built-up density", y = "Ratio of changes in bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 



#+ rwttTEHdens1, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "urban")], vary_rwtt_df[,c("id", grep("TEH_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  stat_summary(fun = median, geom = "errorbar", aes(x = urban, y = value, ymax = ..y.., ymin = ..y.., col = "median"), width = 0.75, size = 1, linetype = "solid") +
  stat_summary(fun = mean, geom = "errorbar", aes(x = urban, y = value, ymax = ..y.., ymin = ..y.., col = "mean"), width = 0.75, size = 1, linetype = "solid") +
  scale_color_manual(values = c(scico(1, palette = 'batlow', begin = 0.65), "black")) +
  facet_grid(.~ rwtt, labeller = label_both)  +
  labs(x = "Built-up density", y = "Ratio of water bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttTEHdens2, echo = F, message = F , fig.width = 10, fig.height = 5

ggplot(plotdf, aes(x = rwtt, y = value)) +
  stat_summary(fun = mean, geom = "col", aes(fill = urban), position = "dodge")  +
  scale_fill_scico_d(palette = 'batlow', begin = 0.1, end = 0.9, direction = -1)  +
  labs(x = "Rainwater tank tax (EUR)", y = "Ratio of water bill to income (%)", "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom")



#+ rwttsubsdens, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "urban")], vary_rwtt_df[,c("id", grep("subs_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_grid(.~ rwtt, labeller = label_both) +
  labs(x = "Built-up density", y = "Subsidy for water bill (EUR)") + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttavprdens1, echo = F, message = F , fig.width = 10, fig.height = 5

plotdf <- left_join(df[,c("id", "urban")], vary_rwtt_df[,c("id", grep("avrprc_", colnames(vary_rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf) +
  geom_rect(data = plotdf[plotdf$rwtt %in% 0,][1,], fill = scico(1, palette = 'batlow', begin = 0.35), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_grid(.~ rwtt, labeller = label_both) +
  labs(x = "Built-up density", y = expression(Average~price~(EUR/m^3))) + 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttavprdens2, echo = F, message = F , fig.width = 10, fig.height = 5


ggplot(plotdf, aes(x = rwtt, y = value, fill = urban))  +
  annotate(geom = "rect", xmin = -25, xmax = 25, ymin = 0, ymax = 8, fill = scico(1, palette = 'batlow', begin = 0.35), alpha = 0.5) +
  stat_summary(geom  = "col", position = "dodge", fun = mean) +
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end = 0.9, direction = -1)+
  labs(x = "Rainwater tank tax (EUR)", y =  expression(Average~price~(EUR/m^3)), fill = "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom") 


### precarious --------------

#+ rwttdpcpreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_rwtt_df[,c("id", grep("difpc_", colnames(vary_rwtt_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) + 
  geom_boxplot()+
  geom_hline(yintercept = 0, col = "red", linetype = "longdash") +
  labs(x = "Rainwater tank tax (EUR)", y = "Changes in water bill (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ rwtttehpreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_rwtt_df[,c("id", grep("TEH_", colnames(vary_rwtt_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) + 
  geom_boxplot() +
  labs(x = "Rainwater tank tax (EUR)", y = "Ratio of water bill to income (%)") + 
  theme_bw() +
  theme(legend.position = "bottom") 


#+ rwttsubspreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_rwtt_df[,c("id", grep("subs_", colnames(vary_rwtt_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) + 
  geom_boxplot() +
  labs(x = "Rainwater tank tax (EUR)", y = "Subsidy for water bill (EUR)") +
  scale_x_discrete(labels = rwtts)+ 
  theme_bw() +
  theme(legend.position = "bottom") 

#+ rwttavprpreca, echo = F, message = F

plotdf <- left_join(df[,c("id", "inccat")], vary_rwtt_df[,c("id", grep("avrprc_", colnames(vary_rwtt_df), value = T))])

plotdf <- plotdf[plotdf$inccat %in% "precarious",]

plotdf <- melt(plotdf, id.vars = c("id", "inccat"))

plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) + 
  geom_boxplot() +
  labs(x = "Rainwater tank tax (EUR)", y = expression(Average~price~(EUR/m^3))) +
  scale_x_discrete(labels = rwtts)+ 
  theme_bw() +
  theme(legend.position = "bottom") 



## 3.7. UP vs IBTcap vs IBT con ----------


### new price ---------
#+ upcctariff, echo = F, message = F, results = 'asis'
cat("\n")
kable(up_tariff)
cat("\n")

#+ ibtcontariff, echo = F, message = F, results = 'asis'
cat("\n")
kable(ibtcon_tariff)
cat("\n")

#+ ibtcaptariff, echo = F, message = F, results = 'asis'
cat("\n")
kable(ibtcap_tariff)
cat("\n")

### by income ------------

#+ upccpcinc, echo = F, message = F , fig.width = 10, fig.height = 10

plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("difpc_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = incqnt)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red" , linetype = "longdash") +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")  +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP"))+
  labs(x = "Tariff types", y = "Changes in water bill (%)", fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom")

#+ upcctehinc1, echo = F, message = F , fig.width = 10, fig.height = 10


plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("TEH_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = incqnt)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom")

#+ upcctehinc2, echo = F, message = F , fig.width = 10, fig.height = 10


plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("TEH_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf, aes(x = tariff, y = value, fill = incqnt))  +
  stat_summary(fun = mean, geom = "col", position = "dodge") + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom")


#+ upccsubsinc, echo = F, message = F , fig.width = 10, fig.height = 10
plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("subs_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = incqnt)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types", y = "Subsidy for water bill (EUR)", fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom")

#+ upccavprinc, echo = F, message = F , fig.width = 10, fig.height = 10

plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("avrprc_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = incqnt)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types", y =  expression(Average~price~(EUR/m^3)), fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom")

### by urban ------------

#+ upccpcdens, echo = F, message = F , fig.width = 10, fig.height = 10

plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("difpc_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "urban")])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = urban)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  geom_hline(yintercept = 0, col = "red" , linetype = "longdash") +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")  +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP"))+
  labs(x = "Tariff types", y = "Changes in water bill (%)", fill = "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom")

#+ upcctehdens1, echo = F, message = F , fig.width = 10, fig.height = 10


plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("TEH_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "urban")])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = urban)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom")

#+ upcctehdens2, echo = F, message = F , fig.width = 10, fig.height = 10


plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("TEH_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "urban")])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf, aes(x = tariff, y = value, fill = urban))  +
  stat_summary(fun = mean, geom = "col", position = "dodge") + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom")


#+ upccsubsdens, echo = F, message = F , fig.width = 10, fig.height = 10
plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("subs_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "urban")])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = urban)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types", y = "Subsidy for water bill (EUR)", fill = "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom")

#+ upccavprdens, echo = F, message = F , fig.width = 10, fig.height = 10

plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("avrprc_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "urban")])

plotdf <- melt(plotdf, id.vars = c("id", "urban"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = urban)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types", y =  expression(Average~price~(EUR/m^3)), fill = "Built-up density") + 
  theme_bw() +
  theme(legend.position = "bottom")

knitr::knit_exit()

#+ notcorrectedyet

### testing, will you print everything? ------------
#+ test, results = "asis"
cor(df$income, df$inceqa, method = "spearman")

df$hhsadj <- 1 + (df$hhs_20_95 - 1)*0.5 + (df$hhs_0_19)*0.3

df$cspea <- df$csmptv/df$hhsadj

inceqa <- df$income*12/df$hhsadj
plot(df$cspea, inceqa)
plot(df$csmptv, df$income)
cor(df$csmptv, df$income)
summary(lm(csmptv ~ hhs + income, df))
summary(lm(cspea ~ inceqa, df))

ggplot(df, aes(x = ieaqnt, y =  billpc)) +
  geom_boxplot()

plotdf_ls <- lapply(list(up_df, ibtcon_df, ibtcap_df), function(x) x[, c("id", grep("subs_", colnames(x), value = T))])

ggplot(df, aes(x = ipcqnt, fill = hhscat)) +
  geom_bar(position = position_fill(reverse = F))+
  scale_fill_scico_d(palette = "batlow", begin = 0.1, end =0.9, direction = -1) +
  labs(x = "Income per equivalent adult quintiles", y = "Proportion of households" , fill = "Household size") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "ieaqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "ieaqnt"))

plotdf$tariff <- gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <- as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf)  +
  geom_boxplot(aes(x = tariff, y = value, fill = ieaqnt)) + 
  facet_grid(fixed ~ revincr, labeller = label_both) +
  scale_fill_scico_d(begin = 0.3, end = 0.7, palette = "batlow")+
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types", y = "Subsidy for water bill (EUR)", fill = "Income quintiles") + 
  theme_bw() +
  theme(legend.position = "bottom")



