
# notes from last run----------------------


# 1. set up -------------------

## 1.1. load functions -------------

### new functions ------------


loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}




sim.f <- function(a = "25005", data = df, hhsNA = F, rwt_snros = 1) {
  hhsdf <- data[data$municd %in% a,]

  ### hhsize ---------------
  
  hhs_sim <- hhsdf[, c("municd", "ststcd", "year", "bltupss", "hhs_1", "hhs_2", "hhs_3", "hhs_4")]
  
  hhs_sim[is.na(hhs_sim)] <- 0
  
  bighh <- lapply(hhsdf$`hhs_5+`, function(x) {
    if (!is.na(x) & x > 0) {
      bhh_v <- data.frame(rmultinom(x, 1, prob = bighh_prob))
      res <- apply(bhh_v, 1, sum)
    } else {
      res <- rep(0, 5)
    }
  })
  
  bighh <- Reduce(rbind.data.frame, bighh)
  colnames(bighh) <- paste0("hhs_", 5:9)
  
  hhs_sim <- cbind.data.frame(hhs_sim, bighh)
  
  hhs_sim$nhh_sim <- apply(hhs_sim[,5:13], 1, sum)
  
  if (hhsNA) {
    # x <- unlist(cbind.data.frame(nbhh_prv = hhsdf$nbhh_prv, hhs_sim[,4:14])[1,])
    hhs_NA <- apply(cbind.data.frame(nbhh_prv = hhsdf$nbhh_prv, hhs_sim[,4:14]), 1, function(x) {
      n_NA <- x[1] - x[12]
      if (!is.na(n_NA) & n_NA > 0) {
        hhs_prob <- x[3:11]/sum(x[3:11])
        
        if (sum(hhs_prob, na.rm = T) == 1) {
          
          hhs_v <- data.frame(rmultinom(n_NA, 1, prob = hhs_prob))
        } else {
          hhs_prob <- hhs_prob_ss[hhs_prob_ss$bltupss == x[2], -10]
          hhs_v <- data.frame(rmultinom(n_NA, 1, prob = hhs_prob))
          
        }
        res <- apply(hhs_v, 1, sum)
      } else {
        res <- rep(0, 9)
      }
    })
    
    
    
    hhs_sim[, 5:13] <- hhs_sim[, 5:13] + t(hhs_NA)
    hhs_sim$nhh_sim <- apply(hhs_sim[, 5:13], 1, sum)    
  }

  

  
  ### create dataframe with each row is a family
  
  hhs_sim_lg <- melt(hhs_sim[, 1:13], id.vars = c("municd", "ststcd", "year", "bltupss"))
  
  fam_sim <- hhs_sim_lg[rep(1:nrow(hhs_sim_lg), hhs_sim_lg$value), -6]
  rownames(fam_sim) <- 1:nrow(fam_sim)
  
  fam_sim$hhs_tot <- as.integer(gsub("hhs_", "", fam_sim$variable))
  
  fam_sim <- fam_sim[,-5]
  
  ### ref person age ------------------------------
  
  
  fam_sim$rpage_cat <- sapply(fam_sim$hhs_tot, function(x) {
    pr <- p_rpa_hhs[, dimnames(p_rpa_hhs)[[2]] ==x ]
    rpa_v <- rmultinom(1, 1, prob = pr)
    res <- dimnames(p_rpa_hhs)[[1]][rpa_v == 1]
  })
  
  
  
  ### household composition -------------------------------
  
  
  fam_sim$nch <- 0L
  
  fam_sim$nch[fam_sim$hhs_tot > 1] <- apply(fam_sim[fam_sim$hhs_tot > 1, c("rpage_cat", "bltupss", "hhs_tot")], 1, function(x) {
    
    rpa_tmp <- x[1]
    blt_tmp <- x[2]
    hhs_tmp <- x[3]
    
    pr1 <-
      p_chd_rpa_blt_hhs[, dimnames(p_chd_rpa_blt_hhs)[[2]] %in% rpa_tmp, dimnames(p_chd_rpa_blt_hhs)[[3]] %in% blt_tmp, dimnames(p_chd_rpa_blt_hhs)[[4]] %in% hhs_tmp]
    pr2 <-
      p_chd_blt_hhs[, dimnames(p_chd_blt_hhs)[[2]] %in% blt_tmp, dimnames(p_chd_blt_hhs)[[3]] %in% hhs_tmp]
    pr3 <- p_chd_hhs[, dimnames(p_chd_hhs)[[2]] %in% hhs_tmp]
    
    if (sum(pr1, na.rm = T) > 0.999 ){
      nch_v <- rmultinom(1, 1, prob = pr1)
    } else {
      if (sum(pr2, na.rm = T) > 0.999) {
        nch_v <- rmultinom(1, 1, prob = pr2)
      } else {
        nch_v <- rmultinom(1, 1, prob = pr3)
      }
    }
    
    res <- as.integer(dimnames(p_chd_hhs)[[1]][nch_v == 1])
  })
  
  
  
  fam_sim$nad <- fam_sim$hhs_tot - fam_sim$nch
  
  ### rainwater tank -----------
  
  
  fam_sim$rwt_prop <- rwt_mun$rwt_prop[rwt_mun$municd == a]*rwt_snros/100
  
  fam_sim$rwtank <- sapply(fam_sim$rwt_prop, function(x) {
    res <- rbinom(1, 1, prob = x)
  })
  
  fam_sim$rwtank <- ifelse(fam_sim$rwtank ==1, "yes", "no")
  ### water consumption --------------
  
  fam_sim$bltupss <- as.factor(fam_sim$bltupss)
  
  pred.mean <- predict(wcons_fit, newdata = fam_sim, allow.new.levels = T)
  
  cspd <- sapply(pred.mean, function(x) {
    res <- rnorm(n = 1, mean = x, sd = res_sd)
  })
  
  ndays <- yday(as.Date(paste0(fam_sim$year, "-12-31")))
  
  
  fam_sim$cons <- cspd*ndays/1000
  
  ### summarise -------------
  
  res <- fam_sim %>%
    group_by(municd, year) %>%
    summarise(nad = sum(nad, na.rm = T),
              nch = sum(nch, na.rm = T),
              n = n(),
              rwt = sum(rwtank == "yes", na.rm = T)/n(),
              cons = sum(cons, na.rm = T))
}




### packages ----------


loadpackage("here")
loadpackage("reshape2")
loadpackage("ggplot2")
loadpackage("dplyr")
loadpackage("MASS")
loadpackage("scico")
loadpackage("tidyr")
loadpackage("sf")
loadpackage("xlsx")
loadpackage("ggpmisc")
loadpackage("nnet")
loadpackage("lme4")
loadpackage("lubridate")
loadpackage('doParallel')



source(here("scripts", "general_functions.R"))



## 1.2. load data ------------------------

### data folder --------

rdir <- "data/raw"
pdir <- "data/processed"

### historical consumption --------


load(file = here(pdir, "water_histcons_Wal", "water_histcons.Rdata"))
### utility survey ----------

us <-
  read.csv(file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_AquaWal_prd.csv"
  ))




### ggs --------------

load(file = here(pdir, "sode_GGS_GGP_Be/sode_GGS_Be.Rdata"))

### rainwater tank ------------

rwt_mun <- read.csv(here(pdir, "raintank_muni_Be", "raintank_muni_Be.csv"))

### statistical sectors ------------

load(file = here(pdir, "sode_ss_Be", "sode_ss_Be.Rdata"))


hhs_ss_wal <- hhs_ss[hhs_ss$regicd %in% 3000,]

### administrative borders -----------

load(file = here(
  pdir,
  "admin_border_Be/admin_Be.Rdata"
))

stst_wal <- stst[stst$regicd %in% 3000, ]
stst_wal$municd <- as.character(stst_wal$municd)


### builtup density at ss -----------

load(file = here(
  pdir,
  "urban_5cat_Ahmed_Wal/urban_5cat_ss_Wal.Rdata"
))
head(bltupss)

### consumption at mun -----------

load(file = here(pdir, "water_histcons_Wal", "water_consum_mun.Rdata"))

### projected IWEPS ----------

#### nhh
prjnhh <- read.xlsx(here(rdir, "sode_proj_IWEPS_Wal", "244601.xlsx"), sheetIndex = 1, startRow = 11)[,c(1:2, 6:8, 5)]
colnames(prjnhh) <- c("municd", "type", "nhh_2020", "nhh_2025", "nhh_2030", "nhh_2035")


prjnhh <- prjnhh[prjnhh$type %in% "Commune", c(1,3:6)]

#### pop


prjpop <- read.xlsx(here(rdir, "sode_proj_IWEPS_Wal", "244600.xlsx"), sheetIndex = 1, startRow = 11)[,c(1:2, 6:8, 5)]
colnames(prjpop) <- c("municd", "type", "pop_2020", "pop_2025", "pop_2030", "pop_2035")


prjpop <- prjpop[prjpop$type %in% "Commune", c(1,3:6)]



knitr::knit_exit()

#+ notcorrectedyet

# 3. process data------------------

## 3.1. utility survey -----------

us$municd <- as.character(us$municd)
us <- us[!is.na(us$hhs_18_95) & us$hhs_18_95 > 0 & !is.na(us$rwtank),]

us_age <- 2014 - us[, grep("by_", colnames(us))]

us_age[us_age < 0] <- NA

us$hhs_20_95 <-
  apply(us_age, 1, function(x)
    sum(x >= 20, na.rm = T))

us$hhs_0_19 <-  apply(us_age, 1, function(x)
  sum(x < 20, na.rm = T))

# table(us$hhs_tot)
#
# prop.table(table(us$hhs_20_95))
#
# table(us$hhs_0_19)

us$hhs_cat <- paste0("hhs_", ifelse(us$hhs_tot < 5, as.character(us$hhs_tot), "5+"))

us$rpage_cat <- cut(us$rpage, breaks = c(seq(15, 80, 5), 150), include.lowest = T, right = F)


us <- left_join(us, bltupss[,c("ststcd", "LU2010_5cls_x25")])
colnames(us)[ncol(us)] <- "bltupss"

### houshold size proportion ---------



hhs_prob_us  <- prop.table(table(us$hhs_tot))


bighh_us <- us[us$hhs_tot > 4,] %>%
  group_by(hhs_tot) %>%
  summarise(count_us = n())

bighh_us$prop_us <- bighh_us$count_us/sum(bighh_us$count_us)


## 3.2. ggs ------------

ggs <- ggs_wal[, c("arid", "ahhsize", grep("ahg", colnames(ggs_wal), value = T))]

colnames(ggs)[1:2] <- c("id", "hhs_tot")



ggs_age <- ggs[, grep("ahg5_", colnames(ggs))]

ggs$hhs_20_95 <-
  apply(ggs_age, 1, function(x)
    sum(x >= 20, na.rm = T))

ggs$hhs_0_19 <-  apply(ggs_age, 1, function(x)
  sum(x <20, na.rm = T))

ggs$hhs_cat <- paste0("hhs_", ifelse(ggs$hhs_tot < 5, as.character(ggs$hhs_tot), "5+"))

ggs$rpage_cat <- cut(ggs$ahg5_1, breaks = c(seq(15, 80, 5), 150), include.lowest = T, right = F)

### houshold size proportion ---------

hhs_prob_ggs <- prop.table(table(ggs$hhs_tot))



bighh_ggs <- ggs[ggs$hhs_tot > 4,] %>%
  group_by(hhs_tot) %>%
  summarise(count_ggs = n())

bighh_ggs$prop_ggs <- bighh_ggs$count_ggs/sum(bighh_ggs$count_ggs)


## 3.3.  statistical sectors -----------------

### hhs ----------
hhs_ss_wal$municd <- as.character(hhs_ss_wal$municd)
hhs_ss_wal <- left_join(hhs_ss_wal, bltupss[, c("ststcd", "LU2010_5cls_x25")])
colnames(hhs_ss_wal)[ncol(hhs_ss_wal)] <- "bltupss"
hhs_ss_wal$bltupss <- as.integer(hhs_ss_wal$bltupss)

hhs_ss_wal$year <- as.integer(hhs_ss_wal$year)

hhs_ss_lg <- melt(hhs_ss_wal[,c("municd", "ststcd", "year", "hhs_1", "hhs_2", "hhs_3", "hhs_4", "hhs_5+")], id.vars = c("municd", "ststcd", "year"))


colnames(hhs_ss_lg)[4:5] <- c("hhsize", "obs")






### houshold size proportion ---------


hhs_n_ss <- hhs_ss_wal[, c("bltupss", grep("hhs_\\d", colnames(hhs_ss_wal), value =T))] %>%
  group_by(bltupss) %>%
  summarise_all(sum, na.rm = T)

hhs_prob_ss <- data.frame(t(apply(hhs_n_ss[,2:6], 1, function(x) {
  res <- x/sum(x)
  res <- c(res[1:4], res[5]*bighh_us$prop_us)})))
 
hhs_prob_ss$bltupss <- hhs_n_ss$bltupss
hhs_prob_ss

### hhs by mun ----------


hhs_mun_wal <- hhs_ss_wal[, c("municd", "year", "nbhh_prv", grep("hhs_\\d", colnames(hhs_ss_wal), value = T))] %>%
  group_by(municd, year) %>%
  summarise_all(sum, na.rm = T)

### age -------------

age_ss_wal <- age_ss[age_ss$regicd %in% 3000,]
age_ss_wal$municd <- as.character(age_ss_wal$municd)
age_ss_wal <- 
  age_ss_wal[, c(grep("age_", colnames(age_ss_wal), value = T), "municd", "ststcd", "year", "population")]

age_ss_wal$hhs_0_19 <- apply(age_ss_wal[,1:4], 1, sum, na.rm = T)
age_ss_wal$hhs_20_95 <- apply(age_ss_wal[,5:17], 1, sum, na.rm = T)

### population density -------------


stst_wal$area <- st_area(stst_wal)/1e6

age_ss_wal <- left_join(age_ss_wal, st_drop_geometry(stst_wal[, c("ststcd", "area")]))

age_ss_wal$dens <- as.numeric(age_ss_wal$population/age_ss_wal$area)

age_ss_wal$dens_cat <-cut(age_ss_wal$dens, breaks = c(0, 300, 1500, max(age_ss_wal$dens, na.rm = T)), include.lowest = T, right = F)

hist(age_ss_wal$dens, breaks = 300)
abline(v = c(300, 1500), col = "red")
table(age_ss_wal$dens_cat)



### nad/nch by mun ----------


age_mun_wal <- age_ss_wal[, c("municd", "year", "population", "hhs_0_19",  "hhs_20_95")] %>%
  group_by(municd, year) %>%
  summarise_all(sum, na.rm = T)



## 3.4. rainwater tank ----------

rwt_mun$municd <- as.character(rwt_mun$municd)

## 3.5. projected iweps ---------
### nhh
prjnhh_lg <- melt(prjnhh, id.vars = "municd")
colnames(prjnhh_lg)[3] <- "prj_nhh"
prjnhh_lg$year <- as.numeric(gsub("nhh_", "", prjnhh_lg$variable))

prjnhh_lg <- prjnhh_lg[order(prjnhh_lg$municd, prjnhh_lg$year), c("municd", "year", "prj_nhh")]

### pop 

prjpop_lg <- melt(prjpop, id.vars = "municd")
colnames(prjpop_lg)[3] <- "prj_pop"
prjpop_lg$year <- as.numeric(gsub("pop_", "", prjpop_lg$variable))

prjpop_lg <- prjpop_lg[order(prjpop_lg$municd, prjpop_lg$year), c("municd", "year", "prj_pop")]

### merge 

prj_iweps <- inner_join(prjnhh_lg, prjpop_lg)

chg_ins <- data.frame(new_ins = c("58001", "58002", "58003", "58004", "55085", "55086", "51067", "51068", "51069", "57096", "57097"), old_ins = c("55022", "56011", "56085", "56087", "52063", "52043", "55010", "55039", "55023", "54007", "54010"))

# prj_iweps[prj_iweps$municd %in% 58001,]

prj_iweps$municd <- sapply(prj_iweps$municd, function(x) ifelse(x %in% chg_ins$new_ins, chg_ins$old_ins[chg_ins$new_ins == x], x))

prj_iweps[prj_iweps$municd %in% 55022,]

prj_iweps$av_hhs <- prj_iweps$prj_pop/prj_iweps$prj_nhh

prj_iweps$municd <- as.character(prj_iweps$municd)
## 3.6. consumption at mun -----------

cslt250_lg <- melt(csmun[, c("municd", grep("cons_lt250_", colnames(csmun), value =T))], id.vars = "municd")
colnames(cslt250_lg)[ncol(cslt250_lg)] <- "cslt250"
cslt250_lg$year <- as.integer(gsub(".*_", "", cslt250_lg$variable))


cstot_lg <- melt(csmun[, c("municd", grep("cons_tot_", colnames(csmun), value =T))], id.vars = "municd")
colnames(cstot_lg)[ncol(cstot_lg)] <- "cstot"
cstot_lg$year <- as.integer(gsub(".*_", "", cstot_lg$variable))
## some often use municipalities ----------

mncdls <- lvls(hhs_ss_wal$municd)
majormun <- c("62063", "52011", "92094", "53053", "55022", "57081")

# 4. exploring ------

## 4.1. rainwater tank -----------

# rwt_us <- us %>%
#   group_by(municd) %>%
#   summarise(rwt_prop_us = sum(rwtank == "yes")*100/n(),
#             count_us = n())
# 
# plotdf <- inner_join(rwt_us, rwt_mun)
# 
# cor.test(plotdf$rwt_prop_us[plotdf$count_us > 20], plotdf$rwt_prop[plotdf$count_us > 20])
# 
# plotdf <- plotdf[plotdf$count_us > 20,]
# 
# ggplot(plotdf, aes(x = rwt_prop_us, y = rwt_prop)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
#   geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
#   stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) 
# 
# 
# summary(rwt_mun$rwt_propna)
# 
# cor.test(as.numeric(as.factor(us$rwtank)), as.numeric(us$bltupss))

## 4.2. age --------

# us_age <- 2014 - us[grep("by", colnames(us))]
# 
# us_age[us_age < 0] <- NA
# 
# us_age_v <- unlist(us_age)
# us_age_cat <- cut(us_age_v, breaks = c(seq(0, 80, 5), 150), include.lowest = T, right = F)
# 
# 
# plotdf <- as.data.frame(prop.table(table(unlist(us_age_cat))))
# 
# colnames(plotdf) <- c("age_cat", "freq_us")
# 
# ggs_age_cat <- cut(unlist(ggs_age), breaks = c(seq(0, 80, 5), 150), include.lowest = T, right = F)
# 
# plotdf$freq_ggs <- prop.table(table(ggs_age_cat))
# 
# 
# 
# age_ss_2014 <- apply(age_ss_wal[age_ss_wal$year %in% 2014, grep("age_", colnames(age_ss_wal))],  2, sum, na.rm = T)
# 
# plotdf$freq_ss <- age_ss_2014/sum(age_ss_2014)
# 
# 
# ggplot(plotdf, aes(x = age_cat)) +
#   geom_line(aes(y = freq_us, col = "us", group = 1), size = 1)  +
#   geom_line(aes(y = freq_ggs, col = "ggs", group = 1), size = 1) +
#   geom_line(aes(y = freq_ss, col = "ss", group = 1), size = 1) +
#   theme_kat()  +
#   scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)
# 
# cor.test(plotdf$freq_ggs, plotdf$freq_ss)
# cor.test(plotdf$freq_us, plotdf$freq_ss)


## 4.3. hhsize -------------


# plotdf <- cbind.data.frame(prob_us = as.numeric(hhs_prob_us), prob_ggs = as.numeric(hhs_prob_ggs), t(hhs_prob_ss[,1:9]))
# 
# colnames(plotdf)[3:8] <- paste("ss_blt_", 0:5)
# 
# plotdf$hhs <- paste0("hhs_", 1:9)
# 
# plotdf <- melt(plotdf, id.vars = "hhs")
# 
# 
# ggplot(plotdf, aes(x = hhs, y = value, color = variable, group = variable)) +
#   geom_line(size = 0.8) +
#   theme_kat()  +
#   scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)
# 
# 
# test <- us %>%
#   group_by(dstrcd) %>%
#   summarise(n = n(),
#             n5 = sum(hhs_tot %in% 5),
#             n6 = sum(hhs_tot %in% 6),
#             n7 = sum(hhs_tot %in% 7),
#             n8 = sum(hhs_tot %in% 8),
#             n9 = sum(hhs_tot %in% 9))
# 
# test1 <- t(apply(test[,3:7], 1, function(x) x*100/sum(x)))
# 
# test2 <- reshape2::melt(test1[test$n > 120,], id.vars = NULL)
# 
# ggplot(test2, aes(x = Var2, y = value, group = Var1)) + geom_line()
# 
# 
# ## CONCLUSION: if the sample size is large enough the distribution of hhsize from 5 to 9 are quite close to each other => sampling sample size from the group of 5+ using the distribution of whole us data (which almost identical with ggs data)
# 
# 
# plotdf <- hhs_ss_wal[, c("ststcd", "year", "bltupss", grep("hhs_", colnames(hhs_ss_wal), value = T))]
# 
# propdf <- data.frame(t(apply(plotdf[, -c(1:3)], 1, function(x) x*100/sum(x))))
# 
# plotdf <- cbind.data.frame(plotdf[, 1:3], propdf)
# 
# plotdf <- melt(plotdf, id.vars = c("ststcd", "year", "bltupss"))
# 
# plotdf <- inner_join(plotdf, age_ss_wal[, c("ststcd", "year", "dens_cat", "dens")]) 
# 
# cor.test(plotdf$bltupss, plotdf$dens)
# 
# sample <- sample(unique(plotdf$ststcd), 6)
# 
# ggplot(plotdf[plotdf$ststcd %in% sample,], aes(x = variable, y = value, color = as.factor(year))) + geom_line(aes(group = as.factor(year))) + facet_wrap(. ~ ststcd)
# 
# ### CONCLUSION the changes in relative proportion of households size in each stst through time is marginal
# 
# sample <- sample(unique(plotdf$ststcd), 100)
# 
# ggplot(plotdf[plotdf$ststcd %in% sample,], aes(x = variable, y = value, color = as.factor(bltupss), group = ststcd)) + geom_line() + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)
# 
# ggplot(plotdf[plotdf$ststcd %in% sample,], aes(x = variable, y = value, color = dens_cat, group = ststcd)) + geom_line() + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)
# 
# 
# ggplot(plotdf, aes(x = variable, y = value, color = as.factor(bltupss), group = as.factor(bltupss))) + stat_summary(geom = "line", fun = "mean") + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)
# 
# ggplot(plotdf, aes(x = variable, y = value, color = dens_cat, group = dens_cat)) + stat_summary(geom = "line", fun = "mean") + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)
# 
# summary(lm(value ~ as.factor(bltupss), data = plotdf[plotdf$variable %in% "hhs_1",]))
# 
# summary(lm(value ~ dens_cat, data = plotdf[plotdf$variable %in% "hhs_1",]))
# 
# summary(lm(value ~ as.factor(bltupss), data = plotdf[plotdf$variable %in% "hhs_4",]))
# 
# summary(lm(value ~ dens_cat, data = plotdf[plotdf$variable %in% "hhs_4",]))
# 
# 
# hhs_prob_dens <- plotdf %>%
#   group_by(dens_cat, variable) %>%
#   summarise(mean = mean(value, na.rm = T),
#             sd = sd(value, na.rm = T))
# 
# hhs_prob_dens <- hhs_prob_dens[complete.cases(hhs_prob_dens),]
# 
# # average hhsize through time 
# 
# plotdf <- inner_join(hhs_ss_wal, age_ss_wal)
# 
# plotdf <- plotdf %>%
#   group_by(year) %>%
#   summarise(avr_hhs = sum(population, na.rm = T)/sum(nbhh_prv, na.rm = T))
# 
# 
# ggplot(plotdf, aes(x = year, y = avr_hhs)) +
#   geom_line()

### hhsize in IWEPS projection ---------------



plotdf <- prj_iweps %>%
  group_by(year) %>%
  summarise(av_hhs = sum(prj_pop, na.rm = T)/sum(prj_nhh, na.rm = T))


ggplot(plotdf, aes(x = year, y = av_hhs)) +
  geom_point() +
  theme_kat()

ggplot(prj_iweps[prj_iweps$municd %in% majormun,], aes(x = year, y = av_hhs)) +
  geom_point() +
  facet_wrap(.~municd) +
  theme_kat()



# 5. Simulation for the past-------------



## 5.1. fitting ------------

### hhsize ------------



bighh_prob <- bighh_us$prop_us


### rpage ------------
p_rpa_hhs <- prop.table(table(us$rpage_cat, us$hhs_tot), margin = 2)

p_rpa_blt_hhs <- prop.table(table(us$rpage_cat, us$bltupss, us$hhs_tot), margin = c(2:3))



### hh composition ---------

p_chd_rpa_blt_hhs <- prop.table(table(us$hhs_0_19, us$rpage_cat, us$bltupss, us$hhs_tot), margin = c(2:4))

p_chd_blt_hhs <- prop.table(table(us$hhs_0_19, us$bltupss, us$hhs_tot), margin = c(2:3))

p_chd_hhs <- prop.table(table(us$hhs_0_19, us$hhs_tot), margin = 2)

p_chd_rpa_hhs <- prop.table(table(us$hhs_0_19, us$rpage_cat, us$hhs_tot), margin = c(2:3))


### water consumption ------------

fitdf <- fitdf[!is.na(fitdf$cspd) & fitdf$cspd > 5,]
fitdf$nad <- fitdf$hhs_20_100
fitdf$nch <- fitdf$hhs_0_19

wcons_fit <- lmer(cspd ~  nch + nad + rwtank  + bltupss + (1+ nad + bltupss|municd), data = fitdf, na.action = na.exclude)

# varcor <- summary(wcons_fit)$varcor
# 
# qnb_sd <- sqrt(as.numeric(varcor[[1]]))
res_sd <- summary(wcons_fit)$sigma

# sd <- qnb_sd + res_sd

# save(bighh_prob, hhs_prob_ss, p_rpa_hhs, p_chd_rpa_blt_hhs, p_chd_blt_hhs, p_chd_hhs, rwt_mun, wcons_fit, res_sd, file = here(pdir, "cons_proj_Wal", "fitted_probs.Rdata"))

## 5.2. generating ---------


system.time(testres <- lapply(mncdls[1:2], sim.f,data = hhs_ss_wal, hhsNA = T, rwt_snros = 1))

system.time({
  ncores <- detectCores(logical = T)
  cl <- makeCluster(ncores - 1)
  clusterEvalQ(cl, c(library(lme4), library(dplyr), library(lubridate), library(reshape2)))
  clusterExport(cl,list("hhs_ss_wal", "bighh_prob", "hhs_prob_ss", "p_rpa_hhs", "p_chd_rpa_blt_hhs", "p_chd_blt_hhs", "p_chd_hhs", "rwt_mun", "wcons_fit", "res_sd", "mncdls", "sim.f"))
  res <- parLapply(cl, 1:100, function(i) {
    res <- lapply(mncdls, function(x) sim.f(a = x, data = hhs_ss_wal, hhsNA = T, rwt_snros = 1))
    res <- bind_rows(res)
    })

  stopCluster(cl)
})




sim_agg <- bind_rows(res, .id = "rep")
sim_agg$pop <- sim_agg$nad + sim_agg$nch


save(sim_agg, file = here(pdir, "cons_proj_Wal", "sim_agg_1119.Rdata"))

load(file = here(pdir, "cons_proj_Wal", "sim_agg_1119.Rdata"))

## 5.3. validation ----------------------

### household composition ------------------

plotdf <- sim_agg[,-1] %>%
  group_by(municd, year) %>%
  summarise(mean_nad = mean(nad),
            q1_nad = quantile(nad, props = 0.25),
            q3_nad = quantile(nad, props = 0.75),
            mean_nch = mean(nch),
            q1_nch = quantile(nch, props = 0.25),
            q3_nch = quantile(nch, props = 0.75),
            mean_pop = mean(pop),
            q1_pop = quantile(pop, props = 0.25),
            q3_pop = quantile(pop, props = 0.75))

plotdf <- inner_join(plotdf, age_mun_wal)

 
ggplot(plotdf, aes(x = hhs_0_19, y = mean_nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
 
ggplot(plotdf, aes(x = hhs_20_95, y = mean_nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

ggplot(plotdf, aes(x = population, y = mean_pop)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)
 


### rainwater tank -----------------

plotdf <- sim_agg[,-1] %>%
  group_by(municd, year) %>%
  summarise(mean_rwt = mean(rwt),
            q1_rwt = quantile(rwt, props = 0.25),
            q3_rwt = quantile(rwt, props = 0.75))


plotdf <- left_join(plotdf, rwt_mun)

plotdf <- left_join(plotdf, rwt_us)


ggplot(plotdf, aes(x = rwt_prop/100, y = mean_rwt)) +
  geom_point(aes(color = as.factor(year)))+
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
  theme_kat()

ggplot(plotdf[plotdf$municd %in% majormun,], aes(x = rwt_prop/100, y = mean_rwt)) +
  geom_point(aes(color = as.factor(year)))+
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
  theme_kat()




### water consumption -------------

plotdf <- sim_agg[,-1] %>%
  group_by(municd, year) %>%
  summarise(mean_cons = mean(cons),
            q1_cons = quantile(cons, probs = 0.25),
            q3_cons = quantile(cons, probs = 0.75))




plotdf <- inner_join(plotdf, cslt250_lg[,-2])
plotdf <- inner_join(plotdf, cstot_lg[,-2])

plotdf$dif <- plotdf$mean_cons - plotdf$cslt250
plotdf$difpc <- plotdf$dif*100/plotdf$cslt250
summary(plotdf$difpc)


ggplot(plotdf, aes(x = cslt250, y = mean_cons)) + 
  geom_point()+
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) 

ggplot(plotdf[plotdf$municd %in% majormun,], aes(x = cslt250, y = mean_cons)) + 
  geom_point(aes(color = municd))+
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) 


ggplot(plotdf[plotdf$municd %in% majormun,]) +
  geom_line(aes(x = year, y = mean_cons, color = "sim")) +
  geom_line(aes(x = year, y = cslt250, color = "total small consumptions")) +
  geom_ribbon(aes(x = year, ymin = q1_cons, ymax= q3_cons, fill = "sim"), color = "grey", alpha = 0.3) +
  facet_grid(.~municd, scale = "free") +
  theme_kat()


head(plotdf)
test <-age_ss_wal
test$cons <- test$population*31
plotdf <- test %>% 
  group_by(municd, year) %>%
  summarise(cons = sum(cons, na.rm = T))



  
# 6. simulate for the future ------------------------

## 6.1. hhsize -------------
# generate number of family in each hhsize group using past data 


ft_year <- seq(2020,2040, 5)

# a <- 62063

pred_11_19 <- data.frame(municd = as.character(), ststcd = as.character(), year = as.numeric(), hhsize = as.character(), obs = as.numeric(), pred = as.numeric())

pred_20_40 <- data.frame(municd = as.character(), ststcd = as.character(), year = as.numeric(), hhsize = as.character(), pred = as.numeric())

system.time(for (a in mncdls) {
  fitdf <- hhs_ss_lg[hhs_ss_lg$municd %in% a & !is.na(hhs_ss_lg$obs),]
  
  maxyear <- max(fitdf$year, na.rm = T)
  
  train <- fitdf[fitdf$year < maxyear,]
  vald <- train[!duplicated(train[,c("ststcd", "hhsize")]),]
  vald$year <- 2019
  
  new_tmp <- fitdf[!duplicated(fitdf[,c("ststcd", "hhsize")]),c("municd", "ststcd", "hhsize")]
  n <- nrow(new_tmp)
  
  if (n > 0) {
    
    new_tmp <- new_tmp[rep(1:n, 5),]
    new_tmp$year <- rep(ft_year, each = n)
    
    if (nlvls(fitdf$hhsize) > 1) {
      
      fit1 <- lm(log(obs+1) ~ year*hhsize + hhsize*ststcd, train, na.action = na.exclude)
      fit2 <- lm(log(obs+1) ~ year*hhsize + hhsize*ststcd, fitdf, na.action = na.exclude)
      
    } else {
      
      fit1 <- lm(log(obs+1) ~ year + ststcd, train, na.action = na.exclude)
      fit2 <- lm(log(obs+1) ~ year + ststcd, fitdf, na.action = na.exclude)
    }
    
    train$pred <- round(exp(predict(fit1)) - 1)
    vald$pred <- round(exp(predict(fit1, newdata = vald)) - 1)
    pred_11_19 <- rbind.data.frame(pred_11_19, rbind.data.frame(train, vald))
    
    new_tmp$pred <- round(exp(predict(fit2, newdata = new_tmp)) - 1)
    pred_20_40 <- rbind.data.frame(pred_20_40, new_tmp)   
  }
  
})


hhs_sim_2040 <- dcast(municd + ststcd + year ~ hhsize, data = pred_20_40, value.var = "pred") 

# test <- hhs_sim_2040[complete.cases(hhs_sim_2040),] %>%
#   group_by(year) %>%
#   summarise(tot = sum(nbhh_prv),
#             hhs1 = sum(hhs_1))
# 
# test$hhs1*100/test$tot

save(hhs_sim_2040, file = here(pdir, "cons_proj_Wal", "hhs_sim_2040.Rdata"))

load(file = here(pdir, "cons_proj_Wal", "hhs_sim_2040.Rdata"))

# validating 

ggplot(pred_11_19, aes(x = obs, y = pred, color = as.factor(year))) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red") + scale_color_manual(values = c(rep("black", 8), "red"))

train <- pred_11_19[pred_11_19$year < 2019,]
vald <- pred_11_19[pred_11_19$year %in% 2019,]
r2_train <- 1 - sum((train$pred - train$obs)^2, na.rm = T)/sum((mean(train$obs, na.rm = T) - train$obs)^2, na.rm = T)

r2_vald <- 1 - sum((vald$pred - vald$obs)^2, na.rm = T)/sum((mean(vald$obs, na.rm = T) - vald$obs)^2, na.rm = T)

pred_mun <- pred_20_40 %>%
  group_by(year, municd) %>%
  summarise(prj = sum(pred, na.rm = T),
            count = n())

plotdf <- inner_join(prj_iweps, pred_mun)


ggplot(plotdf, aes(x = prj_nhh, y = prj)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red")

## 6.2. Bltup & rwt scenarios -----------------

## continue using cluster and the cons_proj_cluster.R

bltup_snros <- colnames(bltupss_sim)[-(1:3)]
