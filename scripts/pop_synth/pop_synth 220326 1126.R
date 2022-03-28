#' ---
#' title: "Population synthesis"
#' author: "Nguyen Bich Ngoc, Ismail Saadi, Jacques Teller"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---

#+ r setup, include = F, message = F
# notes from last run----------------------

### currently code of us not working due to different age cut off for adults/children


# rmarkdown::render("scripts/pop_synth/pop_synth.R",output_file=paste0("pop_synth_", format(Sys.time(), "%y%m%d_%H%M"),".md"))

# 1. set up -------------------

## 1.1. load functions -------------

### new functions ------------


loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

# my naive way of applying Bayesian network

hhcomps.f <-
  function(x) {
    n_hhs <- as.numeric(x[1:9])
    ststcd <- x[10]
    year <- as.numeric(x[11])
    bltup <- x[12]
    n <- sum(n_hhs)
    
    res <- data.frame(hhsize = as.numeric(), nad = as.numeric(), nch = as.numeric, ststcd = as.character(), year = as.numeric(), bltup = as.numeric())
    
    if (n > 0) {
      
      hhs_v <- rep(1:9, n_hhs)
      nch <- as.numeric()
      
      for (i in 1:n) {
        # hhs
        
        hhsize <- hhs_v[i]
        
        if (hhsize == 1) {
          nch_tmp <- 0
          
        } else {
          
          pr <- p_rpa_hhs[, dimnames(p_rpa_hhs)[[2]] == hhsize]
          rpa_v <- rmultinom(1, 1, prob = pr)
          rpage <- dimnames(p_rpa_hhs)[[1]][rpa_v == 1]
          
          
          pr1 <-
            p_chd_rpa_blt_hhs[, dimnames(p_chd_rpa_blt_hhs)[[2]] %in% rpage, dimnames(p_chd_rpa_blt_hhs)[[3]] %in% bltup, dimnames(p_chd_rpa_blt_hhs)[[4]] %in% hhsize]
          pr2 <-
            p_chd_blt_hhs[, dimnames(p_chd_blt_hhs)[[2]] %in% bltup, dimnames(p_chd_blt_hhs)[[3]] %in% hhsize]
          pr3 <- p_chd_hhs[, dimnames(p_chd_hhs)[[2]] %in% hhsize]
          if (sum(pr1, na.rm = T) == 1 ){
            nch_v <- rmultinom(1, 1, prob = pr1)
          } else {
            if (sum(pr2, na.rm = T) == 1) {
              nch_v <- rmultinom(1, 1, prob = pr2)
            } else {
              nch_v <- rmultinom(1, 1, prob = pr3)
            }
            
          }
          
          nch_tmp <- dimnames(p_chd_blt_hhs)[[1]][nch_v == 1]
          
        }
        
        nch <- c(nch, nch_tmp)
        
      }
      res <- data.frame(hhsize = hhs_v, nch = nch)
      res$nch <- as.integer(res$nch)
      res$nad <- res$hhsize - res$nch
      res$ststcd <- ststcd
      res$year <- year
      res$bltupss <- bltup
    }
    res
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



source(here("scripts", "general_functions.R"))

### plot params -----------

col1_dark <- scico(1, palette = "lapaz", begin = 0.2)

col1_light <- scico(1, palette = "lapaz", begin = 0.4)

pal_div <- "roma"
pal_con <- "oslo"
pal_disc <- "lapaz"

pal_bg <- 0.2
pal_end <- 0.8

fig_d1 <- 3.54331
fig_d2 <- 5.51181
fig_d3 <- 7.48031


knitr::opts_chunk$set(fig.width = fig_d1,
                      fig.height = fig_d1,
                      dpi = 1000)

## 1.2. load data ------------------------

### data folder --------

rdir <- "data/raw"
pdir <- "data/processed"



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


### builtup density at ss -----------

load(file = here(
  pdir,
  "urban_5cat_Ahmed_Wal/urban_5cat_ss_Wal.Rdata"
))

### projected nbhh IWEPS ----------


prjnhh <- read.xlsx(here(rdir, "sode_proj_IWEPS_Wal", "244601.xlsx"), sheetIndex = 1, startRow = 11)[,c(1:2, 6:8, 5)]
colnames(prjnhh) <- c("municd", "type", "nhh_2020", "nhh_2025", "nhh_2030", "nhh_2035")


prjnhh <- prjnhh[prjnhh$type %in% "Commune", c(1,3:6)]

### projected pop IWEPS ----------


prjpop <- read.xlsx(here(rdir, "sode_proj_IWEPS_Wal", "244600.xlsx"), sheetIndex = 1, startRow = 11)[,c(1:2, 6:8, 5)]
colnames(prjpop) <- c("municd", "type", "pop_2020", "pop_2025", "pop_2030", "pop_2035")


prjpop <- prjpop[prjpop$type %in% "Commune", c(1,3:6)]

knitr::knit_exit()

#+ notcorrectedyet

# 3. process data------------------

## 3.1. utility survey -----------


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

us <- left_join(us, bltupss)

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

hhs_ss_wal <- left_join(hhs_ss_wal, bltupss[, c("ststcd", "LU2010_5cls_x25")])


hhs_ss_lg <- melt(hhs_ss_wal[,c("ststcd", "year", "hhs_1", "hhs_2", "hhs_3", "hhs_4", "hhs_5+")], id.vars = c("ststcd", "year"))


colnames(hhs_ss_lg)[3:4] <- c("hhsize", "obs")

hhs_ss_lg <- left_join(hhs_ss_lg, st_drop_geometry(stst_wal[,c("ststcd", "municd")]))

### houshold size proportion ---------

hhs_ss_wal_sub <- hhs_ss_wal[, grep("hhs_", colnames(hhs_ss_wal), value =T)][,1:5]

hhs_ss <- apply(hhs_ss_wal_sub, 2, sum, na.rm = T)

freq_ss <- hhs_ss/sum(hhs_ss)


hhs_prob_ss <- c(freq_ss[1:4], freq_ss[5]*bighh_ggs$prop_ggs)

### age -------------

age_ss_wal <- age_ss[age_ss$regicd %in% 3000,]
age_ss_wal <- 
  age_ss_wal[, c(grep("age_", colnames(age_ss_wal), value = T), "ststcd", "year", "population")]

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







## 3.4. rainwater tank ----------

## 3.5. projected nbhh ---------

prjnhh_lg <- melt(prjnhh, id.vars = "municd")
colnames(prjnhh_lg)[3] <- "prj_iweps"
prjnhh_lg$year <- as.numeric(gsub("nhh_", "", prjnhh_lg$variable))

prjnhh_lg <- prjnhh_lg[order(prjnhh_lg$municd, prjnhh_lg$year), c("municd", "year", "prj_iweps")]

## 3.6. projected pop ---------

prjpop_lg <- melt(prjpop, id.vars = "municd")
colnames(prjpop_lg)[3] <- "prj_iweps"
prjpop_lg$year <- as.numeric(gsub("pop_", "", prjpop_lg$variable))

prjpop_lg <- prjpop_lg[order(prjpop_lg$municd, prjpop_lg$year), c("municd", "year", "prj_iweps")]

# 4. exploring ------

## 4.1. rainwater tank -----------

rwt_us <- us %>%
  group_by(municd) %>%
  summarise(rwt_prop_us = sum(rwtank == "yes")*100/n(),
            count_us = n())

plotdf <- inner_join(rwt_us, rwt_mun)

cor.test(plotdf$rwt_prop_us[plotdf$count_us > 20], plotdf$rwt_prop[plotdf$count_us > 20])

plot(plotdf$rwt_prop_us[plotdf$count_us > 20], plotdf$rwt_prop[plotdf$count_us > 20])
abline(a = 0, b = 1, col = col1_dark)

summary(rwt_mun$rwt_propna)

## 4.2. age --------

us_age <- 2014 - us[grep("by", colnames(us))]

us_age[us_age < 0] <- NA

us_age_v <- unlist(us_age)
us_age_cat <- cut(us_age_v, breaks = c(seq(0, 80, 5), 150), include.lowest = T, right = F)


plotdf <- as.data.frame(prop.table(table(unlist(us_age_cat))))

colnames(plotdf) <- c("age_cat", "freq_us")

ggs_age_cat <- cut(unlist(ggs_age), breaks = c(seq(0, 80, 5), 150), include.lowest = T, right = F)

plotdf$freq_ggs <- prop.table(table(ggs_age_cat))



age_ss_2014 <- apply(age_ss_wal[age_ss_wal$year %in% 2014, grep("age_", colnames(age_ss_wal))],  2, sum, na.rm = T)

plotdf$freq_ss <- age_ss_2014/sum(age_ss_2014)


ggplot(plotdf, aes(x = age_cat)) +
  geom_line(aes(y = freq_us, col = "us", group = 1), size = 1)  +
  geom_line(aes(y = freq_ggs, col = "ggs", group = 1), size = 1) +
  geom_line(aes(y = freq_ss, col = "ss", group = 1), size = 1) +
  theme_kat()  +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)

cor.test(plotdf$freq_ggs, plotdf$freq_ss)
cor.test(plotdf$freq_us, plotdf$freq_ss)


## 4.3. hhsize -------------

plotdf <- data.frame(prob_us = as.numeric(hhs_prob_us), prob_ggs = as.numeric(hhs_prob_ggs), prob_ss = hhs_prob_ss)

plotdf$hhs <- paste0("hhs_", 1:9)


ggplot(plotdf, aes(x = hhs)) +
  geom_line(aes(y = prob_us, col = "us", group = 1), size = 1)  +
  geom_line(aes(y = prob_ggs, col = "ggs", group = 1), size = 1) +
  geom_line(aes(y = prob_ss, col = "ss", group = 1), size = 1) +
  theme_kat()  +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)







test <- us %>%
  group_by(dstrcd) %>%
  summarise(n = n(),
            n5 = sum(hhs_tot %in% 5),
            n6 = sum(hhs_tot %in% 6),
            n7 = sum(hhs_tot %in% 7),
            n8 = sum(hhs_tot %in% 8),
            n9 = sum(hhs_tot %in% 9))

test1 <- t(apply(test[,3:7], 1, function(x) x*100/sum(x)))

test2 <- reshape2::melt(test1[test$n > 120,], id.vars = NULL)

ggplot(test2, aes(x = Var2, y = value, group = Var1)) + geom_line()


## CONCLUSION: if the sample size is large enough the distribution of hhsize from 5 to 9 are quite close to each other => sampling sample size from the group of 5+ using the distribution of whole ggs data (which almost identical with survey data)

plotdf <- hhs_ss_wal[, c("ststcd", "year", "LU2010_5cls_x25", grep("hhs_", colnames(hhs_ss_wal), value = T))]

propdf <- data.frame(t(apply(plotdf[, -c(1:3)], 1, function(x) x*100/sum(x))))

plotdf <- cbind.data.frame(plotdf[, 1:3], propdf)

plotdf <- melt(plotdf, id.vars = c("ststcd", "year", "LU2010_5cls_x25"))

plotdf <- inner_join(plotdf, age_ss_wal[, c("ststcd", "year", "dens_cat")]) 


sample <- sample(unique(plotdf$ststcd), 6)

ggplot(plotdf[plotdf$ststcd %in% sample,], aes(x = variable, y = value, color = as.factor(year))) + geom_line(aes(group = as.factor(year))) + facet_wrap(. ~ ststcd)

### CONCLUSION the changes in relative proportion of households size in each stst through time is marginal

sample <- sample(unique(plotdf$ststcd), 100)

ggplot(plotdf[plotdf$ststcd %in% sample,], aes(x = variable, y = value, color = as.factor(LU2010_5cls_x25), group = ststcd)) + geom_line() + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)

ggplot(plotdf[plotdf$ststcd %in% sample,], aes(x = variable, y = value, color = dens_cat, group = ststcd)) + geom_line() + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)


ggplot(plotdf, aes(x = variable, y = value, color = as.factor(LU2010_5cls_x25), group = as.factor(LU2010_5cls_x25))) + stat_summary(geom = "line", fun = "mean") + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)

ggplot(plotdf, aes(x = variable, y = value, color = dens_cat, group = dens_cat)) + stat_summary(geom = "line", fun = "mean") + facet_grid(.~ year) + scale_color_scico_d(end = 0.8)

summary(lm(value ~ as.factor(LU2010_5cls_x25), data = plotdf[plotdf$variable %in% "hhs_1",]))

summary(lm(value ~ dens_cat, data = plotdf[plotdf$variable %in% "hhs_1",]))

summary(lm(value ~ as.factor(LU2010_5cls_x25), data = plotdf[plotdf$variable %in% "hhs_4",]))

summary(lm(value ~ dens_cat, data = plotdf[plotdf$variable %in% "hhs_4",]))


hhs_prob_dens <- plotdf %>%
  group_by(dens_cat, variable) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T))

hhs_prob_dens <- hhs_prob_dens[complete.cases(hhs_prob_dens),]

# 5. Simulation for the past-------------


## 5.1. hhsize ---------


# count from macro data for hhs below 5 at stst level 
# count from ggs for hhs from 5-9
# use nbhh_prv as the targeted number of synthesised families
# sample the hhs_NA (=nbhh_prv - sum(hhs_1:5+)) using stst specific prob if available
# or averaged of all stst


bighh_prob <- bighh_ggs$prop_ggs

res <- data.frame(matrix(0, nrow = nrow(hhs_ss_wal), ncol = 9))
colnames(res) <- paste("hhs", 1:9, sep = "_")
res$ststcd <- hhs_ss_wal$ststcd
res$year <- hhs_ss_wal$year

system.time(

for (i in 1:nrow(hhs_ss_wal)) {
  
  res[i, 1:4] <- sapply(hhs_ss_wal[i, c("hhs_1", "hhs_2", "hhs_3", "hhs_4")], function(x) ifelse(is.na(x), 0, x))
  
  n_bhh <- hhs_ss_wal[i, "hhs_5+"]
  
  if (!is.na(n_bhh) & n_bhh > 0) {
    bhh_v <- data.frame(rmultinom(n_bhh, 1, prob = bighh_prob))
    res[i, 5:9] <- apply(bhh_v, 1, sum)
  } else {
    res[i, 5:9] <- 0
  }
  
  
  n_NA <- hhs_ss_wal$nbhh_prv[i] - sum(res[i, 1:9], na.rm = T)
  
  if (!is.na(n_NA) & n_NA > 0) {
    hhs_prob <- res[i, 1:9]/sum(res[i, 1:9])
    
    if (sum(hhs_prob, na.rm = T) == 1) {
      
      hhs_v <- data.frame(rmultinom(n_NA, 1, prob = hhs_prob))
    } else {
      hhs_v <- data.frame(rmultinom(n_NA, 1, prob = hhs_prob_ss))
    }
    
    res[i, 1:9] <- res[i, 1:9] + apply(hhs_v, 1, sum)
  }
}

)

hhs_sim <- res

hhs_sim_agg <- res[,c("ststcd", "year", "hhs_1", "hhs_2", "hhs_3", "hhs_4")]
hhs_sim_agg$`hhs_5+` <- apply(res[,c("hhs_5", "hhs_6", "hhs_7", "hhs_8", "hhs_9")], 1, sum, na.rm = T)

hhs_sim_agg$tot <- apply(hhs_sim_agg[,c("hhs_1", "hhs_2", "hhs_3", "hhs_4", "hhs_5+")], 1, sum, na.rm = T)

hhs_sim_agg_lg <- melt(hhs_sim_agg[,1:7], id.vars = c("ststcd", "year"))

colnames(hhs_sim_agg_lg)[3:4] <- c("hhsize", "hhs_sim")

hhs_sim_agg_lg <- left_join(hhs_sim_agg_lg, st_drop_geometry(stst_wal[,c("ststcd", "municd")]))

plotdf1 <- inner_join(hhs_ss_lg, hhs_sim_agg_lg)


ggplot(plotdf1, aes(x = obs, y = hhs_sim)) +
  geom_point()  +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
  facet_wrap(.~ hhsize, scales = "free") + 
  theme_kat()
  

plotdf2 <- inner_join(hhs_sim_agg[,c("ststcd", "year", "tot")],  hhs_ss_wal[,c("ststcd", "year", "nbhh_prv")])

ggplot(plotdf2, aes(x = nbhh_prv, y = tot))+
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
  theme_kat()



## 5.2. nb adults and children ----------


#### fitting ----------------



p_rpa_hhs <- prop.table(table(us$rpage_cat, us$hhs_tot), margin = 2)


#### family type given hhs and rpage 

p_chd_rpa_blt_hhs <- prop.table(table(us$hhs_0_19, us$rpage_cat, us$LU2010_5cls_x25, us$hhs_tot), margin = c(2:4))


# p_chd_blt_rpa_hhs[, dimnames(p_chd_blt_rpa_hhs)[[2]] == 0, dimnames(p_chd_blt_rpa_hhs)[[3]] == "[40,45)", dimnames(p_chd_blt_rpa_hhs)[[4]] == 3]


p_chd_blt_hhs <- prop.table(table(us$hhs_0_19, us$LU2010_5cls_x25, us$hhs_tot), margin = c(2:3))

p_chd_hhs <- prop.table(table(us$hhs_0_19, us$hhs_tot), margin = 2)


#### generating --------------------

hhs_sim <- left_join(hhs_sim, bltupss[, c("ststcd", "LU2010_5cls_x25")])

# colnames(hhs_sim)
# x <- unlist(hhs_sim[1,])


system.time(nad_sim <- apply(hhs_sim, 1 , hhcomps.f))

#### validating -----------------

nad_sim_ss <- hhs_sim[, c("ststcd", "year")]
nad_sim_ss$nad <- NA
nad_sim_ss$nch <- NA

for (i in 1:length(nad_sim)) {
  if (nrow(nad_sim[[i]]) > 0) {
    nad_sim_ss[i, "nad"] <- sum(nad_sim[[i]]$nad, na.rm = T)
    nad_sim_ss[i, "nch"] <- sum(nad_sim[[i]]$nch, na.rm = T)    
  }

}

nad_sim_ss$bltup <- hhs_sim$LU2010_5cls_x25



plotdf <- inner_join(age_ss_wal, nad_sim_ss)

colnames(age_ss_wal)


ggplot(plotdf, aes(x = hhs_20_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_kat()





ggplot(plotdf, aes(x = hhs_0_19, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()


ggplot(plotdf, aes(x = hhs_20_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  facet_wrap(.~ bltup) +
  theme_kat()



ggplot(plotdf, aes(x = hhs_0_19, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  + 
  facet_wrap(.~ bltup) +
  theme_kat()


### fitting -------------------------
### follow Mistry 
#### select data 


families <- ggs[ggs$hhs_tot > 1,]

rel_mat <- families[, grep("ahg3_", colnames(families))][, -1]

## there are 2 families with 2 spouses (error in inputing data) => remove

nsp <- apply(rel_mat, 1, function(x) sum(x == "partner or spouse", na.rm = T))

families <- families[nsp<2,]

rel_mat <- rel_mat[nsp < 2,]
age_mat <- families[, grep("ahg5_", colnames(families))][, -1]

#### p reference person age |hhs

p_rpa_hhs <- prop.table(table(ggs$ahg5_1, ggs$hhs_tot), margin = 2)


#### p family type 
rel_freq <- data.frame(table(unlist(rel_mat)))

rel_freq <- rel_freq[order(rel_freq$Freq, decreasing = T),]


fam_type <- apply(rel_mat, 1, function(x) {paste(lvls(factor(x, levels = rel_freq$Var1)), collapse = ",")})

p_ft_rpa_hhs <- prop.table(table(fam_type, families$ahg5_1,families$hhs_tot), margin = c(2,3))

p_ft_rpa_hhs[,2,2]
# 
# dimnames(p_ft_rpa_hhs)


#### spouse/partner 
# only in person 2

p_sp_rpa <- prop.table(table(families$ahg3_2 %in% "partner or spouse", families$ahg5_1), margin = 2)

spage <- families$ahg5_2
spage[(!families$ahg3_2 == "partner or spouse")] <- NA

p_spa_rpa <- prop.table(table(spage, families$ahg5_1), margin = 2)
sum(p_spa_rpa[,5], na.rm = T)

p_rpg_rpa <- prop.table(table(families$ahg4_1, families$ahg5_1), margin = 2)

p_spa_rpg_rpa <- prop.table(table(spage, families$ahg4_1, families$ahg5_1), margin = c(2,3))

#### other relations
relations <- apply(rel_mat, 1, function(x) lvls(as.factor(x))
)
relations <- lvls(unlist(relation, recursive = T))
relations <- relations[!(relations %in% "partner or spouse")]


p_rel_ls <- lapply(relations, function(rel) {
  
  rel_count <- apply(rel_mat, 1, function(x) sum(x == rel, na.rm = T))
  p_nr_rpa <- prop.table(table(rel_count, families$ahg5_1), margin = 2)
  rel_age <- age_mat
  rel_age[rel_mat != rel] <- NA
  rel_age_v <- as.vector(t(rel_age))
  rel_age_v <- rel_age_v[!is.na(rel_age_v)]
  rpage <- rep(families$ahg5_1, rel_count)
  p_ra_rpa <- prop.table(table(rel_age_v, rpage), margin = 2)
  list(p_nr_rpa, p_ra_rpa)
})

names(p_rel_ls) <- relations

# p_rel_ls[names(p_rel_ls) %in% "a non-relative"][[1]][[1]]


### generate --------------


mistry.f <-
  function(x) {
    n_hhs <- as.numeric(x[1:9])
    ststcd <- x[10]
    year <- as.numeric(x[11])
    n <- sum(n_hhs)
    
    res <- list()
    
    if (n > 0) {
      
      hhs_v <- rep(1:9, n_hhs)
      
      for (i in 1:n) {
        # hhs
        
        hhsize <- hhs_v[i]
        
        # rpage
        pr <- p_rpa_hhs[, dimnames(p_rpa_hhs)[[2]] == hhsize]
        rpa_v <- rmultinom(1, 1, prob = pr)
        rpage <- dimnames(p_rpa_hhs)[[1]][rpa_v == 1]
        age <- rpage
        hhs_tmp <- 1
        
        if (hhsize == 1) {
          ft <- "single member"
          
        } else {
          #famtype
          
          pr <-
            p_ft_rpa_hhs[, dimnames(p_ft_rpa_hhs)[[2]] %in% rpage, dimnames(p_ft_rpa_hhs)[[3]] %in% hhsize]
          ft_v <- rmultinom(1, 1, prob = pr)
          ft <- dimnames(p_ft_rpa_hhs)[[1]][ft_v == 1]
          rels <- unlist(strsplit(ft, split = ","))
          
          if ("partner or spouse" %in% rels) {
            pr <- p_spa_rpa[, dimnames(p_spa_rpa)[[2]] == rpage]
            spa_v <- rmultinom(1, 1, prob = pr)
            spa <- dimnames(p_spa_rpa)[[1]][spa_v == 1]
            age <- c(age, spa)
            hhs_tmp <- hhs_tmp + 1
          }
          
          rels <- rels[rels != "partner or spouse"]
          if (length(rels) > 0) {
            for (a in rels) {
              pr_tab <- p_rel_ls[names(p_rel_ls) == a][[1]][[2]]
              pr <- pr_tab[, dimnames(pr_tab)[[2]] == rpage]
              rela_v <- rmultinom(1, 1, prob = pr)
              rela <- dimnames(pr_tab)[[1]][rela_v == 1]
              age <- c(age, rela)
              hhs_tmp <- hhs_tmp + 1
            }
            
            for (a in rels) {
              pr_tab <- p_rel_ls[names(p_rel_ls) == a][[1]][[1]]
              pr <- pr_tab[, dimnames(pr_tab)[[2]] == rpage]
              nrel_v <- rmultinom(1, 1, prob = pr)
              nrel <-
                as.numeric(dimnames(pr_tab)[[1]][nrel_v == 1])
              nrel_tmp <- 1
              
              while (nrel_tmp < nrel & hhs_tmp < hhsize) {
                pr_tab <- p_rel_ls[names(p_rel_ls) == a][[1]][[2]]
                pr <- pr_tab[, dimnames(pr_tab)[[2]] == rpage]
                rela_v <- rmultinom(1, 1, prob = pr)
                rela <- dimnames(pr_tab)[[1]][rela_v == 1]
                age <- c(age, rela)
                hhs_tmp <- hhs_tmp + 1
                nrel_tmp <- nrel_tmp + 1
              }
            }
          }
        }
        
        res[[i]] <- list(hhsize, ft, age)
        
      }
    }
    res
  }

system.time(fam_sim <- apply(hhs_sim, 1 , mistry.f))

# test <- fam_sim[[1]]

### validate -------------

hhs_sim_mistry <- hhs_sim

hhs_sim_mistry[,1:9] <- 0
 
for (i in 1:nrow(hhs_sim_mistry)) {
  if (length(fam_sim[[i]]) > 0) {
    hhs <- sapply(fam_sim[[i]], function(y) y[[1]][[1]])   
    res <- data.frame(table(hhs)) 
    hhs_sim_mistry[i, as.integer(res$hhs)] <- res$Freq    
  }
}

hhs_sim_mistry$tot <- apply(hhs_sim_mistry[,1:9], 1, sum, na.rm = T)

hhs_sim_mistry$pop <- hhs_sim_mistry$hhs_1 + hhs_sim_mistry$hhs_2*2 + hhs_sim_mistry$hhs_3*3 + hhs_sim_mistry$hhs_4*4 + hhs_sim_mistry$hhs_5*5 + hhs_sim_mistry$hhs_6*6 + hhs_sim_mistry$hhs_7*7 + hhs_sim_mistry$hhs_8*8 + hhs_sim_mistry$hhs_9*9

hhs_sim_mistry_agg <- hhs_sim_mistry[, c(1:4, 10:11)]
hhs_sim_mistry_agg$`hhs_5+` <- apply(hhs_sim_mistry[, 5:9], 1, sum, na.rm = T)

hhs_sim_mistry_lg <- melt(hhs_sim_mistry_agg, id.vars = c("ststcd", "year"))

colnames(hhs_sim_mistry_lg)[3:4] <- c("hhsize", "hhs_sim")

hhs_sim_mistry_lg <- left_join(hhs_sim_mistry_lg, st_drop_geometry(stst_wal[,c("ststcd", "municd")]))

plotdf1 <- inner_join(hhs_ss_lg, hhs_sim_mistry_lg)
plotdf1 <- inner_join(plotdf1, bltupss[,1:2])

ggplot(plotdf1, aes(x = obs, y = hhs_sim)) +
  geom_point()  +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
  facet_wrap(.~ hhsize, scales = "free") + 
  theme_kat()


plotdf2 <- inner_join(hhs_sim_mistry[,c("ststcd", "year", "tot")],  hhs_ss_wal[,c("ststcd", "year", "nbhh_prv")])

ggplot(plotdf2, aes(x = nbhh_prv, y = tot))+
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) + 
  theme_kat()


plotdf <- inner_join(hhs_sim_mistry[, c("ststcd", "year", "pop")], age_ss_wal[,c("ststcd", "year", "population")])


ggplot(plotdf, aes(x = population, y = pop)) + geom_point()
#### 

age_sim <- age_ss_wal[,1:19]


age_sim <- left_join(hhs_sim_mistry[, c("ststcd", "year")], age_sim)

age_sim[,3:19] <- 0


system.time(for (i in 1:nrow(age_sim)) {
  if (length(fam_sim[[i]]) > 0) {
    age <- as.numeric(unlist(sapply(fam_sim[[i]], function(y) y[[3]])))
    age_cat <- cut(age, c(seq(0, 80,5), 200))
    age_sim[i, 3:19] <- data.frame(table(age_cat))$Freq    
  }
}
)


age_sim_lg <- melt(age_sim, id.vars = c("ststcd", "year"))

colnames(age_sim_lg)[3:4] <- c("age_grp", "sim")

age_wal_lg <- melt(age_ss_wal[,1:19], id.vars = c("ststcd", "year"))

colnames(age_wal_lg)[3:4] <- c("age_grp", "obs")

plotdf <- inner_join(age_wal_lg, age_sim_lg)


plotdf <- left_join(plotdf, bltupss[,1:2])


ggplot(plotdf, aes(x = obs, y = sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_kat()


ggplot(plotdf, aes(x = obs, y = sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  facet_wrap(.~ age_grp) +
  theme_kat()


colnames(age_sim)
age_sim$nad_20_sim <- apply(age_sim[, 7:19], 1, sum, na.rm = T)
age_sim$nad_15_sim <- apply(age_sim[, 6:19], 1, sum, na.rm = T) 
age_sim$nch_14_sim <- apply(age_sim[, 3:5], 1, sum, na.rm = T)
age_sim$nch_19_sim <- apply(age_sim[, 3:6], 1, sum, na.rm = T)
age_sim$pop_sim <- apply(age_sim[, 3:19], 1, sum, na.rm = T)


colnames(age_ss_wal)
age_ss_wal$nad_20_obs <- apply(age_ss_wal[, 5:17], 1, sum, na.rm = T)
age_ss_wal$nad_15_obs <- apply(age_ss_wal[, 4:17], 1, sum, na.rm = T) 
age_ss_wal$nch_14_obs <- apply(age_ss_wal[, 1:3], 1, sum, na.rm = T)
age_ss_wal$nch_19_obs <- apply(age_ss_wal[, 1:4], 1, sum, na.rm = T)
age_ss_wal$pop_obs <- apply(age_ss_wal[, 1:17], 1, sum, na.rm = T)

plotdf <- inner_join(age_sim[, c("ststcd", "year", "nad_20_sim", "nad_15_sim", "nch_14_sim", "nch_19_sim", "pop_sim")], age_ss_wal[, c("ststcd", "year", "nad_20_obs", "nad_15_obs", "nch_14_obs", "nch_19_obs", "pop_obs")])




ggplot(plotdf, aes(x = pop_obs, y = pop_sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()


ggplot(plotdf, aes(x = nad_20_obs, y = nad_20_sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()

ggplot(plotdf, aes(x = nad_15_obs, y = nad_15_sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()



ggplot(plotdf, aes(x = nch_14_obs, y = nch_14_sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)   +
  theme_kat()


ggplot(plotdf, aes(x = nch_19_obs, y = nch_19_sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)   +
  theme_kat()

### trial 1 -----------
### age group:1
### using simulate data with number of family with hhsize above 5

### rpage given hhs 

 
p_hhs_rpa <- prop.table(table(ggs$hhs_tot, ggs$ahg5_1), margin = 1)

 
#### family type given hhs and rpage 

p_hhs_rpa_nad <- prop.table(table(ggs$hhs_tot, ggs$ahg5_1, ggs$hhs_15_95), margin = c(1,2))




nbn.f <-
  function(n = 100,
           hhsize = 2,
           pt2w = p_hhs_rpa,
           pt3w = p_hhs_rpa_nad) {
    
    if (!is.na(n) & n > 0) {
      df <- data.frame(hhs = rep(hhsize, n), rpage = rep(NA, n), nad = rep(NA,n))
      
      for (i in 1:nrow(df)) {
        
        p_2w <- pt2w[dimnames(pt2w)[[1]] %in% hhsize,]
        rpa_v <- rmultinom(1, 1, prob = p_2w) 
        rpage <- dimnames(pt2w)[[2]][rpa_v == 1]
        df$rpage[i] <- rpage
        
        p_3w <- pt3w[dimnames(pt3w)[[1]] %in% hhsize, dimnames(pt3w)[[2]] %in% rpage,]
        p_3w[is.na(p_3w)] <- 0
        
        if (sum(p_3w, na.rm = T) == 1) {
          nad_v <- rmultinom(1, 1, p_3w)
        } else {
          nad_v <- rmultinom(1, 1, prob = rep(1/hhsize, hhsize))
        }
        
        nad <- dimnames(pt3w)[[3]][which(nad_v %in% 1)]
        df$nad[i] <- as.numeric(nad)
        
        
      }
      
      df$nch <- df$hhs - df$nad
      
    } 
    
    df
  }


hhs_2_sim <- nbn.f(n = 10000, hhsize = 2)
hhs_3_sim <- nbn.f(n = 10000, hhsize = 3)
hhs_4_sim <- nbn.f(n = 10000, hhsize = 4)
hhs_5_sim <- nbn.f(n = 10000, hhsize = 5)
hhs_6_sim <- nbn.f(n = 10000, hhsize = 6)
hhs_7_sim <- nbn.f(n = 10000, hhsize = 7)
hhs_8_sim <- nbn.f(n = 10000, hhsize = 8)
hhs_9_sim <- nbn.f(n = 10000, hhsize = 9)


system.time(nad_sim <- apply(hhs_sim, 1, function(x) {
  hhs1_tmp <- data.frame(hhs = 1, rpage = NA, nad = 1, nch = 0)
  hhs1_tmp <- hhs1_tmp[rep(1, x[[1]]),]
  hhs2_tmp <- hhs_2_sim[sample(1:10000, size = ifelse(is.na(x[[2]]), 0, x[[2]])), ]
  hhs3_tmp <- hhs_3_sim[sample(1:10000, size = ifelse(is.na(x[[3]]), 0, x[[3]])), ]
  hhs4_tmp <- hhs_4_sim[sample(1:10000, size = ifelse(is.na(x[[4]]), 0, x[[4]])), ]
  hhs5_tmp <- hhs_5_sim[sample(1:10000, size = ifelse(is.na(x[[5]]), 0, x[[5]])), ]
  hhs6_tmp <- hhs_6_sim[sample(1:10000, size = ifelse(is.na(x[[6]]), 0, x[[6]])), ]
  hhs7_tmp <- hhs_7_sim[sample(1:10000, size = ifelse(is.na(x[[7]]), 0, x[[7]])), ]
  hhs8_tmp <- hhs_8_sim[sample(1:10000, size = ifelse(is.na(x[[8]]), 0, x[[8]])), ]
  hhs9_tmp <- hhs_9_sim[sample(1:10000, size = ifelse(is.na(x[[9]]), 0, x[[9]])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp, hhs6_tmp, hhs7_tmp, hhs8_tmp, hhs9_tmp))
  if (nrow(res) > 0) {
    res$ststcd <- x[[10]]
    res$year <- x[[11]]    
  }
  res
})
)


x <- nad_sim[[1]]
system.time(nad_sim_ss <- lapply(nad_sim, function(x) {
  n <- nrow(x)
  nad <- sum(x$nad, na.rm = T)
  nch <- sum(x$nch, na.rm = T)
  res <- data.frame(n, nad, nch)
}))

system.time(nad_sim_ss <- Reduce(rbind.data.frame, nad_sim_ss))

nad_sim_ss$ststcd <- hhs_sim$ststcd
nad_sim_ss$year <- hhs_sim$year


plotdf <- inner_join(age_ss_wal, nad_sim_ss)


plotdf <- left_join(plotdf, bltupss[,1:2])


ggplot(plotdf, aes(x = hhs_15_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_kat()





ggplot(plotdf, aes(x = hhs_0_14, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()


ggplot(plotdf, aes(x = hhs_15_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()



ggplot(plotdf, aes(x = hhs_0_14, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()


### trial 2 -----------
### age group: 5
### using simulate data with number of family with hhsize above 5

### rpage given hhs 


p_hhs_rpa <- prop.table(table(ggs$hhs_tot, ggs$rpage_cat), margin = 1)



p_hhs_nad <- prop.table(table(ggs$hhs_tot, ggs$hhs_15_95), margin = 1)


nbn.f <-
  function(n = 100,
           hhsize = 2,
           pt2w = p_hhs_rpa,
           pt2w2 = p_hhs_nad) {
    
    if (!is.na(n) & n > 0) {
      df <- data.frame(hhs = rep(hhsize, n), rpage = rep(NA, n), nad = rep(NA,n))
      
      for (i in 1:nrow(df)) {
        
        p_2w <- pt2w[dimnames(pt2w)[[1]] %in% hhsize,]
        rpa_v <- rmultinom(1, 1, prob = p_2w) 
        rpage <- dimnames(pt2w)[[2]][rpa_v == 1]
        df$rpage[i] <- rpage
        
        p_2w2 <- pt2w2[dimnames(pt2w2)[[1]] %in% hhsize, ]
        nad_v <- rmultinom(1, 1, p_2w2)
        
        
        
        nad <- dimnames(pt2w2)[[2]][which(nad_v %in% 1)]
        df$nad[i] <- as.numeric(nad)
        
        
      }
      
      df$nch <- df$hhs - df$nad
      
    } 
    
    df
  }



hhs_2_sim <- nbn.f(n = 10000, hhsize = 2)
hhs_3_sim <- nbn.f(n = 10000, hhsize = 3)
hhs_4_sim <- nbn.f(n = 10000, hhsize = 4)
hhs_5_sim <- nbn.f(n = 10000, hhsize = 5)
hhs_6_sim <- nbn.f(n = 10000, hhsize = 6)
hhs_7_sim <- nbn.f(n = 10000, hhsize = 7)
hhs_8_sim <- nbn.f(n = 10000, hhsize = 8)
hhs_9_sim <- nbn.f(n = 10000, hhsize = 9)


system.time(nad_sim <- apply(hhs_sim, 1, function(x) {
  hhs1_tmp <- data.frame(hhs = 1, rpage = NA, nad = 1, nch = 0)
  hhs1_tmp <- hhs1_tmp[rep(1, x[[1]]),]
  hhs2_tmp <- hhs_2_sim[sample(1:10000, size = ifelse(is.na(x[[2]]), 0, x[[2]])), ]
  hhs3_tmp <- hhs_3_sim[sample(1:10000, size = ifelse(is.na(x[[3]]), 0, x[[3]])), ]
  hhs4_tmp <- hhs_4_sim[sample(1:10000, size = ifelse(is.na(x[[4]]), 0, x[[4]])), ]
  hhs5_tmp <- hhs_5_sim[sample(1:10000, size = ifelse(is.na(x[[5]]), 0, x[[5]])), ]
  hhs6_tmp <- hhs_6_sim[sample(1:10000, size = ifelse(is.na(x[[6]]), 0, x[[6]])), ]
  hhs7_tmp <- hhs_7_sim[sample(1:10000, size = ifelse(is.na(x[[7]]), 0, x[[7]])), ]
  hhs8_tmp <- hhs_8_sim[sample(1:10000, size = ifelse(is.na(x[[8]]), 0, x[[8]])), ]
  hhs9_tmp <- hhs_9_sim[sample(1:10000, size = ifelse(is.na(x[[9]]), 0, x[[9]])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp, hhs6_tmp, hhs7_tmp, hhs8_tmp, hhs9_tmp))
  if (nrow(res) > 0) {
    res$ststcd <- x[[10]]
    res$year <- x[[11]]    
  }
  res
})
)


# x <- nad_sim[[1]]
system.time(nad_sim_ss <- lapply(nad_sim, function(x) {
  n <- nrow(x)
  nad <- sum(x$nad, na.rm = T)
  nch <- sum(x$nch, na.rm = T)
  res <- data.frame(n, nad, nch)
}))

system.time(nad_sim_ss <- Reduce(rbind.data.frame, nad_sim_ss))

nad_sim_ss$ststcd <- hhs_sim$ststcd
nad_sim_ss$year <- hhs_sim$year


plotdf <- inner_join(age_ss_wal, nad_sim_ss)


plotdf <- left_join(plotdf, bltupss[,1:2])

ggplot(plotdf, aes(x = hhs_15_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_kat()





ggplot(plotdf, aes(x = hhs_0_14, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()


ggplot(plotdf, aes(x = hhs_15_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()



ggplot(plotdf, aes(x = hhs_0_14, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()

### trial 3 -----------
### age group:1
### using simulate data with number of family with hhsize above 5

### rpage given hhs 


p_hhs_rpa <- prop.table(table(ggs$hhs_tot, ggs$rpage_cat), margin = 1)


#### family type given hhs and rpage 

p_hhs_rpa_chd <- prop.table(table(ggs$hhs_tot, ggs$rpage_cat, ggs$hhs_0_19), margin = c(1,2))

p_hhs_chd <- prop.table(table(ggs$hhs_tot, ggs$hhs_0_19), margin = 1)


nbn.f <-
  function(n = 100,
           hhsize = 2,
           pt2w1 = p_hhs_rpa,
           pt3w = p_hhs_rpa_chd,
           pt2w2 = p_hhs_chd) {
    
    if (!is.na(n) & n > 0) {
      df <- data.frame(hhs = rep(hhsize, n), rpage = rep(NA, n), nad = rep(NA,n))
      
      for (i in 1:nrow(df)) {
        
        p_2w1 <- pt2w1[dimnames(pt2w1)[[1]] %in% hhsize,]
        rpa_v <- rmultinom(1, 1, prob = p_2w1) 
        rpage <- dimnames(pt2w1)[[2]][rpa_v == 1]
        df$rpage[i] <- rpage
        
        p_3w <- pt3w[dimnames(pt3w)[[1]] %in% hhsize, dimnames(pt3w)[[2]] %in% rpage,]
        p_3w[is.na(p_3w)] <- 0
        
        p_2w2 <- pt2w2[dimnames(pt2w2)[[1]] %in% hhsize,]
        
        if (sum(p_3w, na.rm = T) == 1) {
          nch_v <- rmultinom(1, 1, p_3w)
          nch <- dimnames(pt3w)[[3]][which(nch_v %in% 1)]          
        } else {
          nch_v <- rmultinom(1, 1, p_2w2)
          nch <- dimnames(pt2w2)[[2]][which(nch_v %in% 1)] 
        }
        

        df$nch[i] <- as.numeric(nch)
        
        
      }
      
      df$nad <- df$hhs - df$nch
      
    } 
    
    df
  }


hhs_2_sim <- nbn.f(n = 10000, hhsize = 2)
hhs_3_sim <- nbn.f(n = 10000, hhsize = 3)
hhs_4_sim <- nbn.f(n = 10000, hhsize = 4)
hhs_5_sim <- nbn.f(n = 10000, hhsize = 5)
hhs_6_sim <- nbn.f(n = 10000, hhsize = 6)
hhs_7_sim <- nbn.f(n = 10000, hhsize = 7)
hhs_8_sim <- nbn.f(n = 10000, hhsize = 8)
hhs_9_sim <- nbn.f(n = 10000, hhsize = 9)


system.time(nad_sim <- apply(hhs_sim, 1, function(x) {
  hhs1_tmp <- data.frame(hhs = 1, rpage = NA, nad = 1, nch = 0)
  hhs1_tmp <- hhs1_tmp[rep(1, x[[1]]),]
  hhs2_tmp <- hhs_2_sim[sample(1:10000, size = ifelse(is.na(x[[2]]), 0, x[[2]])), ]
  hhs3_tmp <- hhs_3_sim[sample(1:10000, size = ifelse(is.na(x[[3]]), 0, x[[3]])), ]
  hhs4_tmp <- hhs_4_sim[sample(1:10000, size = ifelse(is.na(x[[4]]), 0, x[[4]])), ]
  hhs5_tmp <- hhs_5_sim[sample(1:10000, size = ifelse(is.na(x[[5]]), 0, x[[5]])), ]
  hhs6_tmp <- hhs_6_sim[sample(1:10000, size = ifelse(is.na(x[[6]]), 0, x[[6]])), ]
  hhs7_tmp <- hhs_7_sim[sample(1:10000, size = ifelse(is.na(x[[7]]), 0, x[[7]])), ]
  hhs8_tmp <- hhs_8_sim[sample(1:10000, size = ifelse(is.na(x[[8]]), 0, x[[8]])), ]
  hhs9_tmp <- hhs_9_sim[sample(1:10000, size = ifelse(is.na(x[[9]]), 0, x[[9]])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp, hhs6_tmp, hhs7_tmp, hhs8_tmp, hhs9_tmp))
  if (nrow(res) > 0) {
    res$ststcd <- x[[10]]
    res$year <- x[[11]]    
  }
  res
})
)


# x <- nad_sim[[1]]
system.time(nad_sim_ss <- lapply(nad_sim, function(x) {
  n <- nrow(x)
  nad <- sum(x$nad, na.rm = T)
  nch <- sum(x$nch, na.rm = T)
  res <- data.frame(n, nad, nch)
}))

system.time(nad_sim_ss <- Reduce(rbind.data.frame, nad_sim_ss))

nad_sim_ss$ststcd <- hhs_sim$ststcd
nad_sim_ss$year <- hhs_sim$year


plotdf <- inner_join(age_ss_wal, nad_sim_ss)


plotdf <- left_join(plotdf, bltupss[,1:2])


ggplot(plotdf, aes(x = hhs_20_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_kat()





ggplot(plotdf, aes(x = hhs_0_19, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()


ggplot(plotdf, aes(x = hhs_20_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()



ggplot(plotdf, aes(x = hhs_0_19, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()

plotdf$pop_sim <- plotdf$nad + plotdf$nch
plotdf$pop_obs <- plotdf$hhs_0_14 + plotdf$hhs_15_95

ggplot(plotdf, aes(x = pop_obs, y = pop_sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()

### trial 4 -------------

us <- left_join(us, bltupss[, c(1,4)])




p_hhs_rpa <- prop.table(table(us$hhs_tot, us$rpage), margin = 1)


#### family type given hhs and rpage 

p_hhs_rpa_blt_chd <- prop.table(table(us$hhs_tot, us$rpage_cat, us$LU2010_5cls_x25, us$hhs_0_14), margin = c(2,3,4))

test <- p_hhs_rpa_blt_chd[dimnames(p_hhs_rpa_blt_chd)[[1]] %in% 2, dimnames(p_hhs_rpa_blt_chd)[[2]] %in% "[45,50)", dimnames(p_hhs_rpa_blt_chd)[[3]] %in% 4,]

p_hhs_chd <- prop.table(table(ggs$hhs_tot, ggs$hhs_0_14), margin = 1)


nbn.f <-
  function(n = 100,
           hhsize = 2,
           pt2w1 = p_hhs_rpa,
           pt3w = p_hhs_rpa_chd,
           pt2w2 = p_hhs_chd) {
    
    if (!is.na(n) & n > 0) {
      df <- data.frame(hhs = rep(hhsize, n), rpage = rep(NA, n), nad = rep(NA,n))
      
      for (i in 1:nrow(df)) {
        
        p_2w1 <- pt2w1[dimnames(pt2w1)[[1]] %in% hhsize,]
        rpa_v <- rmultinom(1, 1, prob = p_2w1) 
        rpage <- dimnames(pt2w1)[[2]][rpa_v == 1]
        df$rpage[i] <- rpage
        
        p_3w <- pt3w[dimnames(pt3w)[[1]] %in% hhsize, dimnames(pt3w)[[2]] %in% rpage,]
        p_3w[is.na(p_3w)] <- 0
        
        p_2w2 <- pt2w2[dimnames(pt2w2)[[1]] %in% hhsize,]
        
        if (sum(p_3w, na.rm = T) == 1) {
          nch_v <- rmultinom(1, 1, p_3w)
          nch <- dimnames(pt3w)[[3]][which(nch_v %in% 1)]          
        } else {
          nch_v <- rmultinom(1, 1, p_2w2)
          nch <- dimnames(pt2w2)[[2]][which(nch_v %in% 1)] 
        }
        
        
        df$nch[i] <- as.numeric(nch)
        
        
      }
      
      df$nad <- df$hhs - df$nch
      
    } 
    
    df
  }


hhs_2_sim <- nbn.f(n = 10000, hhsize = 2)
hhs_3_sim <- nbn.f(n = 10000, hhsize = 3)
hhs_4_sim <- nbn.f(n = 10000, hhsize = 4)
hhs_5_sim <- nbn.f(n = 10000, hhsize = 5)
hhs_6_sim <- nbn.f(n = 10000, hhsize = 6)
hhs_7_sim <- nbn.f(n = 10000, hhsize = 7)
hhs_8_sim <- nbn.f(n = 10000, hhsize = 8)
hhs_9_sim <- nbn.f(n = 10000, hhsize = 9)


system.time(nad_sim <- apply(hhs_sim, 1, function(x) {
  hhs1_tmp <- data.frame(hhs = 1, rpage = NA, nad = 1, nch = 0)
  hhs1_tmp <- hhs1_tmp[rep(1, x[[1]]),]
  hhs2_tmp <- hhs_2_sim[sample(1:10000, size = ifelse(is.na(x[[2]]), 0, x[[2]])), ]
  hhs3_tmp <- hhs_3_sim[sample(1:10000, size = ifelse(is.na(x[[3]]), 0, x[[3]])), ]
  hhs4_tmp <- hhs_4_sim[sample(1:10000, size = ifelse(is.na(x[[4]]), 0, x[[4]])), ]
  hhs5_tmp <- hhs_5_sim[sample(1:10000, size = ifelse(is.na(x[[5]]), 0, x[[5]])), ]
  hhs6_tmp <- hhs_6_sim[sample(1:10000, size = ifelse(is.na(x[[6]]), 0, x[[6]])), ]
  hhs7_tmp <- hhs_7_sim[sample(1:10000, size = ifelse(is.na(x[[7]]), 0, x[[7]])), ]
  hhs8_tmp <- hhs_8_sim[sample(1:10000, size = ifelse(is.na(x[[8]]), 0, x[[8]])), ]
  hhs9_tmp <- hhs_9_sim[sample(1:10000, size = ifelse(is.na(x[[9]]), 0, x[[9]])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp, hhs6_tmp, hhs7_tmp, hhs8_tmp, hhs9_tmp))
  if (nrow(res) > 0) {
    res$ststcd <- x[[10]]
    res$year <- x[[11]]    
  }
  res
})
)


# x <- nad_sim[[1]]
system.time(nad_sim_ss <- lapply(nad_sim, function(x) {
  n <- nrow(x)
  nad <- sum(x$nad, na.rm = T)
  nch <- sum(x$nch, na.rm = T)
  res <- data.frame(n, nad, nch)
}))

system.time(nad_sim_ss <- Reduce(rbind.data.frame, nad_sim_ss))

nad_sim_ss$ststcd <- hhs_sim$ststcd
nad_sim_ss$year <- hhs_sim$year


plotdf <- inner_join(age_ss_wal, nad_sim_ss)


plotdf <- left_join(plotdf, bltupss[,1:2])


ggplot(plotdf, aes(x = hhs_15_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  theme_kat()





ggplot(plotdf, aes(x = hhs_0_14, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  +
  theme_kat()


ggplot(plotdf, aes(x = hhs_15_95, y = nad)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()



ggplot(plotdf, aes(x = hhs_0_14, y = nch)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()

plotdf$pop_sim <- plotdf$nad + plotdf$nch
plotdf$pop_obs <- plotdf$hhs_0_14 + plotdf$hhs_15_95

ggplot(plotdf, aes(x = pop_obs, y = pop_sim)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)  + 
  facet_wrap(.~ LU1990_5cls_x25) +
  theme_kat()


# 6. simulate for the future ---------
## 6.1. hhsize -------------


mncdls <- lvls(hhs_ss_lg$municd)
year <- seq(2020,2040, 5)
a <- mncdls[1]

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
    new_tmp$year <- rep(year, each = n)
    
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


ggplot(pred_11_19, aes(x = obs, y = pred, color = as.factor(year))) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red") + scale_color_manual(values = c(rep("black", 8), "red"))

train <- pred_11_19[pred_11_19$year < 2019,]
vald <- pred_11_19[pred_11_19$year %in% 2019,]
r2_train <- 1 - sum((train$pred - train$obs)^2, na.rm = T)/sum((mean(train$obs, na.rm = T) - train$obs)^2, na.rm = T)

r2_vald <- 1 - sum((vald$pred - vald$obs)^2, na.rm = T)/sum((mean(vald$obs, na.rm = T) - vald$obs)^2, na.rm = T)

pred_mun <- pred_20_40 %>%
  group_by(year, municd) %>%
  summarise(prj = sum(pred, na.rm = T),
            count = n())

plotdf <- inner_join(prjnhh_lg, pred_mun)
colnames(plotdf)

ggplot(plotdf, aes(x = prj_iweps, y = prj)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red")

plotdf$dif <- (plotdf$prj - plotdf$prj_iweps)*100/plotdf$prj_iweps

r2mun <- 1 - sum((plotdf$prj - plotdf$prj_iweps)^2, na.rm = T)/sum((mean(plotdf$prj_iweps, na.rm = T) - plotdf$prj_iweps)^2, na.rm = T)

bigdif <- plotdf[abs(plotdf$dif) > 10,]


## 6.2. simulate for big hh ----------

hhs_pred <- dcast(pred_20_40, municd + ststcd + year ~ hhsize , value.var = "pred")

bighh_prob <- bighh_ggs$prop_ggs

res <- data.frame(matrix(0, nrow = nrow(hhs_pred), ncol = 9))
colnames(res) <- paste("hhs", 1:9, sep = "_")
res$ststcd <- hhs_pred$ststcd
res$year <- hhs_pred$year

system.time(
  
  for (i in 1:nrow(hhs_pred)) {
    
    res[i, 1:4] <- sapply(hhs_pred[i, c("hhs_1", "hhs_2", "hhs_3", "hhs_4")], function(x) ifelse(is.na(x), 0, x))
    
    n_bhh <- hhs_pred[i, "hhs_5+"]
    
    if (!is.na(n_bhh) & n_bhh > 0) {
      bhh_v <- data.frame(rmultinom(n_bhh, 1, prob = bighh_prob))
      res[i, 5:9] <- apply(bhh_v, 1, sum)
    } else {
      res[i, 5:9] <- 0
    }
    
    

    
  }
  
)

hhs_sim_fut <- res



## 6.3. simulate nad and nch ----------------


system.time(nad_sim_fut <- apply(hhs_sim_fut, 1, function(x) {
  hhs1_tmp <- data.frame(hhs = 1, rpage = NA, nad = 1, nch = 0)
  hhs1_tmp <- hhs1_tmp[rep(1, x[[1]]),]
  hhs2_tmp <- hhs_2_sim[sample(1:10000, size = ifelse(is.na(x[[2]]), 0, x[[2]])), ]
  hhs3_tmp <- hhs_3_sim[sample(1:10000, size = ifelse(is.na(x[[3]]), 0, x[[3]])), ]
  hhs4_tmp <- hhs_4_sim[sample(1:10000, size = ifelse(is.na(x[[4]]), 0, x[[4]])), ]
  hhs5_tmp <- hhs_5_sim[sample(1:10000, size = ifelse(is.na(x[[5]]), 0, x[[5]])), ]
  hhs6_tmp <- hhs_6_sim[sample(1:10000, size = ifelse(is.na(x[[6]]), 0, x[[6]])), ]
  hhs7_tmp <- hhs_7_sim[sample(1:10000, size = ifelse(is.na(x[[7]]), 0, x[[7]])), ]
  hhs8_tmp <- hhs_8_sim[sample(1:10000, size = ifelse(is.na(x[[8]]), 0, x[[8]])), ]
  hhs9_tmp <- hhs_9_sim[sample(1:10000, size = ifelse(is.na(x[[9]]), 0, x[[9]])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp, hhs6_tmp, hhs7_tmp, hhs8_tmp, hhs9_tmp))
  if (nrow(res) > 0) {
    res$ststcd <- x[[10]]
    res$year <- x[[11]]    
  }
  res
})
)

## 6.4. compare population in 2020 -2035 ----------
# x <- nad_sim_fut[[1]]
system.time(test <- sapply(nad_sim_fut, function(x) {
  res <- sum(x$nad, na.rm = T) +  sum(x$nch, na.rm = T)
}))

test2 <- data.frame(pop_sim = test, ststcd = hhs_sim_fut$ststcd, year = hhs_sim_fut$year)

test2 <- left_join(test2,  st_drop_geometry(stst_wal[,c("ststcd", "municd")]))


colnames(test2)
test2_mun <- test2 %>%
  group_by(municd, year) %>%
  summarise(pop_sim = sum(pop_sim, na.rm = T))


plotdf <- inner_join(prjpop_lg, test2_mun)


plot(plotdf$pop_sim, plotdf$prj_iweps)
abline(a = 0, b = 1, col = "red")
summary(lm(pop_sim ~ prj_iweps, plotdf))


ggplot(plotdf, aes(x = prj_iweps, y = pop_sim)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + facet_grid(.~year)

dif <- (plotdf$pop_sim - plotdf$prj_iweps)*100/plotdf$prj_iweps

summary(dif)

plot(plotdf$nch, plotdf$hhs_0_14)
abline(a = 0, b = 1, col = "red")
summary(lm(nch ~ hhs_0_14, plotdf))


# old code ----------------


# 5. my naive way of bayesian network (need to clean and check for correctness) -----------

## 5.1. with us -------------

### fitting ----------

ct_hhs_rpa <- table(us$hhs_cat, us$rpage_cat)
ct_hhs_rpa_nad <- table(us$hhs_cat, us$rpage_cat, us$hhs_18_95)
ct_hhs_rpa_nad_nch <-
  table(us$hhs_cat, us$rpage_cat, us$hhs_18_95, us$hhs_0_17)
ct_hhs_rpa_nad_nch_rw <- table(us$hhs_cat, us$rpage_cat, us$hhs_18_95, us$hhs_0_17, us$rwtank)

### generating -------------

hhs1_nbn_us <- nbn.f(n = 10000, hhsize = "hhs_1")

hhs2_nbn_us <- nbn.f(n = 10000, hhsize = "hhs_2")

hhs3_nbn_us <- nbn.f(n = 10000, hhsize = "hhs_3")

hhs4_nbn_us <- nbn.f(n = 10000, hhsize = "hhs_4")

hhs5_nbn_us <- nbn.f(n = 10000, hhsize = "hhs_5+")

system.time(nbn_us_wal <- apply(macro_hhs[, 1:5], 1, function(x) {
  hhs1_tmp <- hhs1_nbn_us[sample(1:10000, size = ifelse(is.na(x[[1]]), 0, x[[1]])), ]
  hhs2_tmp <- hhs2_nbn_us[sample(1:10000, size = ifelse(is.na(x[[2]]), 0, x[[2]])), ]
  hhs3_tmp <- hhs3_nbn_us[sample(1:10000, size = ifelse(is.na(x[[3]]), 0, x[[3]])), ]
  hhs4_tmp <- hhs4_nbn_us[sample(1:10000, size = ifelse(is.na(x[[4]]), 0, x[[4]])), ]
  hhs5_tmp <- hhs5_nbn_us[sample(1:10000, size = ifelse(is.na(x[[5]]), 0, x[[5]])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp))
})
)

### validating ------------

system.time(nbn_us_sum_mat <- sapply(nbn_us_wal, function(x) {
  tmp <- as.numeric(x %>%
                      summarise(nhh = n(),
                                nad = sum(nad, na.rm = T),
                                nch = sum(nch, na.rm = T),
                                nrw = sum(rw == "yes")))
}))



nbn_us_sum_df <- data.frame(t(nbn_us_sum_mat))

colnames(nbn_us_sum_df) <- c("nhh", "nad", "nch", "nrw")



plotdf <- cbind.data.frame(macro_ss, nbn_us_sum_df)


plot(plotdf$nad, plotdf$hhs_20_95)
abline(a = 0, b = 1, col = "red")
summary(lm(nad ~ hhs_20_95, plotdf))

plot(plotdf$nch, plotdf$hhs_0_19)
abline(a = 0, b = 1, col = "red")
summary(lm(nch ~ hhs_0_19, plotdf))

rw_df <- plotdf %>%
  group_by(municd) %>% 
  summarise(nhh = sum(nhh, na.rm = T),
            nrw = sum(nrw, na.rm = T))

rw_df$prop <- rw_df$nrw * 100/rw_df$nhh


summary(rw_df)

rw_df <- left_join(rw_df, rwt_mun)

plot(rw_df$prop, rw_df$rwt_prop)
abline(a = 0, b = 1, col = "red")
summary(lm(prop ~ rwt_prop, rw_df))
cor.test(rw_df$prop, rw_df$rwt_prop)

### save output -----------
nbn_us <- mget(ls(pattern = "nbn_us"))

save(nbn_us, file = here(pdir, "pop_synth_Wal", "nbn_us.Rdata"))

## 5.2. with ggs -------------

### fitting ----------

ggs$hhs <- paste0("hhs_", ggs$hhs_tot)

ct_hhs_rpa <- table(ggs$hhs, ggs$rpage_cat)
ct_hhs_rpa_nad <- table(ggs$hhs, ggs$rpage_cat, ggs$hhs_15_95)
ct_hhs_rpa_nad_nch <-
  table(ggs$hhs, ggs$rpage_cat, ggs$hhs_15_95, ggs$hhs_0_14)
ct_hhs_rpa_nad_nch_rw <- table(us$hhs, us$rpage_cat, us$hhs_18_95, us$hhs_0_17, us$rwtank)


### generating -------------

hhs1_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_1")

hhs2_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_2")

hhs3_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_3")

hhs4_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_4")

hhs5_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_5")

hhs6_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_6")

hhs7_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_7")

hhs8_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_8")

hhs9_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_9")

system.time(nbn_ggs_wal <- apply(macro_hhs[, 1:6], 1, function(x) {
  tot <- sum(x, na.rm = T)
  hhs_prob <- x[1:5]/sum(x[1:5])
  hhs_prob <- c(hhs_prob[1:4], hhs_prob[5]*bighh_ggs$prop_ggs)
  
  size <- round(tot*hhs_prob)
  
  hhs1_tmp <- hhs1_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[1]), 0, size[1])), ]
  hhs2_tmp <- hhs2_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[2]), 0, size[2])), ]
  hhs3_tmp <- hhs3_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[3]), 0, size[3])), ]
  hhs4_tmp <- hhs4_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[4]), 0, size[4])), ]
  hhs5_tmp <- hhs5_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[5]), 0, size[5])), ]
  hhs6_tmp <- hhs6_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[6]), 0, size[6])), ]
  hhs7_tmp <- hhs7_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[7]), 0, size[7])), ]
  hhs8_tmp <- hhs8_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[8]), 0, size[8])), ]
  hhs9_tmp <- hhs9_nbn_ggs[sample(1:10000, size = ifelse(is.na(size[9]), 0, size[9])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp, hhs6_tmp, hhs7_tmp, hhs8_tmp, hhs9_tmp))
})
)

### validating ------------

system.time(nbn_ggs_sum_mat <- sapply(nbn_ggs_wal, function(x) {
  tmp <- as.numeric(x %>%
                      summarise(nhh = n(),
                                nad = sum(nad, na.rm = T),
                                nch = sum(nch, na.rm = T)))
}))



nbn_ggs_sum_df <- data.frame(t(nbn_ggs_sum_mat))

colnames(nbn_ggs_sum_df) <- c("nhh", "nad", "nch")



plotdf <- cbind.data.frame(macro_ss, nbn_ggs_sum_df)



plot(plotdf$nad, plotdf$hhs_19_95)
abline(a = 0, b = 1, col = "red")
summary(lm(nad ~ hhs_20_95, plotdf))

plot(plotdf$nch, plotdf$hhs_0_14)
abline(a = 0, b = 1, col = "red")
summary(lm(nch ~ hhs_0_19, plotdf))

rw_df <- plotdf %>%
  group_by(municd) %>% 
  summarise(nhh = sum(nhh, na.rm = T),
            nrw = sum(nrw, na.rm = T))

rw_df$prop <- rw_df$nrw * 100/rw_df$nhh


summary(rw_df)

rw_df <- left_join(rw_df, rwt_mun)

plot(rw_df$prop, rw_df$rwt_prop)
abline(a = 0, b = 1, col = "red")
summary(lm(prop ~ rwt_prop, rw_df))
cor.test(rw_df$prop, rw_df$rwt_prop)

### save output -----------
nbn_ggs <- mget(ls(pattern = "nbn_ggs"))

save(nbn_ggs, file = here(pdir, "pop_synth_Wal", "nbn_ggs.Rdata"))


## 5.3 try again sampling nad and nch at the same time and use also hhs_missing --------

### fitting --------

bighh_prob <- bighh_ggs$prop_ggs

ct_hhs_rpa <- table(ggs$hhs_tot, ggs$rpage_cat)
ggs$famtyp <- paste(ggs$hhs_15_95, ggs$hhs_0_14, sep = "_")

ct_hhs_rpa_fty <- table(ggs$hhs_tot, ggs$rpage_cat, ggs$famtyp)

### generating --------
hhs_prob_ggs <- prop.table(table(ggs$hhs_tot))

nbn2.f <-
  function(x, bighh_prob, ct2w = ct_hhs_rpa,
           ct3w = ct_hhs_rpa_fty) {
    
    n <- as.numeric(x[1])
    
    if (!is.na(n) & n > 0) {
      hhs <- as.numeric(x[2:6])
      smallhh_prob <- hhs*100/sum(hhs)
      hhs_prob <- unlist(c(smallhh_prob[1:4], smallhh_prob[5]*bighh_prob))
      names(hhs_prob) <- paste("hhs", 1:9, sep = "_")
      
      if (sum(hhs_prob, na.rm = T) == 1) {
        hhs_v <- data.frame(rmultinom(n, 1, prob = hhs_prob))
      } else {
        hhs_v <- data.frame(rmultinom(n, 1, prob = hhs_prob_ggs))
      }
      
      
      hhsize <- apply(hhs_v, 2, function(x) rownames(hhs_v)[x == 1])
      df <- data.frame(hhs = gsub("hhs_", "", hhsize))
      df$rpa_cat <- NA
      df$famtyp <- NA
      
      
      
      for (i in 1:nrow(df)) {
        
        p_2w <- prop.table(ct2w[dimnames(ct2w)[[1]] %in% df[i, "hhs"],])
        
        if (sum(p_2w, na.rm = T) == 1) {
          rpa_v <- rmultinom(1, 1, prob = p_2w)   
        } else {
          rpa_v <- rmultinom(1, 1, prob = rep(1/length(p_2w), length(p_2w))) 
        }
        
        rpa_cat <- dimnames(ct2w)[[2]][rpa_v == 1]
        df$rpa_cat[i] <- rpa_cat
        
        p_3w <-
          prop.table(ct3w[dimnames(ct3w)[[1]] %in% df[i, "hhs"], dimnames(ct3w)[[2]] %in% rpa_cat,])
        
        if (sum(p_3w, na.rm = T) == 1) {
          famtyp_v <- rmultinom(1, 1, p_3w)
        } else {
          famtyp_v <- rmultinom(1, 1, prob = rep(1/length(p_3w), length(p_3w))) 
        }
        
        famtyp <- dimnames(ct3w)[[3]][famtyp_v == 1]
        df$famtyp[i] <- famtyp
        
      }
      
      df$nad <- as.numeric(gsub("_.*", "", df$famtyp))
      df$nch <- as.numeric(gsub(".*_", "", df$famtyp))
      df$ststcd <- x[7]
      df$year <- as.numeric(x[8])      
      
    } 
    
    
    df
  }

system.time(nbn2_syn <- apply(hhs_ss_wal[1:100, c("nbhh_prv", "hhs_1", "hhs_2", "hhs_3", "hhs_4", "hhs_5+", "ststcd", "year")], 1, nbn2.f, bighh_prob = bighh_prob)
)

### validate -----------
n <- length(nbn2_syn)

res <- data.frame(ststcd = rep(NA, n), year = rep(NA, n), nad = rep(NA, n), nch = rep(NA, n))

system.time(for (i in 1:n) {
  x <- nbn2_syn[[i]]
  if (length(x) > 1) {
    res[i, "ststcd"] <- unique(x$ststcd)
    res[i, "year"] <- unique(x$year)
    res[i, "nad"] <- sum(x$nad, na.rm = T)
    res[i, "nch"] <- sum(x$nch, na.rm = T)
  }
})

res <- res[complete.cases(res),]


age_sub <- age_ss_wal[, c(grep("age_", colnames(age_ss_wal), value = T), "ststcd", "year")]

age_sub$nch_obs <- apply(age_sub[,1:3], 1,sum)
age_sub$nad_obs <- apply(age_sub[,4:17], 1,sum)


final <- inner_join(res, age_sub)


ggplot(final, aes(x = nad, y = nad_obs)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")


ggplot(final, aes(x = nch, y = nch_obs)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")


summary(lm(nch ~ nch_obs, data = final))
summary(lm(nad ~ nad_obs, data = final))
  
  smallhh_prob <- hhs_count/sum(hhs_count)
  
  hhs_prob <- c(smallhh_prob[1:4], smallhh_prob[5]*bighh_prob)
  names(hhs_prob) <- 1:9
  
  if (!is.na(nbhh_prv)) {
    for (i in seq_len(nbhh_prv)) {
      hhs <- names(hhs_prob[rmultinom(1, 1, prob = hhs_prob) == 1])
      rpa_prob <- p_rpage_hhs[dimnames(p_rpage_hhs)[[1]] %in% hhs]
    }
  }
}







test <- data.frame()

syn <- apply(test, 2, function(x) rownames(test)[x==1])

hhs_syn <- data.frame(table(syn))
sum(hhs_count)
nbhh_prv
hhs_val <- data.frame(hhs = names(hhs_count), hhs_count)
hhs_val$hhs_syn <- c(hhs_syn$Freq[1:4], sum(hhs_syn$Freq[5:9]))

hhs_val <- melt(hhs_val, id.vars = "hhs")


ggplot(hhs_val, aes(x = hhs, y = value, color = variable)) + geom_line(aes(group = variable)) +
  theme_kat() +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)

cor.test(hhs_val$value[hhs_val$variable == "hhs_count"], hhs_val$value[hhs_val$variable == "hhs_syn"] )
hhs_count

test_mlt <- melt(test)
summary(test_mlt$Var1)
summary(test_mlt$Var2)








# 7. Farooq 2013 Simulation  ---------------------


### first attempt -----------

# us$hhs <- paste0("hhs_", ifelse(us$hhstt < 5, as.character(us$hhstt), "5+"))
#
# cd_rage_hhs <- us %>%
#   group_by(hhs) %>%
#   summarise(mean = mean(rprage, na.rm = T),
#             sd = sd(rprage, na.rm = T))
#
# ggplot(us, aes(x = hhs, y = rprage)) +
#   geom_violin() +
#   geom_crossbar(stat="summary", fun=mean, fun.max=mean, fun.min=mean, fatten=2, width=.5) +
#   geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
#   theme_minimal() +
#   labs(y = "household head age", x = "household size")
#
# newdf <- us[, c("id", "rprage", "hhs", "hhs_20_95", "hhstt", "hhs_0_19")]
# newdf <- newdf[order(newdf$hhs, newdf$id), ]
#
# newdf$n_othadlt <- newdf$hhs_20_95 - 1
#
# newdf$rprage_cat <- cut(newdf$rprage, breaks = c(seq(15, 80, 5), 150), include.lowest = T, right = F)
#
# fitoa1 <- glm(n_othadlt ~ rprage  + hhs, newdf, family = "poisson")
#
# fitoa2 <- glm(n_othadlt ~ rprage + I(rprage^3) + hhs, newdf, family = "poisson")
#
# fitoa3 <- glm(n_othadlt ~ rprage + I(rprage^2) + I(rprage^3) + hhs, newdf, family = "poisson")
#
# fitoa4 <- glm(n_othadlt ~ rprage_cat  + hhs, newdf, family = "poisson")
#
# summary(fitoa4)
#
# fitoa5 <- glm.nb(n_othadlt ~ rprage_cat  + hhs, data = newdf)
#
# summary(fitoa5)
#
#
# poa1 <- predict(fitoa1, type = "response")
# poa2 <- predict(fitoa2, type = "response")
# poa3 <- predict(fitoa3, type = "response")
# poa4 <- predict(fitoa4, type = "response")
# poa5 <- predict(fitoa5, type = "response")
#
# RMSE1 <- sqrt(mean((poa1 - newdf$n_othadlt)^2, na.rm = T))
# RMSE2 <- sqrt(mean((poa2 - newdf$n_othadlt)^2, na.rm = T))
# RMSE3 <- sqrt(mean((poa3 - newdf$n_othadlt)^2, na.rm = T))
# RMSE4 <- sqrt(mean((poa4 - newdf$n_othadlt)^2, na.rm = T))
# RMSE5 <- sqrt(mean((poa5 - newdf$n_othadlt)^2, na.rm = T))
#
#
#
# hhs_bres$prob <- prop.table(hhs_bres$nhhs)
#
#
# hhs_bres$hhs_num <- 1:5
#
# hhs_bres$hhs <- as.character(hhs_bres$hhs)
# hhs_bres <- left_join(hhs_bres, cd_rage_hhs)
#
#
#
# synpop <- data.frame(hhs = rep(NA, sum(hhs_bres$nhhs)))
# synpop$rprage <- NA
# synpop$rprage_cat <- NA
# synpop$n_othadlt <- NA
#
#
# for (i in 1:nrow(synpop)) {
#   hhs <- which(rmultinom(1, 1, prob = hhs_bres$prob) == 1)
#   synpop$hhs[i] <- hhs_bres$hhs[hhs_bres$hhs_num %in% hhs]
#   rprage <- rnorm(1, mean =  hhs_bres$mean[hhs_bres$hhs_num %in% hhs], sd =  hhs_bres$sd[hhs_bres$hhs_num %in% hhs])
#   while (rprage < 20) {
#     rprage <-  rnorm(1, mean =  hhs_bres$mean[hhs_bres$hhs_num %in% hhs], sd =  hhs_bres$sd[hhs_bres$hhs_num %in% hhs])
#   }
#   synpop$rprage[i] <- rprage
#   synpop$rprage_cat <- cut(synpop$rprage, breaks = c(seq(15, 80, 5), 150), include.lowest = T, right = F)
#   synpop$n_othadlt[i] <- rpois(1, predict(fitoa4, synpop[i,], type = "response"))
#
# }
#
#
#
#
#
# ggplot(synpop, aes(x = hhs, y = rprage)) +
#   geom_violin() +
#   geom_crossbar(stat="summary", fun=mean, fun.max=mean, fun.min=mean, fatten=2, width=.5) +
#   geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
#   theme_minimal() +
#   labs(y = "household head age", x = "household size")
#
#
#
# table(us$hhs)
#
# fitrprage <- lm(rprage ~ hhs, us)
# summary(fitrprage)
# sum(hhs_bres$value)
#
#
#
# barplot(x = hhs_bres$variable, y = rmultinom(1, 2364, prob = hhs_bres$prop))


## 7.1. Discrete choice models ---------------


## 7.2. Simulation --------------









# 8. IPFs  ----------------------

## 8.1. fitting ----------
## 8.2. generation --------------




# testing ----------

## fitting ordinal logistics to have hhsize distribution for municiaplities in the future -----------

test <- macro_mun[, c("municd", "year", "nbhh_prv", grep("hhs", colnames(macro_mun), value = T))]

test <- test[, 1:9]

test <- melt(test, id.vars = c("municd", "year", "nbhh_prv"))


library(lme4)
library(lmerTest)

fit1 <- glmer(value ~nbhh_prv + variable + year + variable*year + (1 + variable|municd), test, family =  poisson(link = log))


summary(fit1)


fit2 <- glmer(value ~ variable + year  + (1 + variable|municd), test, family =  poisson(link = log))


ggplot(test, aes(x = year, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  facet_wrap(. ~ municd, scales = "free")

repind <- rep(1:nrow(test), test$value)

test2 <- test[repind, c("municd", "year", "variable")]

head(test2)

table(repind)

object.size(test2)

## don't remember -----

x <-
  hhs_ss_wal[1, c("ststcd", "year", "hhs_1", "hhs_2", "hhs_3", "hhs_4", "hhs_5+")]
test <-
  apply(hhs_ss_wal[, c("ststcd", "year", "hhs_1", "hhs_2", "hhs_3", "hhs_4", "hhs_5+")], 1, function(x) {
    hhs1df <- synpop.f(n = x[, "hhs_1"], hhsize = "hhs_1")
  })
hhs_ss_wal$`hhs_5+`
test <- lapply

summary(hhs_ss_wal$hhs_1)
ststcd <- unique(hhs_ss_wal$ststcd)
year <- unique(hhs_ss_wal$year)
colnames(hhs_ss_wal)
test <- synpop.f()


test <- lapply(hhs_ss_wal$hhs_1, synpop.f, hhsize = "hhs_1")

write.table(age_ss_wal[1:10, c("ststcd", "year", grep("age", colnames(age_ss_wal), value = T))]
            ,
            "clipboard",
            row.names = T,
            sep = "\t")

age_ss_wal$age_0_4
