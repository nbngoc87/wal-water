#' ---
#' title: "Population synthesis"
#' author: "Nguyen Bich Ngoc, Ismail Saadi, Jacques Teller"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---

#+ r setup, include = F, message = F
# notes from last run----------------------

# rmarkdown::render("scripts/pop_synth/pop_synth.R",output_file=paste0("pop_synth_", format(Sys.time(), "%y%m%d_%H%M"),".md"))

# 1. set up -------------------

## 1.1. load functions -------------

### new functions ------------


loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

# my naive way of applying Bayesian network

nbn.f <-
  function(n = 100,
           hhsize = "hhs_3",
           ct2w = ct_hhs_rpa,
           ct3w = ct_hhs_rpa_nad ,
           ct4w = ct_hhs_rpa_nad_nch,
           ct5w = ct_hhs_rpa_nad_nch_rw) {
    
    if (!is.na(n) & n > 0) {
      df <- data.frame(hhs = rep(hhsize, n))
      
      for (i in 1:nrow(df)) {
        
        p_2w <- prop.table(ct2w[dimnames(ct2w)[[1]] %in% hhsize,])
        
        if (sum(p_2w, na.rm = T) == 1) {
          rpa_v <- rmultinom(1, 1, prob = p_2w)   
        } else {
          rpa_v <- rmultinom(1, 1, prob = rep(1/length(p_2w), length(p_2w))) 
        }
        
        rpa_cat <- dimnames(ct2w)[[2]][rpa_v == 1]
        df$rpa_cat[i] <- rpa_cat
        
        p_3w <-
          prop.table(ct3w[dimnames(ct3w)[[1]] %in% hhsize, dimnames(ct3w)[[2]] %in% rpa_cat,])
        
        if (sum(p_3w, na.rm = T) == 1) {
          nad_v <- rmultinom(1, 1, p_3w)
        } else {
          nad_v <- rmultinom(1, 1, prob = rep(1/length(p_3w), length(p_3w))) 
        }
        
        nad <- dimnames(ct3w)[[3]][nad_v == 1]
        df$nad[i] <- as.numeric(nad)
        
        p_4w <-
          prop.table(ct4w[dimnames(ct4w)[[1]] %in% hhsize, dimnames(ct4w)[[2]] %in% rpa_cat, dimnames(ct4w)[[3]] %in% nad,])

        if (sum(p_4w, na.rm = T) == 1) {        
          nch_v <- rmultinom(1, 1, p_4w)
        } else {
          nch_v <- rmultinom(1, 1, prob = rep(1/length(p_4w), length(p_4w)))
        }
        
        nch <- dimnames(ct4w)[[4]][nch_v == 1]
        df$nch[i] <- as.numeric(nch)
        
        p_5w <- 
          prop.table(ct5w[dimnames(ct5w)[[1]] %in% hhsize, dimnames(ct5w)[[2]] %in% rpa_cat, dimnames(ct5w)[[3]] %in% min(nad, dim(ct5w)[3]), dimnames(ct5w)[[4]] %in% min(nch, dim(ct5w)[4] - 1),])
        
        if (sum(p_5w, na.rm = T) == 1) { 
          rw_v <- rmultinom(1, 1, p_5w) 
        } else {
          rw_v <- rmultinom(1, 1, prob = rep(1/length(p_5w), length(p_5w)))
        }
        
        rw <- dimnames(ct5w)[[5]][rw_v == 1]
        df$rw[i] <- rw
        
      }
      
    } else {
      
      df <-
        data.frame(
          hhs = character(),
          rpage_cat = character(),
          nad = numeric(),
          nch = numeric(),
          rw = character()
        )
    }
    
    df
  }


### packages ----------


loadpackage("here")
loadpackage("reshape2")
loadpackage("ggplot2")
loadpackage("dplyr")
loadpackage("MASS")
loadpackage("scico")
loadpackage("tidyr")

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


### statistical sectors ------------

load(file = here(pdir, "sode_ss_Be", "sode_ss_Wal.Rdata"))




### rainwater tank ------------

rwt_mun <- read.csv(here(pdir, "raintank_muni_Be", "raintank_muni_Be.csv"))


knitr::knit_exit()

#+ notcorrectedyet

# 3. process data------------------

## 3.1. utility survey -----------


us <- us[!is.na(us$hhs_20_95) & us$hhs_20_95 > 0 & !is.na(us$rwtank),]

# table(us$hhs_tot)
#
# prop.table(table(us$hhs_20_95))
#
# table(us$hhs_0_19)

us$hhs_cat <- paste0("hhs_", ifelse(us$hhs_tot < 5, as.character(us$hhs_tot), "5+"))

us$rpage_cat <- cut(us$rpage, breaks = c(seq(15, 80, 5), 150), include.lowest = T, right = F)




## 3.2. ggs ------------

ggs <- ggs_wal[, c("arid", "ahhsize", grep("ahg", colnames(ggs_wal), value = T))]

colnames(ggs)[1:2] <- c("id", "hhs_tot")



ggs_age <- ggs[, grep("ahg5_", colnames(ggs))]

ggs$hhs_20_95 <-
  apply(ggs_age, 1, function(x)
    sum(x >= 20, na.rm = T))

ggs$hhs_0_19 <-  apply(ggs_age, 1, function(x)
  sum(x < 20, na.rm = T))

ggs$hhs_cat <- paste0("hhs_", ifelse(ggs$hhs_tot < 5, as.character(ggs$hhs_tot), "5+"))

ggs$rpage_cat <- cut(ggs$ahg5_1, breaks = c(seq(15, 80, 5), 150), include.lowest = T, right = F)

test <- ggs[ggs$ahg5_1 < 20, ]

## 3.3.  statistical sectors -----------------



macro_ss <- inner_join(hhs_ss_wal[!is.na(hhs_ss_wal$year),], age_ss_wal[!is.na(age_ss_wal$year),])

macro_age <- macro_ss[, grep("age", colnames(macro_ss), value = T)]

macro_ss$hhs_0_19 <- apply(macro_age[,1:4], 1, sum, na.rm = T)
macro_ss$hhs_20_95 <- apply(macro_age[,5:17], 1, sum, na.rm = T)

macro_mun <- macro_ss[, c(5, 11:49)] %>%
  group_by(municd, year) %>%
  summarise_all(sum, na.rm = T)




## 3.4. rainwater tank ----------

# 4. compare marginal between data ------

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



macro_age_14 <- apply(macro_age[macro_ss$year %in% 2014,],  2,sum, na.rm = T)

plotdf$freq_macro <- macro_age_14/sum(macro_age_14)


ggplot(plotdf, aes(x = age_cat)) +
  geom_line(aes(y = freq_us, col = "us", group = 1), size = 1)  +
  geom_line(aes(y = freq_ggs, col = "ggs", group = 1), size = 1) +
  geom_line(aes(y = freq_macro, col = "macro", group = 1), size = 1) +
  theme_kat()  +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)

cor.test(plotdf$freq_ggs, plotdf$freq_macro)
cor.test(plotdf$freq_us, plotdf$freq_macro)

## 4.3. hhsize -------------

plotdf <- data.frame(prop.table(table(us$hhs_cat)))
colnames(plotdf) <- c("hhs_cat", "freq_us")

plotdf$freq_ggs <- prop.table(table(ggs$hhs_cat))

macro_hhs <- macro_ss[, grep("hhs_", colnames(macro_ss))]

macro_hhs_14 <- apply(macro_hhs[macro_ss$year %in% 2014, 1:5], 2, sum, na.rm = T)


plotdf$freq_macro <- macro_hhs_14/sum(macro_hhs_14)

ggplot(plotdf, aes(x = hhs_cat)) +
  geom_line(aes(y = freq_us, col = "us", group = 1), size = 1)  +
  geom_line(aes(y = freq_ggs, col = "ggs", group = 1), size = 1) +
  geom_line(aes(y = freq_macro, col = "macro", group = 1), size = 1) +
  theme_kat()  +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)

cor.test(plotdf$freq_ggs, plotdf$freq_macro)
cor.test(plotdf$freq_us, plotdf$freq_macro)


bighh_us <- us[us$hhs_tot > 4,] %>%
  group_by(hhs_tot) %>%
  summarise(count_us = n())

bighh_us$prop_us <- bighh_us$count_us/sum(bighh_us$count_us)


bighh_ggs <- ggs[ggs$hhs_tot > 4,] %>%
  group_by(hhs_tot) %>%
  summarise(count_ggs = n())

bighh_ggs$prop_ggs <- bighh_ggs$count_ggs/sum(bighh_ggs$count_ggs)

plotdf <- inner_join(bighh_ggs, bighh_us)

cor.test(plotdf$prop_ggs, plotdf$prop_us)

ggplot(plotdf, aes(x = hhs_tot))  +
  geom_line(aes(y = prop_us, col = "us", group = 1), size = 1)  +
  geom_line(aes(y = prop_ggs, col = "ggs", group = 1), size = 1) +
  theme_kat()  +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end)

  
# 5. my naive way of bayesian network (need to clean and check for correctness) -----------

## 5.1. with us -------------

### fitting ----------

ct_hhs_rpa <- table(us$hhs_cat, us$rpage_cat)
ct_hhs_rpa_nad <- table(us$hhs_cat, us$rpage_cat, us$hhs_20_95)
ct_hhs_rpa_nad_nch <-
  table(us$hhs_cat, us$rpage_cat, us$hhs_20_95, us$hhs_0_19)
ct_hhs_rpa_nad_nch_rw <- table(us$hhs_cat, us$rpage_cat, us$hhs_20_95, us$hhs_0_19, us$rwtank)

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

ct_hhs_rpa <- table(ggs$hhs_cat, ggs$rpage_cat)
ct_hhs_rpa_nad <- table(ggs$hhs_cat, ggs$rpage_cat, ggs$hhs_20_95)
ct_hhs_rpa_nad_nch <-
  table(ggs$hhs_cat, ggs$rpage_cat, ggs$hhs_20_95, ggs$hhs_0_19)
ct_hhs_rpa_nad_nch_rw <- table(us$hhs_cat, us$rpage_cat, us$hhs_20_95, us$hhs_0_19, us$rwtank)

### generating -------------

hhs1_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_1")

hhs2_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_2")

hhs3_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_3")

hhs4_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_4")

hhs5_nbn_ggs <- nbn.f(n = 10000, hhsize = "hhs_5+")

system.time(nbn_ggs_wal <- apply(macro_hhs[, 1:5], 1, function(x) {
  hhs1_tmp <- hhs1_nbn_ggs[sample(1:10000, size = ifelse(is.na(x[[1]]), 0, x[[1]])), ]
  hhs2_tmp <- hhs2_nbn_ggs[sample(1:10000, size = ifelse(is.na(x[[2]]), 0, x[[2]])), ]
  hhs3_tmp <- hhs3_nbn_ggs[sample(1:10000, size = ifelse(is.na(x[[3]]), 0, x[[3]])), ]
  hhs4_tmp <- hhs4_nbn_ggs[sample(1:10000, size = ifelse(is.na(x[[4]]), 0, x[[4]])), ]
  hhs5_tmp <- hhs5_nbn_ggs[sample(1:10000, size = ifelse(is.na(x[[5]]), 0, x[[5]])), ]
  res <- Reduce(rbind.data.frame, list(hhs1_tmp, hhs2_tmp, hhs3_tmp, hhs4_tmp, hhs5_tmp))
})
)

### validating ------------

system.time(nbn_ggs_sum_mat <- sapply(nbn_ggs_wal, function(x) {
  tmp <- as.numeric(x %>%
                      summarise(nhh = n(),
                                nad = sum(nad, na.rm = T),
                                nch = sum(nch, na.rm = T),
                                nrw = sum(rw == "yes")))
}))



nbn_ggs_sum_df <- data.frame(t(nbn_ggs_sum_mat))

colnames(nbn_ggs_sum_df) <- c("nhh", "nad", "nch", "nrw")



plotdf <- cbind.data.frame(macro_ss, nbn_ggs_sum_df)



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
nbn_ggs <- mget(ls(pattern = "nbn_ggs"))

save(nbn_ggs, file = here(pdir, "pop_synth_Wal", "nbn_ggs.Rdata"))

# 6. Mistry 2018 -------------

# test with ststcd 62063E00-  

## 6.1. fitting ---------


# count from macro data for hhs below 5 at stst level or municipality level????
# problem with stst level is missing data, problem with mun level is aggregation and missing information???, and also missing data at stst still result in lower total number of households 
# count from ggs for hhs from 5-9
# use nbhh_prv as the targeted number of synthesised families
# use proportion of hhs_1, ..., hhs_5+ as probability for sampling then I don't have to worry about hhs_NA since it was included in total hh already
# treating NA as 0?????????????????????????
# => do with muncipality level 
# => do with STST with NA = 0
# => compare????

### using macro data at mun level ----------
#### P(rpage given hhs) --------------
# age cat of each year or each 5 year??????

p_hhs_rpa <- prop.table(table(ggs$hhs_cat, ggs$rpage_cat), margin = 1)


p_nad_hhs_rpa <- prop.table(table(ggs$hhs_20_95, ggs$hhs_cat, ggs$rpage_cat), margin = c(2,3))



p_nad_hhs_rpa[,,1]

#### general macro info -----

nbhh_prv <- macro_mun$nbhh_prv[1]

hhs_count <- unlist(macro_mun[1, c("hhs_1", "hhs_2", "hhs_3", "hhs_4", "hhs_5+")])

municd <- macro_mun$municd[1]
year <- macro_mun$year[1]




## start function here 

smallhh_prob <- hhs_count/sum(hhs_count)

hhs_prob <- c(smallhh_prob[1:4], smallhh_prob[5]*bighh_prob)
names(hhs_prob) <- 1:9

if (!is.na(nbhh_prv)) {
  for (i in seq_len(nbhh_prv)) {
    hhs <- names(hhs_prob[rmultinom(1, 1, prob = hhs_prob) == 1])
    rpa_prob <- p_rpage_hhs[dimnames(p_rpage_hhs)[[1]] %in% hhs]
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


## 6.2. generating -------------





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
