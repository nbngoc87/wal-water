#' ---
#' title: "Redistribution effects of water tariffs"
#' author: "Nguyen Bich Ngoc, Jacques Teller"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---




#+ r setup, include = F, message = F
# notes from last run----------------------

# rmarkdown::render("scripts/redistribution/redistribution.R",output_file=paste0("redistribution_", format(Sys.time(), "%y%m%d_%H%M"),".md"))

# 1. setup ---------





## 1.1. load functions -----------------
### new functions ------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}


fixed_f <-
  function(dtbtorname = "SWDE",
           fixeds = seq (0, 200, 50)) {
    tmpdf <- df[df$dtbtor %in% dtbtorname,]
    
    bilwot_tot <- sum(tmpdf$bill) / 1.06
    avrprc <- sum(tmpdf$bill) / sum(tmpdf$csmptv)
    fsa <- sum(0.0125 * tmpdf$csmptv)
    cvd_v <- numeric()
    
    for (fixed in fixeds) {
      cvd <-
        (bilwot_tot  - fixed * nrow(tmpdf) - fsa - sum((tmpdf$csmptv - 30) * tmpdf$CVA *
                                                         tmpdf$ab30)) / sum(tmpdf$csmptv * 0.5 + (tmpdf$csmptv - 30) * 0.5 * tmpdf$ab30)
      cvd_v <- c(cvd_v, cvd)
      tmpdf$bill_new <-
        1.06 * (
          fixed  + tmpdf$csmptv * (0.0125 + 0.5 * cvd) + (tmpdf$csmptv - 30) * (0.5 *
                                                                                  cvd + tmpdf$CVA) * tmpdf$ab30
        )
      tmpdf$difab <- tmpdf$bill_new - tmpdf$bill
      tmpdf$difpc <- tmpdf$difab * 100 / tmpdf$bill
      tmpdf$difpcinc <- tmpdf$difab * 100 / (tmpdf$income * 12)
      tmpdf$TEH_new <- tmpdf$bill_new * 100 / (tmpdf$income * 12)
      tmpdf$subs_new <- tmpdf$csmptv * avrprc - tmpdf$bill_new
      tmpdf$avrprc_new <- tmpdf$bill_new / tmpdf$csmptv
      colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <-
        paste(
          c(
            "bill",
            "difab",
            "difpc",
            "difpcinc",
            "TEH",
            "subs",
            "avrprc"
          ),
          "fixed",
          fixed,
          sep = "_"
        )
    }
    
    tmpdf <-
      tmpdf[, c("id", grep("_fixed_", colnames(tmpdf), value = T))]
    
    list(tmpdf, cvd_v)
  }



rwtt_f <- function(dtbtorname = "SWDE",
                   rwtts = seq (0, 200, 50)) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  
  bilwot_tot <- sum(tmpdf$bill) / 1.06
  avrprc <- sum(tmpdf$bill) / sum(tmpdf$csmptv)
  fsa <- sum(0.0125 * tmpdf$csmptv)
  cvd_v <- numeric()
  
  for (rwtt in rwtts) {
    rwtt_tot <- sum(rwtt * tmpdf$rwt_num)
    
    cvd <-
      (bilwot_tot - rwtt_tot - fsa - sum(30 * tmpdf$CVA + (tmpdf$csmptv - 30) *
                                           tmpdf$CVA * tmpdf$ab30)) / sum(20 + tmpdf$csmptv * 0.5 + (tmpdf$csmptv - 30) *
                                                                            0.5 * tmpdf$ab30)
    cvd_v <- c(cvd_v, cvd)
    tmpdf$bill_new <-
      1.06 * (
        rwtt * tmpdf$rwt_num + (30 * tmpdf$CVA + 20 * cvd) + tmpdf$csmptv * (0.0125 + 0.5 *
                                                                               cvd) + (tmpdf$csmptv - 30) * (0.5 * cvd + tmpdf$CVA) * tmpdf$ab30
      )
    tmpdf$difab <- tmpdf$bill_new - tmpdf$bill
    tmpdf$difpc <- tmpdf$difab * 100 / tmpdf$bill
    tmpdf$difpcinc <- tmpdf$difab * 100 / (tmpdf$income * 12)
    tmpdf$TEH_new <- tmpdf$bill_new * 100 / (tmpdf$income * 12)
    tmpdf$subs_new <- tmpdf$csmptv * avrprc - tmpdf$bill_new
    tmpdf$avrprc_new <- tmpdf$bill_new / tmpdf$csmptv
    colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <-
      paste(
        c(
          "bill",
          "difab",
          "difpc",
          "difpcinc",
          "TEH",
          "subs",
          "avrprc"
        ),
        "rwtt",
        rwtt,
        sep = "_"
      )
    
  }
  
  tmpdf <-
    tmpdf[, c("id", grep("_rwtt_", colnames(tmpdf), value = T))]
  list(tmpdf, cvd_v)
}

up_f <-
  function(dtbtorname = "SWDE",
           fixeds = seq (0, 100, 50),
           revincr = 0) {
    tmpdf <- df[df$dtbtor %in% dtbtorname,]
    
    bill_tot <- (1 + revincr) * sum(tmpdf$bill)
    bilwot_tot <- bill_tot / 1.06
    fsa <- sum(0.0125 * tmpdf$csmptv)
    cons_tot <- sum(tmpdf$csmptv)
    avrprc <- bill_tot / cons_tot
    up_v <- numeric()
    
    for (fixed in fixeds) {
      fixed_tot <- fixed * nrow(tmpdf)
      
      up <- (bilwot_tot - fixed_tot - fsa) / cons_tot
      up_v <- c(up_v, up)
      tmpdf$bill_new <-
        1.06 * (fixed + tmpdf$csmptv * (up + 0.0125))
      tmpdf$difab <- tmpdf$bill_new - tmpdf$bill
      tmpdf$difpc <- tmpdf$difab * 100 / tmpdf$bill
      tmpdf$difpcinc <- tmpdf$difab * 100 / (tmpdf$income * 12)
      tmpdf$TEH_new <- tmpdf$bill_new * 100 / (tmpdf$income * 12)
      tmpdf$subs_new <- tmpdf$csmptv * avrprc - tmpdf$bill_new
      tmpdf$avrprc_new <- tmpdf$bill_new / tmpdf$csmptv
      colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <-
        paste(c(
          "bill",
          "difab",
          "difpc",
          "difpcinc",
          "TEH",
          "subs",
          "avrprc"
        ),
        "up",
        fixed,
        sep = "_")
      
    }
    tmpdf <-
      tmpdf[, c("id", grep("_up_", colnames(tmpdf), value = T))]
    list(tmpdf, up_v)
  }

ibtcon_f <-
  function(dtbtorname = "SWDE",
           fixeds = seq (0, 100, 50),
           bl1size = 30,
           blratio = 3.5,
           revincr = 0) {
    tmpdf <- df[df$dtbtor %in% dtbtorname,]
    tmpdf$bl2 <- as.numeric(tmpdf$csmptv > bl1size)
    
    bill_tot <- (1 + revincr) * sum(tmpdf$bill)
    bilwot_tot <- bill_tot / 1.06
    fsa <- sum(0.0125 * tmpdf$csmptv)
    cons_tot <- sum(tmpdf$csmptv)
    avrprc <- bill_tot / cons_tot
    bl1_v <- numeric()
    
    for (fixed in fixeds) {
      fixed_tot <- fixed * nrow(tmpdf)
      
      bl1 <-
        (bilwot_tot - fixed_tot - fsa) / sum(tmpdf$csmptv + (tmpdf$csmptv - bl1size) *
                                               (blratio - 1) * tmpdf$bl2)
      bl1_v <- c(bl1_v, bl1)
      tmpdf$bill_new <-
        1.06 * (
          fixed + tmpdf$csmptv * (bl1 + 0.0125) + (tmpdf$csmptv - bl1size) * (blratio - 1) *
            bl1 * tmpdf$bl2
        )
      tmpdf$difab <- tmpdf$bill_new - tmpdf$bill
      tmpdf$difpc <- tmpdf$difab * 100 / tmpdf$bill
      tmpdf$difpcinc <- tmpdf$difab * 100 / (tmpdf$income * 12)
      tmpdf$TEH_new <- tmpdf$bill_new * 100 / (tmpdf$income * 12)
      tmpdf$subs_new <- tmpdf$csmptv * avrprc - tmpdf$bill_new
      tmpdf$avrprc_new <- tmpdf$bill_new / tmpdf$csmptv
      colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <-
        paste(
          c(
            "bill",
            "difab",
            "difpc",
            "difpcinc",
            "TEH",
            "subs",
            "avrprc"
          ),
          "ibtcon",
          fixed,
          sep = "_"
        )
      
    }
    tmpdf <-
      tmpdf[, c("id", grep("_ibtcon_", colnames(tmpdf), value = T))]
    list(tmpdf, bl1_v)
  }

ibtcap_f <-
  function(dtbtorname = "SWDE",
           fixeds = seq (0, 100, 50),
           bl1size = 12.5,
           blratio = 3.5,
           revincr = 0) {
    tmpdf <- df[df$dtbtor %in% dtbtorname,]
    tmpdf$bl2 <- as.numeric(tmpdf$cspp > bl1size)
    
    bill_tot <- (1 + revincr) * sum(tmpdf$bill)
    bilwot_tot <- bill_tot / 1.06
    fsa <- sum(0.0125 * tmpdf$csmptv)
    cons_tot <- sum(tmpdf$csmptv)
    avrprc <- bill_tot / cons_tot
    bl1_v <- numeric()
    
    for (fixed in fixeds) {
      fixed_tot <- fixed * nrow(tmpdf)
      
      bl1 <-
        (bilwot_tot - fixed_tot - fsa) / sum(
          tmpdf$cspp * tmpdf$hhs_tot + (tmpdf$cspp - bl1size) * tmpdf$hhs_tot * (blratio - 1) *
            tmpdf$bl2
        )
      bl1_v <- c(bl1_v, bl1)
      tmpdf$bill_new <-
        1.06 * (
          fixed + tmpdf$cspp * tmpdf$hhs_tot * (bl1 + 0.0125) + (tmpdf$cspp - bl1size) *
            tmpdf$hhs_tot * (blratio - 1) * bl1 * tmpdf$bl2
        )
      tmpdf$difab <- tmpdf$bill_new - tmpdf$bill
      tmpdf$difpc <- tmpdf$difab * 100 / tmpdf$bill
      tmpdf$difpcinc <- tmpdf$difab * 100 / (tmpdf$income * 12)
      tmpdf$TEH_new <- tmpdf$bill_new * 100 / (tmpdf$income * 12)
      tmpdf$subs_new <- tmpdf$csmptv * avrprc - tmpdf$bill_new
      tmpdf$avrprc_new <- tmpdf$bill_new / tmpdf$csmptv
      colnames(tmpdf)[(ncol(tmpdf) - 6):ncol(tmpdf)] <-
        paste(
          c(
            "bill",
            "difab",
            "difpc",
            "difpcinc",
            "TEH",
            "subs",
            "avrprc"
          ),
          "ibtcap",
          fixed,
          sep = "_"
        )
      
    }
    tmpdf <-
      tmpdf[, c("id", grep("_ibtcap_", colnames(tmpdf), value = T))]
    list(tmpdf, bl1_v)
  }

### packages ----------


loadpackage("here")
loadpackage("dplyr")
loadpackage("ggplot2")
loadpackage("reshape2")
loadpackage("raster")
loadpackage("sf")
loadpackage("scico")
loadpackage("kableExtra")
loadpackage("tidyverse")
loadpackage("cowplot")

source(here("scripts", "general_functions.R"))


### plot params -----------

# col1_dark <- scico(1, palette = "lapaz", begin = 0.2)
col1_dark <- scico(1, palette = "lajolla", begin = 0.7)

# col1_light <- scico(1, palette = "lapaz", begin = 0.4)
col1_light <- scico(1, palette = "lajolla", begin = 0.4)

pal_div <- "roma"
pal_con <- "oslo"
# pal_disc <- "lapaz"
pal_disc <- "lajolla"

pal_bg <- 0.2
pal_end <- 0.8

fig_d1 <- 3.54331
fig_d2 <- 5.51181
fig_d3 <- 7.48031


knitr::opts_chunk$set(fig.width = fig_d1,
                      fig.height = fig_d1,
                      dpi = 1000)

## 1.2 load data --------

### data folder ----
rdir <- "data/raw"
pdir <- "data/processed"

### survey data ----------

surv14 <-
  read.csv(file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_AquaWal_prd.csv"
  ))

### price data --------------

price <-
  read.csv(here(pdir, "water_price_Aquawal_Wal/water_price_Wal_12_17.csv"))

# 2. data processing ----------
## 2.1 select data ----------

df <- surv14[surv14$csmptv <= 300 & surv14$csmptv >= 10,]

df <- df[df$dtbtor %in% c("CILE", "SWDE", "inBW"),]

df <- df[complete.cases(df[, c("income", "csmptv", "hhs_tot")]),]

## 2.2 variable processing -------
# useful vars:
# csmptv: consumption per household per year (m3/year)
# hhs_tot, hhs_0_19, hhs_20_95: household size, # children, # adults
# CVD, CVA, bill70, bill, avrprc, mgnprc: current tariff and bill
# income, inccat, inceqa: household income (EUR/month), categorized income (precarious, modest, average, higher), income per equivalent adults (OECD-scale, EUR/person/year)
# dtbtor: water distributors (consider only 3 big companies SWDE, CILE, inBW)
# bltu5c10: built-up density (0-6) calculated by Ahmed for 2010 landuse

### consumption per person (m3/year)

df$cspp <- df$csmptv / df$hhs_tot

### bill per person --------
df$billpp <- df$bill / df$hhs_tot

### bill components ---------

df$fixedpc <- 1.06 * (20 * df$CVD + 30 * df$CVA) * 100 / df$bill


df$fsapc <- 1.06 * 0.0125 * df$csmptv * 100 / df$bill

df$volpc <- 100 - df$fixedpc  - df$fsapc


### income per person ------

df$incpp <- df$income / df$hhs_tot

### income quintiles -------

df <- df %>% mutate(incqnt = ntile(income, 5))
df$incqnt <- as.factor(df$incqnt)

### income per equivalent adults quintiles ----

df <- df %>% mutate(ieaqnt = ntile(inceqa, 5))
df$ieaqnt <- as.factor(df$ieaqnt)

### income per capita quintiles -------

df <- df %>% mutate(ippqnt = ntile(incpp, 5))
df$ippqnt <- as.factor(df$ippqnt)

### relevel distributors ------

df$dtbtor <-
  factor(df$dtbtor,
         levels = lvls(df$dtbtor)[c(3, 1, 2)])

### average price per person ------

df$avprpp <- df$avrprc / df$hhs_tot


### relevel built-up density ---------



df$bltu5c10 <-
  car::recode(df$bltu5c10,
              "c('0', '1') = 'low'; c('2','3') = 'medium'; c('4','5') = 'high' ")


df$bltu5c10 <- factor(df$bltu5c10, lvls(df$bltu5c10)[c(2, 3, 1)])


### average distributor prices ----------

dtbt_df <- df %>%
  group_by(dtbtor) %>%
  summarise(
    nfam = n(),
    CVD = unique(CVD),
    CVA = unique(CVA),
    dtbt_avrprc = sum(bill) / sum(csmptv),
    bl1 = 1.06 * (mean(CVD) * 0.5),
    bl2 = 1.06 * (mean(CVD) + mean(CVA))
  )

dtbt_df$blratio <- dtbt_df$bl2 / dtbt_df$bl1


df <- left_join(df, dtbt_df[, c("dtbtor", "dtbt_avrprc")])


### cross-subsidies -------

df$subs <- df$dtbt_avrprc * df$csmptv - df$bill

df$hhscat <- df$hhs_tot
df$hhscat[df$hhscat > 4] <- "5+"

df$inccat <-
  factor(df$inccat, levels = lvls(df$inccat)[c(4, 3, 1, 2)])

### rainwater tank numeric -----

df$rwt_num <- as.numeric(df$rwtank %in% "yes")

### distributors list ------

dtbts <- c("SWDE", "CILE", "inBW")

## 2.3. varying fixed --------

fixeds <- seq(0, 200, 50)

fixed_ls <- lapply(dtbts, fixed_f, fixeds = fixeds)

fixed_tariff <- as.data.frame(sapply(fixed_ls, "[[", 2))

fixed_tariff <- rbind(t(dtbt_df$CVD), fixed_tariff)

colnames(fixed_tariff) <-
  paste("CVD", dtbts, sep = "_")

fixed_tariff$CVA <- unique(df$CVA)

fixed_tariff$scenario <- c("As in 2014", 1:(nrow(fixed_tariff) - 1))

fixed_tariff$fixed <-
  c(20 * mean(df$CVD) + 30 * mean(df$CVA), fixeds)

fixed_tariff$rwtt <- 0

avr_cvd <-
  apply(fixed_tariff[, grep("CVD", colnames(fixed_tariff))], 1, weighted.mean, w = dtbt_df$nfam)

fixed_tariff$mgpr_bl1 <- avr_cvd * 0.5
fixed_tariff$mgpr_bl2 <- avr_cvd + unique(fixed_tariff$CVA)



fixed_df <- Reduce(rbind, lapply(fixed_ls, "[[", 1))



## 2.4. rainwater tank tax ---------

rwtts <- seq(0, 200, 50)

rwtt_ls <- lapply(dtbts, rwtt_f, rwtts = rwtts)

rwtt_tariff <- as.data.frame(sapply(rwtt_ls, "[[", 2))

colnames(rwtt_tariff) <-
  paste("CVD", dtbts, sep = "_")

rwtt_tariff$CVA <- unique(df$CVA)

rwtt_tariff$scenario <-
  (nrow(fixed_tariff)):(nrow(fixed_tariff) + nrow(rwtt_tariff) - 1)

avr_cvd <-
  apply(rwtt_tariff[, grep("CVD", colnames(rwtt_tariff))], 1, weighted.mean, w = dtbt_df$nfam)

rwtt_tariff$fixed <- avr_cvd * 20 + unique(df$CVA) * 30

rwtt_tariff$rwtt <- rwtts

rwtt_tariff$mgpr_bl1 <- avr_cvd * 0.5
rwtt_tariff$mgpr_bl2 <- avr_cvd + unique(rwtt_tariff$CVA)


rwtt_df <- Reduce(rbind, lapply(rwtt_ls, "[[", 1))



## 2.5 UP vs IBTcap vs IBTcon ---------
# fixeds = seq(0, 100, 50)
# IBTcon 2 block 0-30 & 30+ price bl2 = 3.5*bl1
# IBTcap 2 block 0-12.5 & 12.5+ price bl2 = 3.5*bl1
# sumbill the same for each utilities

fixeds <- seq(0, 100, 50)
revincrs <- c(0, 0.1, 0.2)



### UP ---------

up_ls <- lapply(revincrs, function(x) {
  res_ls <- lapply(dtbts, up_f, revincr = x, fixeds = fixeds)
  
  res_tariff <- as.data.frame(sapply(res_ls, "[[", 2))
  colnames(res_tariff) <-
    paste("bl1", dtbts, sep = "_")
  res_tariff$fixed <- fixeds
  res_tariff$revincr <- x
  
  res_df <- Reduce(rbind, lapply(res_ls, "[[", 1))
  colnames(res_df)[2:ncol(res_df)] <-
    paste(colnames(res_df)[2:ncol(res_df)], x, sep = "_")
  
  list(res_df, res_tariff)
  
})

up_df <- Reduce(left_join, lapply(up_ls, "[[", 1))

up_tariff <- Reduce(rbind, lapply(up_ls, "[[", 2))

avr_bl1 <-
  apply(up_tariff[, grep("bl1", colnames(up_tariff))], 1, weighted.mean, w = dtbt_df$nfam)

up_tariff$mgpr_bl1 <- avr_bl1
up_tariff$mgpr_bl2 <- avr_bl1

### ibtcon --------

ibtcon_ls <- lapply(revincrs, function(x) {
  res_ls <- lapply(dtbts, ibtcon_f, revincr = x, fixeds = fixeds)
  
  res_tariff <- as.data.frame(sapply(res_ls, "[[", 2))
  colnames(res_tariff) <-
    paste("bl1", dtbts, sep = "_")
  res_tariff$fixed <- fixeds
  res_tariff$revincr <- x
  
  res_df <- Reduce(rbind, lapply(res_ls, "[[", 1))
  colnames(res_df)[2:ncol(res_df)] <-
    paste(colnames(res_df)[2:ncol(res_df)], x, sep = "_")
  
  list(res_df, res_tariff)
  
})

ibtcon_df <- Reduce(left_join, lapply(ibtcon_ls, "[[", 1))

ibtcon_tariff <- Reduce(rbind, lapply(ibtcon_ls, "[[", 2))

avr_bl1 <-
  apply(ibtcon_tariff[, grep("bl1", colnames(ibtcon_tariff))], 1, weighted.mean, w = dtbt_df$nfam)

ibtcon_tariff$mgpr_bl1 <- avr_bl1
ibtcon_tariff$mgpr_bl2 <- avr_bl1 * 3.5

### ibtcap --------

ibtcap_ls <- lapply(revincrs, function(x) {
  res_ls <- lapply(dtbts, ibtcap_f, revincr = x, fixeds = fixeds)
  
  res_tariff <- as.data.frame(sapply(res_ls, "[[", 2))
  colnames(res_tariff) <-
    paste("bl1", dtbts, sep = "_")
  res_tariff$fixed <- fixeds
  res_tariff$revincr <- x
  
  res_df <- Reduce(rbind, lapply(res_ls, "[[", 1))
  colnames(res_df)[2:ncol(res_df)] <-
    paste(colnames(res_df)[2:ncol(res_df)], x, sep = "_")
  
  list(res_df, res_tariff)
  
})

ibtcap_df <- Reduce(left_join, lapply(ibtcap_ls, "[[", 1))

ibtcap_tariff <- Reduce(rbind, lapply(ibtcap_ls, "[[", 2))

avr_bl1 <-
  apply(ibtcap_tariff[, grep("bl1", colnames(ibtcap_tariff))], 1, weighted.mean, w = dtbt_df$nfam)

ibtcap_tariff$mgpr_bl1 <- avr_bl1
ibtcap_tariff$mgpr_bl2 <- avr_bl1 * 3.5

# 3. Outputs -------

## 3.1. univariate -----------


#+ price, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

plotdf <- price[price$dtbtor %in%  c("CILE", "inBW", "SWDE"), ]

plotdf$dtbtor <-
  factor(plotdf$dtbtor, lvls(plotdf$dtbtor)[c(3, 1, 2)])

p1 <-
  ggplot(plotdf) +
  geom_line(aes(x = year, y = CVA, linetype = "CVA"), size = 0.8) +
  geom_line(aes(
    x = year,
    y = CVD,
    linetype = "CVD",
    col = dtbtor
  ), size = 0.8)  +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  scale_color_scico_d(
    palette = pal_disc,
    end = pal_end,
    begin = pal_bg,
    direction = -1
  ) +
  theme_kat() +
  labs(
    x = "Year",
    y = "Water tariff components (€)",
    color = "Utilities",
    linetype = "Tariff components"
  ) +
  ylim(0, 3)

p2 <-
  ggplot(plotdf, aes(x = year, y = bill70, col = dtbtor)) +
  geom_line(size = 0.8) +
  theme_kat() +
  scale_color_scico_d(
    palette = pal_disc,
    end = pal_end,
    begin = pal_bg,
    direction = -1
  ) +
  labs(x = "Year", y = "Water bill for an example family (€)", color = "Utilities")  +
  ylim(100, 420)


legend <- get_legend(p1 +  guides(color = guide_legend(nrow = 1)))

pg <-
  plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    nrow = 1,
    align = "hv",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, 0.135))

# #+ inchist, fig.cap = "Household income histogram", echo = F, message = F
#
#
# ggplot(df, aes(x = income)) +
#   geom_histogram(fill = col1_dark,
#                  binwidth = 500) +
#   labs(x = "Household diposable income (Eur/month)") +
#   theme_kat()

# #+ inccat, echo = F, message = F, results = "asis"
#
# inccatdf <- df[!(is.na(df$inccat)),] %>%
#   group_by(inccat) %>%
#   summarise(
#     count = n(),
#     prop = n() / nrow(df),
#     income_avr = mean(income),
#     income_min = min(income),
#     income_max = max(income),
#     inceqa_avr = mean(inceqa),
#     inceqa_min = min(inceqa),
#     inceqa_max = max(inceqa)
#   )
#
# kabble(inccatdf)

# #+ hhsplot, fig.cap = "Household income histogram", echo = F, message = F
#
#
# ggplot(df, aes(x = hhscat)) +
#   geom_bar(fill = col1_dark) +
#   labs(x = "Household size") +
#   theme_kat()

# #+ cspthist, fig.cap = "Household income histogram", echo = F, message = F
#
#
# ggplot(df, aes(x = csmptv)) +
#   geom_histogram(fill = col1_dark) +
#   labs(x = expression(Consumption ~ (m ^ 3))) +
#   theme_kat()

#+ billcomp, echo = F, message = F, results = "asis"


prccmp_mean <-
  apply(df[, c("fixedpc", "volpc", "fsapc")], 2, mean, na.rm = T)
prccmp_sd <-
  apply(df[, c("fixedpc", "volpc", "fsapc")], 2, sd, na.rm = T)
prccmp_qt <-
  as.data.frame(t(apply(df[, c("fixedpc", "volpc", "fsapc")], 2, quantile)))

billcomp <-
  data.frame(
    "components" = c("fixedpc", "volpc", "fsapc"),
    "mean" = prccmp_mean,
    "sd" = prccmp_sd,
    prccmp_qt
  )

kable(billcomp)

# #+ tabavrprc, echo = F, message = F, results = "asis"
#
#
# cat("\n")
# knitr::kable(
#   dtbt_df,
#   digits = 4,
#   col.names = c(
#     "Utilities",
#     "Number of households",
#     "CVD",
#     "CVA",
#     "Average price",
#     "Block 1 price",
#     "Block 2 price",
#     "Block2/Block1"
#   )
# )
# cat("\n")


#+ avmgprcsm, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

plotdf <- data.frame(csmptv = rep(1:200, 3))
plotdf$CVD <- rep(dtbt_df$CVD, each = 200)
plotdf$dtbtor <- rep (dtbt_df$dtbtor, each = 200)
plotdf$CVA <- rep(dtbt_df$CVA, each = 200)

plotdf$ab30 <- as.numeric(plotdf$csmptv > 30)

plotdf$bill <-
  1.06 * ((20 * plotdf$CVD + 30 * plotdf$CVA)  + 0.5 * plotdf$csmptv * (plotdf$CVD +
                                                                          0.0125) + (plotdf$csmptv - 30) * (0.5 * plotdf$CVD + plotdf$CVA) * plotdf$ab30
  )

plotdf$avrprc <- plotdf$bill / plotdf$csmptv
plotdf$mgnprc <-
  0.5 * plotdf$CVD + (0.5 * plotdf$CVD + plotdf$CVA) * plotdf$ab30

p1 <- ggplot(plotdf, aes(x = csmptv, y = mgnprc, col = dtbtor)) +
  geom_line(size = 0.8) +
  scale_color_scico_d(
    palette = pal_disc,
    end = pal_end,
    begin = pal_bg,
    direction = -1
  ) +
  labs(
    x = expression(Consumption ~ (m ^ 3)),
    y = expression(Marginal ~ price ~ (EUR / m ^ 3)),
    col = "Utilities"
  ) +
  xlim(0, 150)  +
  ylim(0, 5) +
  theme_kat()

p2 <- ggplot(plotdf, aes(x = csmptv, y = avrprc, col = dtbtor)) +
  geom_line(size = 0.8) +
  scale_color_scico_d(
    palette = pal_disc,
    end = pal_end,
    begin = pal_bg,
    direction = -1
  ) +
  labs(
    x = expression(Consumption ~ (m ^ 3)),
    y = expression(Average ~ price ~ (EUR / m ^ 3)),
    col = "Utilities"
  ) +
  xlim(15, 150) +
  ylim(3, 9) +
  theme_kat()


legend <- get_legend(p1 +  guides(color = guide_legend(nrow = 1)))

pg <-
  plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    nrow = 1,
    align = "hv",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, 0.135))

#+ TEH

summary(df$TEH)
sum(df$TEH > 3) * 100 / nrow(df)


## 3.2 by income quantiles -------

### income quantiles -------


#+ tabinc, echo = F, message = F, results = "asis"

incqnt_tab <- df %>%
  group_by(incqnt) %>%
  summarise(
    count = n(),
    avrhhs = sum(hhs_tot) / n(),
    mininc = min(income),
    maxinc = max(income)
  )

cat("\n")
knitr::kable(
  incqnt_tab,
  caption = "Household income quintile characteristics",
  digits = 2,
  col.names = c(
    "Quintile",
    "Number of households",
    "Average household size",
    "Min income (EUR/month)",
    "Max income (EUR/month)"
  )
)

cat("\n")


#+ inceqa1, fig.cap = "Income per equivalent adults for different household income group", echo = F, message = F

ggplot(df, aes(x = incqnt, y = inceqa)) +
  geom_boxplot()  +
  stat_summary(
    fun = median,
    geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., col = "median"),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., col = "mean"),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  scale_color_manual(values = c(col1_dark, "black")) +
  labs(x = "Household income quintiles", y = "Income per equivalent adult (EUR/per/year)", color = "") +
  theme_kat()

# #+ inceqa2, fig.cap = "Income per equivalent adults for different household income group", echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = inceqa)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Household income quintiles", y = "Average income per equivalent adult (EUR/per/year)", color = "") +
#   theme_kat()

# #+ incpc, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = incpp)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Household income quintiles", y = "Average income per capita (EUR/per/year)", color = "") +
#   theme_kat()

### consumption vs income -------

#+ desstat1, warnings = F

# Correlation between water consumption and household income should use spearman?????

cor.test(df$csmptv, df$income, method = "pearson")
cor.test(df$csmptv, df$income, method = "spearman")

# Correlation between water consumption and income per equivalent adult should use spearman?????

cor.test(df$csmptv, df$inceqa, method = "pearson")
cor.test(df$csmptv, df$inceqa, method = "spearman")


#+ blprop1, fig.cap = "Proportion of household paying in which block by quantile", echo = F, message = F
#
# plotdf <- df %>%
#   group_by(incqnt) %>%
#   summarise(prpbl1 = sum(ab30 < 1) * 100 / n(),
#             prpbl2 = sum(ab30 > 0) * 100 / n())
#
# plotdf <- melt(plotdf, id.vars = c("incqnt"))
#
# ggplot(plotdf, aes(x = incqnt, y = value, fill = variable)) +
#   geom_col() +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1,
#     labels = c("block 1", "block 2")
#   ) +
#   labs(x = "Household income quintiles", y = "Proportion of household (%)", fill = "Tariff block") +
#   theme_kat()
#


# #+ blprop2, fig.cap = "Proportion of household paying in which block by income quintile and utilities", echo = F, message = F
#
# plotdf <- df %>%
#   group_by(incqnt, dtbtor) %>%
#   summarise(prpbl1 = sum(ab30 < 1) * 100 / n(),
#             prpbl2 = sum(ab30 > 0) * 100 / n())
#
# plotdf <- melt(plotdf, id.vars = c("incqnt", "dtbtor"))
#
# ggplot(plotdf, aes(x = incqnt, y = value, fill = variable)) +
#   geom_col() +
#   facet_wrap(. ~ dtbtor) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1,
#     labels = c("block 1", "block 2")
#   )  +
#   labs(x = "Household income quintiles", y = "Proportion of household (%)", fill = "Tariff block") +
#   theme_kat()

#+ csinc1, echo = F, message = F

p1 <- ggplot(df, aes(x = incqnt, y = csmptv)) +
  geom_boxplot()  +
  stat_summary(
    fun = median,
    geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., col = "median"),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., col = "mean"),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  scale_color_manual(values = c(col1_dark, "black"))  +
  labs(x = "Household income quintiles",
       y = expression(Average ~ consumption ~ (m ^ 3)),
       color = "") +
  theme_kat()

# #+ csinc2, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = csmptv)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Household income quintiles",
#        y = expression(Average ~ consumption ~ (m ^ 3)) ,
#        color = "") +
#   theme_kat()

### hhs vs income --------

#+ hhsinc1, echo = F, message = F

p2 <- ggplot(df, aes(x = incqnt, fill = hhscat)) +
  geom_bar(position = position_fill(reverse = T)) +
  scale_fill_scico_d(
    palette = pal_disc,
    begin = pal_bg,
    end = pal_end,
    direction = 1
  ) +
  labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Household size") +
  theme_kat() +
  guides(fill = guide_legend(nrow = 1))

# #+ hhsinc2, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, fill = as.factor(hhs_20_95))) +
#   geom_bar(position = position_fill(reverse = F)) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Number of adults") +
#   theme_kat() +
#   guides(fill = guide_legend(nrow = 1))

# #+ hhsieq, echo = F, message = F
#
# ggplot(df, aes(x = ieaqnt, fill = hhscat)) +
#   geom_bar(position = position_fill(reverse = F)) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Income per equivalent adult quintiles", y = "Proportion of households" , fill = "Household size") +
#   theme_kat()  +
#   guides(fill = guide_legend(nrow = 1))

### rainwter tank vs income -----------
#+ rwtinc, echo = F, message = F

p3 <- ggplot(df, aes(x = incqnt, fill = rwtank)) +
  geom_bar(position = position_fill(reverse = T)) +
  scale_fill_scico_d(
    palette = pal_disc,
    begin = pal_bg,
    end = pal_end,
    direction = 1
  ) +
  labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Rainwater tank") +
  theme_kat() +
  guides(fill = guide_legend(nrow = 1))


### built-up density vs income -----------
#+ densinc, echo = F, message = F
p4 <- df %>%
  drop_na(bltu5c10) %>%
  ggplot(aes(x = incqnt, fill = as.factor(bltu5c10))) +
  geom_bar(position = position_fill(reverse = T)) +
  scale_fill_scico_d(
    palette = pal_disc,
    begin = pal_bg,
    end = pal_end,
    direction = 1
  ) +
  labs(x = "Household income quintiles", y = "Proportion of households" , fill = "Buit-up density") +
  theme_kat() +
  guides(fill = guide_legend(nrow = 1))

### combine plot 1 ---------
#+ desinc, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d2

plot_grid(p1,
          p2,
          p3,
          p4,
          nrow = 2,
          align = "hv",
          labels = "AUTO")

### water bill vs income -----------

# #+ billinc1, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = bill)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = col1_dark, "black"))  +
#   labs(x = "Household income quintiles", y = "Water bill in 2014 (EUR)", color = "") +
#   theme_kat()

# #+ billinc2, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = bill)) +
#   stat_summary(
#     fun = sum,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Household income quintiles", y = "Total water bills in 2014 (EUR)", color = "") +
#   theme_kat()

### billcomponents vs income ---------

#+ bcpninc, echo = F, message = F, results = "asis"

bcpninc <- df %>%
  group_by(incqnt) %>%
  summarise_at(c("fixedpc", "volpc", "fsapc"), list(mean, sd))

bcpninc[, -1] <-
  apply(bcpninc[, -1], 2, format, digits = 2, nsmall = 2L)

bcpninc$fixed <-
  paste(bcpninc$fixedpc_fn1, bcpninc$fixedpc_fn2, sep = "\u00b1")

bcpninc$vol <-
  paste(bcpninc$volpc_fn1, bcpninc$volpc_fn2, sep = "\u00b1")

bcpninc$fsa <-
  paste(bcpninc$fsapc_fn1, bcpninc$fsapc_fn2, sep = "\u00b1")

kable(bcpninc)

### TEH vs income ------------
#+ TEHinc, echo = F, message = F

p1 <- ggplot(df, aes(x = incqnt, y = TEH)) +
  geom_boxplot()  +
  stat_summary(
    fun = median,
    geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., col = "median"),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y.., col = "mean"),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  scale_color_manual(values = c(col1_dark, "black"))  +
  labs(x = "Household income quintiles", y = "Ratio of water bill to income (%)", color = "") +
  theme_kat()

#+ TEHprop, echo = F, message = F, results = "asis"


tehprop <- df %>%
  group_by(incqnt) %>%
  summarise(tehprop = sum(TEH > 3) * 100 / n())


cat("\n")
knitr::kable(tehprop)
cat("\n")



### marginal price vs income -------------

# #+ mgprinc1, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = mgnprc)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black")) +
#   labs(x = "Household income quintiles",
#        y = expression(Marginal ~ price ~ (EUR / m ^ 3)),
#        color = "") +
#   ylim(1, 15) +
#   theme_kat()

# #+ mgprinc2, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = mgnprc)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Household income quintiles",
#        y = expression(Marginal ~ price ~ (EUR / m ^ 3)) ,
#        color = "") +
#   theme_kat()


# #+ mgrprchhsinc, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = mgnprc, fill = hhscat)) +
#   stat_summary(fun = mean,
#                geom = "col",
#                position = "dodge") +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Household income quintiles",
#        y = expression(Marginal ~ price ~ (EUR / m ^ 3)) ,
#        fill = "Household size") +
#   theme_kat()  +
#   guides(fill = guide_legend(nrow = 1))



### average price vs income ---------


# #+ avprinc1, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = avrprc)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black")) +
#   labs(x = "Household income quintiles",
#        y = expression(Average ~ price ~ (EUR / m ^ 3)),
#        color = "") +
#   ylim(1, 15) +
#   theme_kat()

# #+ avprinc2, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = avrprc)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Household income quintiles",
#        y = expression(Average ~ price ~ (EUR / m ^ 3)) ,
#        color = "") +
#   theme_kat()

# #+ avprinc3, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = avprpp)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Household income quintiles",
#        y = expression(Average ~ price ~ (EUR / person / m ^ 3)) ,
#        color = "") +
#   theme_kat()

# #+ avrprchhsinc, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = avrprc, fill = hhscat)) +
#   stat_summary(fun = mean,
#                geom = "col",
#                position = "dodge") +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Household income quintiles",
#        y = expression(Average ~ price ~ (EUR / m ^ 3)) ,
#        fill = "Household size") +
#   theme_kat()  +
#   guides(fill = guide_legend(nrow = 1))





### subsidy vs income ----------------

# #+ subsinc1, echo = F, message = F
#
# ggplot(df, aes(x = incqnt, y = subs)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black"))  +
#   labs(x = "Household income quintiles", y = "Subsidy for water bill (EUR)", color = "") +
#   theme_kat()

#+ subsinc2, echo = F, message = F

p2 <- ggplot(df, aes(x = incqnt, y = subs)) +
  stat_summary(fun = sum,
               geom = "col",
               fill = col1_dark) +
  labs(x = "Household income quintiles", y = "Subsidy for water bill (EUR)", color = "") +
  ylim(-16800, 16800) +
  theme_kat()

### combine plot 2 --------



#+ equity, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

plot_grid(p1,
          p2,
          nrow = 1,
          align = "hv",
          labels = "AUTO")

## 3.3. by built-up density ------

### consumption vs built-up -------

# #+ csdens1, echo = F, message = F
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = csmptv)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black"))  +
#   labs(x = "Built-up density",
#        y = expression(Average ~ consumption ~ (m ^ 3)),
#        color = "") +
#   theme_kat()

# #+ csdens2, echo = F, message = F
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = csmptv)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Built-up density",
#        y = expression(Average ~ consumption ~ (m ^ 3)) ,
#        color = "") +
#   theme_kat()

### consumption vs built-up -------

# #+ incdens1, echo = F, message = F
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = income)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black"))  +
#   labs(x = "Built-up density", y = "Household income (EUR/month)", color = "") +
#   theme_kat()

# #+ incdens2, echo = F, message = F
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = income)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Built-up density", y = "Household income (EUR/month)", color = "") +
#   theme_kat()

### hhs vs builtup density --------

# #+ hhsdens, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, fill = hhscat)) +
#   geom_bar(position = position_fill(reverse = F)) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Built-up density", y = "Proportion of households" , fill = "Household size") +
#   theme_kat() +
#   guides(fill = guide_legend(nrow = 1))

### rainwter tank vs builtup density-----------
# #+ rwtdens, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, fill = rwtank)) +
#   geom_bar(position = position_fill(reverse = F)) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Built-up density", y = "Proportion of households" , fill = "Rainwater tank") +
#   theme_kat()  +
#   guides(fill = guide_legend(nrow = 1))

### water bill vs density -----------

# #+ billdens, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = bill)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black"))  +
#   labs(x = "Built-up density", y = "Water bill in 2014 (EUR)", color = "") +
#   theme_kat()



### TEH vs density ------------
# #+ TEHdens, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = TEH)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black"))  +
#   labs(x = "Built-up density", y = "Ratio of water bill to income (%)", color = "") +
#   theme_kat()

### average price vs density ---------


# #+ avprdens1, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = avrprc)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black")) +
#   labs(x = "Built-up density",
#        y = expression(Average ~ price ~ (EUR / m ^ 3)),
#        color = "") +
#   ylim(1, 15) +
#   theme_kat()

# #+ avprdens2, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = avrprc)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Built-up density",
#        y = expression(Average ~ price ~ (EUR / m ^ 3)) ,
#        color = "") +
#   theme_kat()

# #+ avprdens3, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = avprpp)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Built-up density",
#        y = expression(Average ~ price ~ (EUR / person / m ^ 3)) ,
#        color = "") +
#   theme_kat()

# #+ avrprchhsdens, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = avrprc, fill = hhscat)) +
#   stat_summary(fun = mean,
#                geom = "col",
#                position = "dodge") +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Built-up density",
#        y = expression(Average ~ price ~ (EUR / m ^ 3)) ,
#        fill = "Household size") +
#   theme_kat()  +
#   guides(fill = guide_legend(nrow = 1))



### subsidy vs density ----------------
# #+ subsdens1, echo = F, message = F
#
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = subs)) +
#   geom_boxplot()  +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "median"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(ymax = ..y.., ymin = ..y.., col = "mean"),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black"))  +
#   labs(x = "Built-up density", y = "Subsidy for water bill (EUR)", color = "") +
#   theme_kat()

# #+ subsdens2, echo = F, message = F
#
# df %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = bltu5c10, y = subs)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     fill = col1_dark
#   ) +
#   labs(x = "Built-up density", y = "Subsidy for water bill (EUR)", color = "") +
#   theme_kat()

## 3.4. precarious ------

#+ avprpoor

summary(df$avrprc)
summary(df$avrprc[df$inccat == "precarious"])
summary(df$subs[df$inccat == "precarious"])

summary(df$mgnprc)
summary(df$mgnprc[df$inccat == "precarious"])

## 3.5. changing fixed  -----

### new cvd ------

#+ fixedtab, echo = F, message = F, results = "asis"

cat("\n")
knitr::kable(fixed_tariff, digits = 4)
cat("\n")

### by income ------

#+ fixpcinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1

plotdf <-
  left_join(df[, c("id", "incqnt")], fixed_df[, c("id", grep("difpc_", colnames(fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

p1 <- ggplot(plotdf) +
  geom_rect(
    data = plotdf[plotdf$fixed == 100,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(. ~ fixed, labeller = label_both) +
  geom_hline(
    yintercept = 0,
    col = "red" ,
    size = 0.3,
    linetype = "longdash"
  ) +
  labs(x = "Income quintiles", y = "Changes in water bill (%)") +
  theme_kat()



# #+ fixpcincinc, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1
#
#
# plotdf <-
#   left_join(df[, c("id", "incqnt")], fixed_df[, c("id", grep("difpcinc_", colnames(fixed_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf) +
#   geom_rect(
#     data = plotdf[plotdf$fixed == 100,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = incqnt, y = value)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   geom_hline(yintercept = 0,
#              col = "red",
#              linetype = "longdash") +
#   labs(x = "Income quintiles", y = "Ratio of changes in bill to income (%)") +
#   theme_kat()


#+ fixTEHinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1


plotdf <-
  left_join(df[, c("id", "incqnt")], fixed_df[, c("id", grep("TEH_", colnames(fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

p3 <- ggplot(plotdf) +
  geom_rect(
    data = plotdf[plotdf$fixed == 100,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(. ~ fixed, labeller = label_both) +
  labs(x = "Income quintiles", y = "Ratio of water bill to income (%)") +
  theme_kat()


#+ fixsubsinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1


plotdf <-
  left_join(df[, c("id", "incqnt")], fixed_df[, c("id", grep("subs_", colnames(fixed_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))

p2 <-
  ggplot(plotdf) +
  geom_rect(
    data = plotdf[plotdf$fixed == 100,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  stat_summary(
    aes(x = incqnt, y = value),
    geom = "col",
    fun = sum,
    position = "dodge",
    fill = col1_dark
  ) +
  facet_grid(. ~ fixed, labeller = label_both) +
  geom_hline(
    yintercept = 0,
    col = "red" ,
    size = 0.3,
    linetype = "longdash"
  ) +
  labs(x = "Income quintiles", y = "Subsidy for water bill (EUR)") +
  ylim(-16800, 16800) +
  theme_kat() 

# #+ fixavprinc1, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
#
# plotdf <-
#   left_join(df[, c("id", "incqnt")], fixed_df[, c("id", grep("avrprc_", colnames(fixed_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf) +
#   geom_rect(
#     data = plotdf[plotdf$fixed == 100,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = incqnt, y = value)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   labs(x = "Income quintiles", y = expression(Average ~ price ~ (EUR / m ^
#                                                                    3))) +
#   theme_kat()

# #+ fixavprinc2, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
#
#
# ggplot(plotdf, aes(x = fixed, y = value, fill = incqnt))  +
#   annotate(
#     geom = "rect",
#     xmin = 75,
#     xmax = 125,
#     ymin = 0,
#     ymax = 8,
#     fill = col1_dark,
#     alpha = 0.5
#   ) +
#   stat_summary(geom  = "col",
#                position = "dodge",
#                fun = mean) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Fixed fee (EUR)",
#        y =  expression(Average ~ price ~ (EUR / m ^ 3)),
#        fill = "Income quintiles") +
#   theme_kat()

#### combine plot ----------

#+ fixcb, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3

plot_grid(p1,
          p2,
          p3,
          nrow = 3,
          align = "v",
          labels = "AUTO")

### by urban -----------
#
# #+ fixpcdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], fixed_df[, c("id", grep("difpc_", colnames(fixed_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$fixed == 100,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   geom_hline(yintercept = 0,
#              col = "red",
#              linetype = "longdash") +
#   labs(x = "Built-up density", y = "Changes in water bill (%)") +
#   theme_kat()

# #+ fixpcincdens, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1
#
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], fixed_df[, c("id", grep("difpcinc_", colnames(fixed_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$fixed == 100,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   geom_hline(yintercept = 0,
#              col = "red",
#              linetype = "longdash") +
#   labs(x = "Built-up density", y = "Ratio of changes in bill to income (%)") +
#   theme_kat()



# #+ fixTEHdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
#
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], fixed_df[, c("id", grep("TEH_", colnames(fixed_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
#
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$fixed == 100,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   labs(x = "Built-up density", y = "Ratio of water bill to income (%)") +
#   theme_kat()


# #+ fixsubsdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], fixed_df[, c("id", grep("subs_", colnames(fixed_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
#
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$fixed == 100,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   labs(x = "Built-up density", y = "Subsidy for water bill (EUR)") +
#   theme_kat()

# #+ fixavprdens1, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], fixed_df[, c("id", grep("avrprc_", colnames(fixed_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$fixed == 100,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   labs(x = "Built-up density", y = expression(Average ~ price ~ (EUR / m ^
#                                                                    3))) +
#   theme_kat()

# #+ fixavprdens2, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = fixed, y = value, fill = bltu5c10))  +
#   annotate(
#     geom = "rect",
#     xmin = 75,
#     xmax = 125,
#     ymin = 0,
#     ymax = 8,
#     fill = col1_dark,
#     alpha = 0.5
#   ) +
#   stat_summary(geom  = "col",
#                position = "dodge",
#                fun = mean) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Fixed fee (EUR)",
#        y =  expression(Average ~ price ~ (EUR / m ^ 3)),
#        fill = "Built-up density") +
#   theme_kat()
#
#
#

### precarious --------------

# #+ fixdpcpreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], fixed_df[, c("id", grep("difpc_", colnames(fixed_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# ggplot(plotdf, aes(x = as.factor(fixed), y = value)) +
#   geom_boxplot() +
#   geom_hline(
#     yintercept = 0,
#     col = col1_dark,
#     linetype = "longdash",
#     size = 0.8
#   ) +
#   labs(x = "Fixed fee (EUR)", y = "Changes in water bill (%)") +
#   theme_kat()

# #+ fixtehpreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], fixed_df[, c("id", grep("TEH_", colnames(fixed_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf, aes(x = as.factor(fixed), y = value)) +
#   geom_boxplot() +
#   labs(x = "Fixed fee (EUR)", y = "Ratio of water bill to income (%)") +
#   theme_kat()


# #+ fixsubspreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], fixed_df[, c("id", grep("subs_", colnames(fixed_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf, aes(x = as.factor(fixed), y = value)) +
#   geom_boxplot() +
#   labs(x = "Fixed fee (EUR)", y = "Subsidy for water bill (EUR)") +
#   theme_kat()

# #+ fixavprpreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], fixed_df[, c("id", grep("avrprc_", colnames(fixed_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf, aes(x = as.factor(fixed), y = value)) +
#   geom_boxplot() +
#   labs(x = "Fixed fee (EUR)", y = expression(Average ~ price ~ (EUR / m ^
#                                                                   3))) +
#   theme_kat()
#


## 3.6. changing rwtt  -----

### new cvd ------

#+ rwtttab, echo = F, message = F, results = "asis"

cat("\n")

knitr::kable(rwtt_tariff, digits = 4)
cat("\n")

### by income ------

#+ rwttpcinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1


plotdf <-
  left_join(df[, c("id", "incqnt")], rwtt_df[, c("id", grep("difpc_", colnames(rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

p1 <- ggplot(plotdf) +
  geom_rect(
    data = plotdf[plotdf$rwtt == 0,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  stat_summary(
    fun = median,
    geom = "errorbar",
    aes(
      x = incqnt,
      y = value,
      ymax = ..y..,
      ymin = ..y..,
      col = "median"
    ),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(
      x = incqnt,
      y = value,
      ymax = ..y..,
      ymin = ..y..,
      col = "mean"
    ),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  scale_color_manual(values = c(col1_dark, "black")) +
  facet_grid(. ~ rwtt, labeller = label_both) +
  geom_hline(
    yintercept = 0,
    col = "red" ,
    size = 0.3,
    linetype = "longdash"
  ) +
  labs(x = "Income quintiles", y = "Changes in water bill (%)", color = "") +
  theme_kat()

# #+ rwttpcincinc, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1
#
# plotdf <-
#   left_join(df[, c("id", "incqnt")], rwtt_df[, c("id", grep("difpcinc_", colnames(rwtt_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf) +
#   geom_rect(
#     data = plotdf[plotdf$rwtt %in% 0,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = incqnt, y = value)) +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(
#       x = incqnt,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "median"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(
#       x = incqnt,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "mean"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black")) +
#   facet_grid(. ~ rwtt, labeller = label_both) +
#   geom_hline(yintercept = 0,
#              col = "red",
#              linetype = "longdash") +
#   labs(x = "Income quintiles", y = "Ratio of changes in bill to income (%)") +
#   theme_kat()



#+ rwttTEHinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1

plotdf <-
  left_join(df[, c("id", "incqnt")], rwtt_df[, c("id", grep("TEH_", colnames(rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

p3 <- ggplot(plotdf) +
  geom_rect(
    data = plotdf[plotdf$rwtt %in% 0,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  stat_summary(
    fun = median,
    geom = "errorbar",
    aes(
      x = incqnt,
      y = value,
      ymax = ..y..,
      ymin = ..y..,
      col = "median"
    ),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  stat_summary(
    fun = mean,
    geom = "errorbar",
    aes(
      x = incqnt,
      y = value,
      ymax = ..y..,
      ymin = ..y..,
      col = "mean"
    ),
    width = 0.75,
    size = 1,
    linetype = "solid"
  ) +
  scale_color_manual(values = c(col1_dark, "black")) +
  facet_grid(. ~ rwtt, labeller = label_both) +
  labs(x = "Income quintiles", y = "Ratio of water bill to income (%)", color = "") +
  theme_kat()

#+ rwttsubsinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1

plotdf <-
  left_join(df[, c("id", "incqnt")], rwtt_df[, c("id", grep("subs_", colnames(rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

p2 <- ggplot(plotdf) +
  geom_rect(
    data = plotdf[plotdf$rwtt %in% 0,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  stat_summary(
    aes(x = incqnt, y = value),
    geom = "col",
    fun = sum,
    position = "dodge",
    fill = col1_dark
  ) +
  geom_hline(
    yintercept = 0,
    col = "red" ,
    size = 0.3,
    linetype = "longdash"
  ) +
  facet_grid(. ~ rwtt, labeller = label_both) +
  labs(x = "Income quintiles", y = "Subsidy for water bill (EUR)", color = "") +
  ylim(-16800, 16800) +
  theme_kat()

# #+ rwttavprinc1, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
# plotdf <-
#   left_join(df[, c("id", "incqnt")], rwtt_df[, c("id", grep("avrprc_", colnames(rwtt_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf) +
#   geom_rect(
#     data = plotdf[plotdf$rwtt %in% 0,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = incqnt, y = value)) +
#   facet_grid(. ~ rwtt, labeller = label_both) +
#   labs(x = "Income quintiles", y = expression(Average ~ price ~ (EUR / m ^
#                                                                    3))) +
#   theme_kat()

# #+ rwttavprinc2, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
#
# ggplot(plotdf, aes(x = rwtt, y = value, fill = incqnt))  +
#   annotate(
#     geom = "rect",
#     xmin = -25,
#     xmax = 25,
#     ymin = 0,
#     ymax = 8,
#     fill = col1_dark,
#     alpha = 0.5
#   ) +
#   stat_summary(geom  = "col",
#                position = "dodge",
#                fun = mean) +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   ) +
#   labs(x = "Fixed fee (EUR)",
#        y =  expression(Average ~ price ~ (EUR / m ^ 3)),
#        fill = "Income quintiles") +
#   theme_kat()

#### combine plot ----------

#+ rwttcb, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3

legend <- get_legend(p1 +  guides(color = guide_legend(nrow = 1)))

pg <-
  plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    p3 + theme(legend.position = "none"),
    nrow = 3,
    align = "v",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, 0.08))

### by urban -----------

# #+ rwttpcdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], rwtt_df[, c("id", grep("difpc_", colnames(rwtt_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$rwtt %in% 0,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(
#       x = bltu5c10,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "median"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(
#       x = bltu5c10,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "mean"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black")) +
#   facet_grid(. ~ rwtt, labeller = label_both) +
#   geom_hline(yintercept = 0,
#              col = "red",
#              linetype = "longdash") +
#   labs(x = "Built-up density", y = "Changes in water bill (%)") +
#   theme_kat()

# #+ rwttpcincdens, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], rwtt_df[, c("id", grep("difpcinc_", colnames(rwtt_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$rwtt %in% 0,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(
#       x = bltu5c10,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "median"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(
#       x = bltu5c10,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "mean"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black")) +
#   facet_grid(. ~ rwtt, labeller = label_both)  +
#   geom_hline(yintercept = 0,
#              col = "red",
#              linetype = "longdash") +
#   labs(x = "Built-up density", y = "Ratio of changes in bill to income (%)") +
#   theme_kat()


# #+ rwttTEHdens1, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], rwtt_df[, c("id", grep("TEH_", colnames(rwtt_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$rwtt %in% 0,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   stat_summary(
#     fun = median,
#     geom = "errorbar",
#     aes(
#       x = bltu5c10,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "median"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "errorbar",
#     aes(
#       x = bltu5c10,
#       y = value,
#       ymax = ..y..,
#       ymin = ..y..,
#       col = "mean"
#     ),
#     width = 0.75,
#     size = 1,
#     linetype = "solid"
#   ) +
#   scale_color_manual(values = c(col1_dark, "black")) +
#   facet_grid(. ~ rwtt, labeller = label_both)  +
#   labs(x = "Built-up density", y = "Ratio of water bill to income (%)") +
#   theme_kat()

# #+ rwttTEHdens2, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = rwtt, y = value)) +
#   stat_summary(
#     fun = mean,
#     geom = "col",
#     aes(fill = bltu5c10),
#     position = "dodge"
#   )  +
#   scale_fill_scico_d(
#     palette = pal_disc,
#     begin = pal_bg,
#     end = pal_end,
#     direction = -1
#   )  +
#   labs(x = "Rainwater tank tax (EUR)", y = "Ratio of water bill to income (%)", "Built-up density") +
#   theme_kat()



# #+ rwttsubsdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1
#
# plotdf <-
#   left_join(df[, c("id", "bltu5c10")], rwtt_df[, c("id", grep("subs_", colnames(rwtt_df), value = T))])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_rect(
#     data = plotdf[plotdf$rwtt %in% 0,][1,],
#     fill = col1_dark,
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.5
#   ) +
#   geom_boxplot(aes(x = bltu5c10, y = value)) +
#   facet_grid(. ~ rwtt, labeller = label_both) +
#   labs(x = "Built-up density", y = "Subsidy for water bill (EUR)") +
#   theme_kat()

#+ rwttavprdens1, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1

plotdf <-
  left_join(df[, c("id", "bltu5c10")], rwtt_df[, c("id", grep("avrprc_", colnames(rwtt_df), value = T))])

plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))

plotdf %>%
  drop_na(bltu5c10) %>%
  ggplot() +
  geom_rect(
    data = plotdf[plotdf$rwtt %in% 0,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  geom_boxplot(aes(x = bltu5c10, y = value)) +
  facet_grid(. ~ rwtt, labeller = label_both) +
  labs(x = "Built-up density", y = expression(Average ~ price ~ (EUR / m ^
                                                                   3))) +
  theme_kat()

#+ rwttavprdens2, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1


plotdf %>%
  drop_na(bltu5c10) %>%
  ggplot(aes(x = rwtt, y = value, fill = bltu5c10))  +
  annotate(
    geom = "rect",
    xmin = -25,
    xmax = 25,
    ymin = 0,
    ymax = 8,
    fill = col1_dark,
    alpha = 0.5
  ) +
  stat_summary(geom  = "col",
               position = "dodge",
               fun = mean) +
  scale_fill_scico_d(
    palette = pal_disc,
    begin = pal_bg,
    end = pal_end,
    direction = -1
  ) +
  labs(x = "Rainwater tank tax (EUR)",
       y =  expression(Average ~ price ~ (EUR / m ^ 3)),
       fill = "Built-up density") +
  theme_kat()


### precarious --------------

# #+ rwttdpcpreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], rwtt_df[, c("id", grep("difpc_", colnames(rwtt_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
# ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 0,
#              col = "red",
#              linetype = "longdash") +
#   labs(x = "Rainwater tank tax (EUR)", y = "Changes in water bill (%)") +
#   theme_kat()

# #+ rwtttehpreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], rwtt_df[, c("id", grep("TEH_", colnames(rwtt_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) +
#   geom_boxplot() +
#   labs(x = "Rainwater tank tax (EUR)", y = "Ratio of water bill to income (%)") +
#   theme_kat()

# #+ rwttsubspreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], rwtt_df[, c("id", grep("subs_", colnames(rwtt_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) +
#   geom_boxplot() +
#   labs(x = "Rainwater tank tax (EUR)", y = "Subsidy for water bill (EUR)") +
#   scale_x_discrete(labels = rwtts) +
#   theme_kat()

# #+ rwttavprpreca, echo = F, message = F
#
# plotdf <-
#   left_join(df[, c("id", "inccat")], rwtt_df[, c("id", grep("avrprc_", colnames(rwtt_df), value = T))])
#
# plotdf <- plotdf[plotdf$inccat %in% "precarious",]
#
# plotdf <- melt(plotdf, id.vars = c("id", "inccat"))
#
# plotdf$rwtt <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# ggplot(plotdf, aes(x = as.factor(rwtt), y = value)) +
#   geom_boxplot() +
#   labs(x = "Rainwater tank tax (EUR)", y = expression(Average ~ price ~
#                                                         (EUR / m ^ 3))) +
#   scale_x_discrete(labels = rwtts) +
#   theme_kat()
#


## 3.7. UP vs IBTcap vs IBT con ----------


### new price ---------
#+ upcctariff, echo = F, message = F, results = "asis"
cat("\n")
kable(up_tariff)
cat("\n")

#+ ibtcontariff, echo = F, message = F, results = "asis"
cat("\n")
kable(ibtcon_tariff)
cat("\n")

#+ ibtcaptariff, echo = F, message = F, results = "asis"
cat("\n")
kable(ibtcap_tariff)
cat("\n")

### by income ------------

# #+ upccpcinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
#
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("difpc_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "incqnt")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
# plotdf %>%
#   filter(revincr == 0) %>%
#   ggplot() +
#   geom_boxplot(aes(x = tariff, y = value, fill = incqnt)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   geom_hline(yintercept = 0,
#              col = "red" , size = 0.3,
#              linetype = "longdash") +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc)  +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   labs(x = "Tariff types", y = "Changes in water bill (%)", fill = "Income quintiles") +
#   theme_kat()

# #+ upcctehinc1, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
#
#
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("TEH_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "incqnt")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# plotdf %>%
#   filter(revincr == 0) %>%
#   ggplot() +
#   geom_boxplot(aes(x = tariff, y = value, fill = incqnt)) +
#   facet_grid(. ~ fixed, labeller = label_both) +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc) +
#   labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Income quintiles") +
#   theme_kat()

# #+ upcctehinc2, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
#
#
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("TEH_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "incqnt")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# ggplot(plotdf, aes(x = tariff, y = value, fill = incqnt))  +
#   stat_summary(fun = mean,
#                geom = "col",
#                position = "dodge") +
#   facet_grid(revincr ~ fixed, labeller = label_both) +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc) +
#   labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Income quintiles") +
#   theme_kat()


#+ upccsubsinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1

plotdf_ls <-
  lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
    x[, c("id", grep("subs_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <-
  gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <-
  as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


p2 <-
  plotdf %>%
  filter(revincr == 0) %>%
  ggplot(aes(x = tariff, y = value, fill = incqnt)) +
  stat_summary(geom = "col",
               fun = sum,
               position = "dodge") +
  facet_grid(. ~ fixed, labeller = label_both) +
  geom_hline(
    yintercept = 0,
    col = "red" ,
    size = 0.3,
    linetype = "longdash"
  ) +
  scale_fill_scico_d(begin = pal_bg,
                     end = pal_end,
                     palette = pal_disc) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types", y = "Subsidy for water bill (EUR)", fill = "Income quintiles") +
  ylim(-16800, 16800) +
  theme_kat()


p2b <-
  plotdf %>%
  filter(revincr == 0 & fixed == 100) %>%
  ggplot(aes(x = tariff, y = value, fill = incqnt)) +
  stat_summary(geom = "col",
               fun = sum,
               position = "dodge")  +
  geom_hline(
    yintercept = 0,
    col = "red" ,
    size = 0.3,
    linetype = "longdash"
  ) +
  scale_fill_scico_d(begin = pal_bg,
                     end = pal_end,
                     palette = pal_disc) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types", y = "Subsidy for water bill (EUR)", fill = "Income quintiles") +
  ylim(-16800, 16800) +
  theme_kat()

#+ upccavprinc, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d1

plotdf_ls <-
  lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
    x[, c("id", grep("avrprc_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <-
  gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <-
  as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


p1 <- plotdf %>%
  filter(revincr == 0) %>%
  ggplot(aes(x = tariff, y = value, fill = incqnt)) +
  geom_boxplot(outlier.size = 0.7, lwd = 0.3) +
  facet_grid(. ~ fixed, labeller = label_both) +
  scale_fill_scico_d(begin = pal_bg,
                     end = pal_end,
                     palette = pal_disc) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types",
       y =  expression(Average ~ price ~ (EUR / m ^ 3)),
       fill = "Income quintiles") +
  theme_kat()


p1b <- plotdf %>%
  filter(revincr == 0 & fixed == 100) %>%
  ggplot(aes(x = tariff, y = value, fill = incqnt)) +
  geom_boxplot(outlier.size = 0.7, lwd = 0.3) +
  scale_fill_scico_d(begin = pal_bg,
                     end = pal_end,
                     palette = pal_disc) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  labs(x = "Tariff types",
       y =  expression(Average ~ price ~ (EUR / m ^ 3)),
       fill = "Income quintiles") +
  theme_kat()


#### combine plot ----------

#+ upcccb, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d2

legend <- get_legend(p1 +  guides(color = guide_legend(nrow = 1)))

pg <-
  plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    nrow = 2,
    align = "v",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, 0.1))


#+ upcc100, echo = F, message = F , fig.width = fig_d1, fig.height = fig_d2

legend <- get_legend(p1b +  guides(color = guide_legend(nrow = 1)))

pg <-
  plot_grid(
    p1b + theme(legend.position = "none"),
    p2b + theme(legend.position = "none"),
    nrow = 2,
    align = "v",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, 0.1))



### by urban ------------

# #+ upccpcdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
#
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("difpc_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "bltu5c10")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_boxplot(aes(x = tariff, y = value, fill = bltu5c10)) +
#   facet_grid(revincr ~ fixed, labeller = label_both) +
#   geom_hline(yintercept = 0,
#              col = "red" , size = 0.3,
#              linetype = "longdash") +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc)  +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   labs(x = "Tariff types", y = "Changes in water bill (%)", fill = "Built-up density") +
#   theme_kat()
#
# #+ upcctehdens1, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
#
#
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("TEH_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "bltu5c10")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot() +
#   geom_boxplot(aes(x = tariff, y = value, fill = bltu5c10)) +
#   facet_grid(revincr ~ fixed, labeller = label_both) +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc) +
#   labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Built-up density") +
#   theme_kat()
#
# #+ upcctehdens2, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
#
#
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("TEH_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "bltu5c10")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = tariff, y = value, fill = bltu5c10))  +
#   stat_summary(fun = mean,
#                geom = "col",
#                position = "dodge") +
#   facet_grid(revincr ~ fixed, labeller = label_both) +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc) +
#   labs(x = "Tariff types", y = "Ratio of water bill to income (%)", fill = "Built-up density") +
#   theme_kat()
#
#
# #+ upccsubsdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("subs_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "bltu5c10")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot(aes(x = tariff, y = value, fill = bltu5c10)) +
#   stat_summary(geom = "col", fun = sum, position = "dodge") +
#   facet_grid(revincr ~ fixed, labeller = label_both) +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc) +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   labs(x = "Tariff types", y = "Subsidy for water bill (EUR)", fill = "Built-up density") +
#   theme_kat()

# #+ upccavprdens, echo = F, message = F , fig.width = fig_d3, fig.height = fig_d3
#
# plotdf_ls <-
#   lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
#     x[, c("id", grep("avrprc_", colnames(x), value = T))])
#
#
# plotdf <- Reduce(left_join, plotdf_ls)
# plotdf <- left_join(plotdf, df[, c("id", "bltu5c10")])
#
# plotdf <- melt(plotdf, id.vars = c("id", "bltu5c10"))
#
# plotdf$tariff <-
#   gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))
#
# plotdf$fixed <-
#   as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))
#
# plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))
#
#
# plotdf %>%
#   drop_na(bltu5c10) %>%
#   ggplot()  +
#   geom_boxplot(aes(x = tariff, y = value, fill = bltu5c10)) +
#   facet_grid(revincr ~ fixed, labeller = label_both) +
#   scale_fill_scico_d(begin = pal_bg,
#                      end = pal_end,
#                      palette = pal_disc) +
#   scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
#   labs(x = "Tariff types",
#        y =  expression(Average ~ price ~ (EUR / m ^ 3)),
#        fill = "Built-up density") +
#   theme_kat()

# testing -------

knitr::knit_exit()

#+ notcorrectedyet


#+ test, results = "asis"
cor(df$income, df$inceqa, method = "spearman")
# I have df$cspeqa but in L/day




df$hhsadj <- 1 + (df$hhs_20_95 - 1) * 0.5 + (df$hhs_0_19) * 0.3

df$cspea <- df$csmptv / df$hhsadj

inceqa <- df$income * 12 / df$hhsadj
plot(df$cspea, inceqa)
plot(df$csmptv, df$income)
cor(df$csmptv, df$income)
summary(lm(csmptv ~ hhs + income, df))
summary(lm(cspea ~ inceqa, df))

ggplot(df, aes(x = ieaqnt, y =  billpc)) +
  geom_boxplot()

plotdf_ls <-
  lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
    x[, c("id", grep("subs_", colnames(x), value = T))])

ggplot(df, aes(x = ipcqnt, fill = hhscat)) +
  geom_bar(position = position_fill(reverse = F)) +
  scale_fill_scico_d(
    palette = pal_disc,
    begin = pal_bg,
    end = pal_end,
    direction = -1
  ) +
  labs(x = "Income per equivalent adult quintiles", y = "Proportion of households" , fill = "Household size") +
  theme_kat()  +
  guides(fill = guide_legend(nrow = 1))

plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "ieaqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "ieaqnt"))

plotdf$tariff <-
  gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <-
  as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))


ggplot(plotdf, aes(x = tariff, y = value, fill = ieaqnt))  +
  stat_summary(geom = "col",
               fun = sum,
               position = "dodge") +
  facet_grid(revincr ~ fixed, labeller = label_both) +
  scale_fill_scico_d(begin = 0.3,
                     end = pal_end,
                     palette = pal_disc) +
  scale_x_discrete(labels = c("IBT-cap", "IBT-con", "UP")) +
  ylim(-16800, 16800) +
  labs(x = "Tariff types", y = "Subsidy for water bill (EUR)", fill = "Income quintiles") +
  theme_kat()

### teh when increase rev ---------
plotdf_ls <-
  lapply(list(up_df, ibtcon_df, ibtcap_df), function(x)
    x[, c("id", grep("TEH_", colnames(x), value = T))])


plotdf <- Reduce(left_join, plotdf_ls)
plotdf <- left_join(plotdf, df[, c("id", "incqnt")])

plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))

plotdf$tariff <-
  gsub("_.*", "", gsub("^[^_]+_", "", plotdf$variable))

plotdf$fixed <-
  as.numeric(gsub(".*_", "", gsub("_[^_]+$", "", plotdf$variable)))

plotdf$revincr <- as.numeric(gsub(".*_", "", plotdf$variable))

str(plotdf$revincr)

plotdf %>%
  filter(tariff == "ibtcon") %>%
  filter(incqnt == 1) %>%
  ggplot(aes(
    x = as.factor(fixed),
    y = value,
    fill = as.factor(revincr)
  )) +
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot(width = 0.6)   +
  scale_fill_scico_d(begin = 0.3,
                     end = pal_end,
                     palette = pal_disc) +
  labs(x = "Fixed", y = "Ratio of water bill to income (%)", fill = "Increase revenue") +
  theme_kat()

plotdf2 <- plotdf %>%
  group_by(fixed, revincr, incqnt) %>%
  summarise(prop = sum(value > 3) * 100 / n())

plotdf2 %>%
  filter(incqnt == 1) %>%
  ggplot(aes(
    x = as.factor(fixed),
    y = prop,
    fill = as.factor(revincr)
  )) +
  geom_col(position = "dodge")
## % of water effort > 3 different fixed
plotdf <-
  left_join(df[, c("id", "incqnt")], fixed_df[, c("id", grep("TEH_", colnames(fixed_df), value = T))])



plotdf <- melt(plotdf, id.vars = c("id", "incqnt"))
plotdf$fixed <- as.numeric(gsub(".*_", "", plotdf$variable))


plotdf2 <- plotdf %>%
  filter(incqnt == 1) %>%
  group_by(fixed) %>%
  summarise(prop = sum(value >= 3) * 100 / n())

ggplot(plotdf) +
  geom_rect(
    data = plotdf[plotdf$fixed == 100,][1,],
    fill = col1_dark,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5
  ) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_grid(. ~ fixed, labeller = label_both) +
  labs(x = "Income quintiles", y = "Ratio of water bill to income (%)") +
  theme_kat()

summary(10000 / df$bill)
summary(5000 / df$bill)

ggplot(df[df$TEH < 3,], aes(x = income, y = csmptv, color = TEH)) +
  geom_point() +
  scale_color_scico(palette = "oslo") +
  theme_kat()
