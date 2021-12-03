#' ---
#' title: "Redistribution effects of water tariffs"
#' author: "Nguyen Bich Ngoc, Jacques Teller"  
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: 
#'  bookdown::pdf_document2:
#'   theme: cosmo
#'   keep_md: true
#' ---
  



#+ r setup, include = F, message = F
# notes from last run----------------------



# 1. setup ---------





## 1.1. load functions -----------------

# install.packages("here")
library(here)


source(here("3 Scripts", "general_functions.R"))

loadpackage('dplyr')
loadpackage('ggplot2')
loadpackage('reshape2')
loadpackage('raster')
loadpackage('sf')

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

# 2. merge data ----------

surv14$year <- 2014

df <- dplyr::left_join(surv14, price)[, c('id', 'CVD', 'CVA', 'csmptv', 'inceqa', 'rwtank', 'hhs_0_19', 'hhs_20_95', 'income', 'inccat', 'dtbtor', "rprage")]
df$hhs <- df$hhs_0_19 + df$hhs_20_95

df$csmptv[df$csmptv > 300] <- NA



urban <- cbind.data.frame(id = surv14_coords$id, urban = extract(urban_10, surv14_coords))

urban <- urban[urban$urban > -1,]

df <- inner_join(df, urban)
df$cspc <- df$csmptv/df$hhs

df <- df[!is.na(df$csmptv) & !is.na(df$inceqa) & !is.na(df$CVD),]

df <- df[df$dtbtor %in% c("CILE", "SWDE", "IECBW"),]

df$ab30 <- as.numeric(df$csmptv > 30)

df$bill_cur <- (20*df$CVD + 30*df$CVA) + 0.0125*df$csmptv + 0.5*df$csmptv*df$CVD + 0.5*(df$csmptv - 30)*df$CVD*df$ab30 + (df$csmptv - 30)*df$CVA*df$ab30

df <- df %>% mutate(incqnt = ntile(income, 4))
df$incqnt <- as.factor(df$incqnt)
df$TEH <- df$bill_cur*100/(df$income*12)


#' # Introduction
#' ## Rational:
#' * Water tariff objectives:
#'    + self-sufficiency for service providers
#'    + equity among customers
#'    + conservation/economic efficiency for society
#' * Current tariff in Wallonia:
#'    + 2 parts: fixed + volumetric
#'    + fixed: 20CVD + 20CVA, differences among distributors are negligible
#'    + volumetric: 2 increasing blocks at connection level --- IBT-con &#40;actually 3 blocks, but household rarely reach block 3&#41;
#'       - 0-30 m^3^: 0.5*CVD
#'       - 30-500 m^3^: CVD + CVA
#' * Arguments for IBT:
#'    + incentive to save water &#40;higher price for larger consumption&#41;
#'    + pro-poor &#40;supposedly&#41;
#' * Arguments against IBT:
#'    + pro-poor often not true due to low correlation between water consumption and income. That value of Wallonia is `r cor(df$csmptv, df$income)` for household consumption and `r cor(df$cspc, df$income)` for consumption per inhabitant.
#'    + difficult to understand hence not clear signal for custumer to use water wisely  
#'    
#' ## Objectives
#' * Assess social aspects of current price tariffs
#' * Compare social equity of different hypothesized tariff schemes 
#' 
#' # Data and method
#' ## Data
#' * Utility survey data provided by Aquawal and CEHD
#'    + year: 2014     
#'    + 1534 households
#'    + 3 main distributors: SWDE &#40;1143&#41;, CILE &#40;265&#41;, inBW &#40;126&#41;
#'    + information include: water consumption, household size, income, rainwater tank ...
#' * built-up density
#'    + year: 2011
#'    + at 100x100m scale
#'    + 3 categories: low, medium, high
#'    
#' ## Method
#' * assess social aspects of current tariff
#'    + divide households into 4 groups using household income quartiles
#'    + pairwise analyses of household income and other factors: income per equivalent adults, water use, water bill, TEH ...
#' * compare different tariff scheme
#'    + current format with different changing fixed part
#'       - assumptions: keep same total bill in 2014 for all households within the same distributor & keep CVA of 2014 not changing
#'       - changing fixed part: EUR 0, 40, ..., 200 
#'       - recalculate CVD for each distributor at each value of fixed part
#'       - recalculate household water bill in 2014 and compare with the actual one
#'    + current format with potential tax on rainwater tank
#'       - assumptions: keep same total bill in 2014 for all households within the same distributor & keep CVA of 2014 not changing
#'       - changing watertank tax: EUR 0, 40, ..., 200 
#'       - recalculate CVD for each distributor at each value of fixed part
#'       - recalculate household water bill in 2014 and compare with the actual one
#'    + compare IBT-con, IBT-cap, linear
#' 
#' # Results
#' ## Social aspects of water tariff in Wallonia

#+ heading1, include = F

# 3. equity analysis ----
## 3.1 income quantile summary -------

incqnt_tab <- df %>%
  group_by(incqnt) %>%
  summarise(count = n(),
            npp = sum(hhs),
            mininc = min(income),
            maxinc = max(income))



#+ tabinc, echo = F, message = F
 
knitr::kable(incqnt_tab, caption = "Household income quartile characteristics", digits = 2, col.names = c("Quartile", "Number of households", "Number of people", "Min income (EUR/month)", "Max income (EUR/month)"))



#+ inceqa, fig.cap = "Income per equivalent adults for different household income group", echo = F, message = F

ggplot(df, aes(x = incqnt, y = inceqa)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Household income quartiles", y = "Income per equivalent adult")

knitr::knit_exit()

#+ new, echo = F, message = F
# shared total revenue by each income group?


ggplot(df, aes(x = incqnt, y = bill_cur)) + 
  stat_summary(
    fun = sum,
    geom = "col",
  )

# TEH by each income group?


ggplot(df, aes(x = incqnt, y = TEH)) + 
  geom_boxplot()

# average price per habitant per m3 by income group

df$pwpcpv <- df$bill_cur/df$csmptv/df$hhs

ggplot(df, aes(x = incqnt, y = pwpcpv)) + 
  geom_boxplot()


plotdf <- df %>% 
  group_by(incqnt) %>%
  summarise(tothab = sum(hhs),
            totbill = sum(bill_cur))

plotdf$billpc <- plotdf$totbill/plotdf$tothab

ggplot(plotdf, aes(x = incqnt, y = billpc)) +
  geom_col()

ggplot(df, aes(x = incqnt, y = inceqa)) +
  geom_boxplot()


df$incpc <- df$income/df$hhs

ggplot(df, aes(x = incqnt, y = incpc)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), col = 'blue', width = 0.75, size = 1, linetype = "solid") 


  
# marginal price vs income quartiles
# average priec vs income quantiles
# 4. changing fixed  -----
#+ changefixed, echo = F, message = F


vary_fixed_f <- function(dtbtorname = "SWDE", fixeds = seq (0, 200, 40)) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  
  total <- sum(tmpdf$bill_cur)
  fse <- sum(0.0125*tmpdf$csmptv)
  cvd_v <- numeric()
  
  for (fixed in fixeds) {
    
    cvd <- (total - fse - fixed*nrow(tmpdf) - sum((tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30))/sum(0.5*tmpdf$csmptv + 0.5*(tmpdf$csmptv - 30)*tmpdf$ab30)
    cvd_v <- c(cvd_v, cvd)
    tmpdf$bill_new <- fixed + 0.0125*tmpdf$csmptv + 0.5*tmpdf$csmptv*cvd + 0.5*(tmpdf$csmptv - 30)*cvd*tmpdf$ab30 + (tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30
    tmpdf$diff <- tmpdf$bill_new - tmpdf$bill_cur
    tmpdf$difpct <- tmpdf$diff*100/tmpdf$bill_cur
    colnames(tmpdf)[ncol(tmpdf) - 2] <- paste("bill_fixed", fixed, sep = "_")
    colnames(tmpdf)[ncol(tmpdf) - 1] <- paste("difab_fixed", fixed, sep = "_")
    colnames(tmpdf)[ncol(tmpdf)] <- paste("difpc_fixed", fixed, sep = "_")
    
  }
  list(tmpdf, cvd_v)
}


vary_fixed_ls <- lapply(c("SWDE", "CILE", "IECBW"),vary_fixed_f)

cvd <- sapply(vary_fixed_ls, function(x) x[[2]])

write.table(cvd, "clipboard", sep = "\t")

vary_fixed_df <- Reduce(rbind, lapply(vary_fixed_ls, function(x) x[[1]]))

df <- left_join(df, vary_fixed_df)

test <- df[,c("id", "income" , grep("difab_fixed", colnames(df), value = T))]

test <- test %>% mutate(incqnt = ntile(income, 4))
test$incqnt <- as.factor(test$incqnt)

test_lg <- melt(test, id.vars = c("id", "income", "incqnt"))
test_lg$fixed <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_wrap(.~ fixed) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "income quartile", y = "Changes in water bill (EUR)")

test <- df[,c("id", "income" , grep("difpc_fixed", colnames(df), value = T))]



test_lg <- melt(test, id.vars = c("id", "income", "incqnt"))
test_lg$fixed <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = incqnt, y = value)) +
  facet_wrap(.~ fixed) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "income quartile", y = "Changes in water bill (%)")

df$urban <- factor(df$urban)
df$urban <- car::recode(df$urban, "c('0', '1') = 'low'; c('2','3') = 'medium'; c('4','5') = 'high' ")

df$urban <- factor(df$urban, levels = lvls(df$urban)[c(2,3,1)])

test <- df[,c("id", "urban" , grep("difab_fixed", colnames(df), value = T))]

test_lg <- melt(test, id.vars = c("id", "urban"))
test_lg$fixed <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_wrap(.~ fixed) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "urbanization", y = "Changes in water bill (EUR)")

test <- df[,c("id", "urban" , grep("difpc_fixed", colnames(df), value = T))]

test_lg <- melt(test, id.vars = c("id", "urban"))
test_lg$fixed <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_wrap(.~ fixed) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "urbanization", y = "Changes in water bill (%)")


# ## 3.3. linear with and without fixed -----
# 
# unpr <- sum(df$bill_14)/sum(df$csmptv)
# 
# df$bill_ln <- df$csmptv*unpr
# 
# df$dif_ln <- df$bill_ln - df$bill_14
# 
# #### with fixed
# fixed <- 1.06*(20*mean(df$CVD) + 30*mean(df$CVA))
# 
# unpr1 <- (sum(df$bill_14) - nrow(df)*fixed)/sum(df$csmptv)
# 
# df$bill_lnwf <- fixed + df$csmptv*unpr1
# 
# 
# df$dif_lnwf <- df$bill_lnwf - df$bill_14
# 
# 
# 
## 3.4. block tariffs per captia -----

### Brussels scheme ------------

df$bl2 <- ifelse(df$cspc <= 30 & df$cspc > 15, 1, 0)
df$bl3 <- ifelse(df$cspc <= 60 & df$cspc >30, 1, 0)
df$bl4 <- ifelse(df$cspc > 60, 1, 0)

summary(df$bl3)

tot <- sum(df$bill_cur)


bl_2_1 <- 3.7696/2.115  # 1.782317
bl_3_1 <- 5.5726/2.115 # 2.634799
bl_4_1 <- 8.1338/2.115 # 3.845768

bl1 <- (tot - 25.23*nrow(df))/sum((df$cspc + (df$cspc-15)*(bl_2_1 - 1)*df$bl2 + (df$cspc-30)*(bl_3_1 - bl_2_1)*df$bl3 + (df$cspc-60)*(bl_4_1 - bl_3_1)*df$bl4)*df$hhs)

df$bill_brx <- 25.23 + bl1*(df$cspc + (df$cspc-15)*(bl_2_1 - 1)*df$bl2 + (df$cspc-30)*(bl_3_1 - bl_2_1)*df$bl3 + (df$cspc-60)*(bl_4_1 - bl_3_1)*df$bl4)*df$hhs

summary(df$bill_brx)
summary(df$bill_cur)
sum(df$bill_brx)
sum(df$bill_cur)
# 
# df$bill_brx <- ifelse(df$cspc <= 60 & df$cspc >30, 25.23 + (15*bl1 + 15*bl2 + (df$cspc - 30)*bl3)*df$hhs, df$bill_brx)
# 
# df$bill_brx <- ifelse(df$cspc <= 30 & df$cspc > 15, 25.23 + (15*bl1 + (df$cspc - 15)*bl2)*df$hhs, df$bill_brx)
# 
# df$bill_brx <- ifelse(df$cspc <=  15, 25.23 + (df$cspc*bl1)*df$hhs, df$bill_brx)
# 
# sum(df$bill_brx)
# sum(df$bill_14)
# 
# df$dif_brx <- df$bill_brx - df$bill_14
# 
## 3.5. rainwater tank tax ---------
#+ changerwtt

df$rwtank <- as.numeric(df$rwtank %in% "yes")


vary_rwtf_f <- function(dtbtorname = "SWDE", rwtfs = seq (0, 200, 40)) {
  tmpdf <- df[df$dtbtor %in% dtbtorname,]
  
  total <- sum(tmpdf$bill_cur)
  fse <- sum(0.0125*tmpdf$csmptv)
  cvd_v <- numeric()
  
  for (rwtf in rwtfs) {
    
    rwtt <- sum(rwtf*tmpdf$rwtank)
    
    cvd <- (total - fse - rwtt - sum(30*tmpdf$CVA + (tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30))/sum(20 + 0.5*tmpdf$csmptv + 0.5*(tmpdf$csmptv - 30)*tmpdf$ab30)
    cvd_v <- c(cvd_v, cvd)
    tmpdf$bill_new <- rwtf*tmpdf$rwtank + 30*tmpdf$CVA + 20*cvd + 0.0125*tmpdf$csmptv + 0.5*tmpdf$csmptv*cvd + 0.5*(tmpdf$csmptv - 30)*cvd*tmpdf$ab30 + (tmpdf$csmptv - 30)*tmpdf$CVA*tmpdf$ab30
    tmpdf$diff <- tmpdf$bill_new - tmpdf$bill_cur
    tmpdf$difpct <- tmpdf$diff*100/tmpdf$bill_cur
    colnames(tmpdf)[ncol(tmpdf) - 2] <- paste("bill_rwtt", rwtf, sep = "_")
    colnames(tmpdf)[ncol(tmpdf) - 1] <- paste("difab_rwtt", rwtf, sep = "_")
    colnames(tmpdf)[ncol(tmpdf)] <- paste("difpc_rwtt", rwtf, sep = "_")
    
  }
  list(tmpdf, cvd_v)
}

### 0-200,40 -------------
vary_rwtf_ls <- lapply(c("SWDE", "CILE", "IECBW"),vary_rwtf_f)

cvd <- sapply(vary_rwtf_ls, function(x) x[[2]])

write.table(cvd, "clipboard", sep = "\t")

ndtb <- matrix(table(df$dtbtor)[c(3,1,2)])

write.table((cvd%*%ndtb*20 + unique(df$CVA)*30*1534)/1534, "clipboard", sep = "\t")

vary_rwtf_df <- Reduce(rbind, lapply(vary_rwtf_ls, function(x) x[[1]]))

df <- left_join(df, vary_rwtf_df)

test <- df[,c("id", "income" , grep("difab_rwtt", colnames(df), value = T))]

test <- test %>% mutate(incqnt = ntile(income, 4))
test$incqnt <- as.factor(test$incqnt)

test_lg <- melt(test, id.vars = c("id", "income", "incqnt"))
test_lg$rwtt <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg[test_lg$rwtt > 0,], aes(x = incqnt, y = value)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), col = 'blue', width = 0.75, size = 1, linetype = "solid") +
  facet_wrap(.~ rwtt) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "income quartile", y = "Changes in water bill (EUR)")

test <- df[,c("id", "income" , grep("difpc_rwtt", colnames(df), value = T))]

test <- test %>% mutate(incqnt = ntile(income, 4))
test$incqnt <- as.factor(test$incqnt)

test_lg <- melt(test, id.vars = c("id", "income", "incqnt"))
test_lg$rwtt <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg[test_lg$rwtt > 0,], aes(x = incqnt, y = value)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), col = 'blue', width = 0.75, size = 1, linetype = "solid") +
  facet_wrap(.~ rwtt) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "income quartile", y = "Changes in water bill (%)")

test <- df[,c("id", "urban" , grep("difab_rwtt", colnames(df), value = T))]

test_lg <- melt(test, id.vars = c("id", "urban"))
test_lg$rwtt <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_wrap(.~ rwtt) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "urbanization", y = "Changes in water bill (%)")


test <- df[,c("id", "urban" , grep("difpc_rwtt", colnames(df), value = T))]

test_lg <- melt(test, id.vars = c("id", "urban"))
test_lg$rwtt <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_wrap(.~ rwtt) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "urbanization", y = "Changes in water bill (%)")


# # 4. plots -----------
# 
# 
# 
# ## 4.1. by income per capita -----
# 
# 
# df <- df %>% mutate(ipcqnt = ntile(inceqa, 10))
# df$ipcqnt <- as.factor(df$ipcqnt)
# 
# 
# 
# df_sum <- df %>%
#   group_by(ipcqnt) %>%
#   summarise(count = n(),
#             avrbill = mean(bill_14, na.rm = T),
#             sdbill = sd(bill_14, na.rm = T),
#             rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
#             avrhhs = mean(hhs, na.rm = T),
#             sdhhs = sd(hhs, na.rm = T),
#             avrinc = mean(inceqa, na.rm = T),
#             sdinc = sd(income, na.rm = T),
#             avrurban = mean(urban, na.rm = T),
#             sdurban = sd(urban, na.rm = T),
#             avr_ln = mean(bill_ln),
#             dif_ln = mean(dif_ln),
#             avr_lnwf = mean(bill_lnwf),
#             dif_lnwf = mean(dif_lnwf),
#             avr_brx = mean(bill_brx),
#             dif_brx = mean(dif_brx))
# 
# 
# 
# df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill
# 
# df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill
# 
# df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill
# 
# 
# plotdf <- melt(df_sum[, c('ipcqnt', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])
# 
# 
# 
# ggplot(plotdf) +
#   geom_col(aes(x = ipcqnt, y = value, fill = variable),position = "dodge2") +
#   scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
#   theme_bw() +
#   labs( x = 'Income per equivalent adult quantiles', y = 'Changes in water bill (%)', fill = 'Tariff scheme')
# 
# 
# ggplot(df_sum, aes(x = ipcqnt, y = avrurban)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avrurban - sdurban, ymax = avrurban + sdurban)) +
#   theme_bw() +
#   labs(y = 'Urban type', x = 'Income per equivalent adult quantiles')
# 
# ggplot(df_sum, aes(x = ipcqnt, y = avrhhs)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
#   theme_bw() +
#   labs(y = 'Household size', x = 'Income per equivalent adult quantiles')
# 
# ggplot(df) +
#   geom_boxplot(aes(x = ipcqnt, y = bill_14)) +
#   theme_bw() +
#   labs(y = 'Water bill in 2014', x = 'Income per equivalent adult quantiles')
# 
# ggplot(df_sum, aes(x = ipcqnt, y = rwtank)) +
#   geom_col() +
#   theme_bw() +
#   labs(y = 'Proportion with rainwater tank', x = 'Income per equivalent adult quantiles')
# 
# 
# 
# ggplot(df) +
#   geom_boxplot(aes(x = ipcqnt, y = dif_ln)) +
#   theme_bw() +
#   labs(y = 'Difference in water bill', x = 'Income per equivalent adult quantiles')
# 
# ggplot(df_sum, aes(x = ipcqnt, y = dif_ln)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Income per equivalent adult quantiles') +
#   ylim(-20,20)
# 
# ggplot(df_sum, aes(x = ipcqnt, y = dif_ln_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Income per equivalent adult quantiles') +
#   ylim(-8,8)
# 
# 
# 
# 
# 
# ggplot(df_sum, aes(x = ipcqnt, y = dif_lnwf)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Income per equivalent adult quantiles') +
#   ylim(-20,20)
# 
# ggplot(df_sum, aes(x = ipcqnt, y = dif_lnwf_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Income per equivalent adult quantiles') +
#   ylim(-8,8)
# 
# 
# 
# ggplot(df_sum, aes(x = ipcqnt, y = dif_brx)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Income per equivalent adult quantiles') +
#   ylim(-35,35)
# 
# ggplot(df_sum, aes(x = ipcqnt, y = dif_brx_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Income per equivalent adult quantiles') +
#   ylim(-12,12)
# 
# 
# ## 4.2. by income -----
# 
# df <- df %>% mutate(incqnt = ntile(income, 10))
# df$incqnt <- as.factor(df$incqnt)
# 
# 
# 
# df_sum <- df %>%
#   group_by(incqnt) %>%
#   summarise(count = n(),
#             avrbill = mean(bill_14, na.rm = T),
#             sdbill = sd(bill_14, na.rm = T),
#             rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
#             avrhhs = mean(hhs, na.rm = T),
#             sdhhs = sd(hhs, na.rm = T),
#             avrinc = mean(inceqa, na.rm = T),
#             sdinc = sd(income, na.rm = T),
#             avrurban = mean(urban, na.rm = T),
#             sdurban = sd(urban, na.rm = T),
#             dif_ln = mean(dif_ln),
#             dif_lnwf = mean(dif_lnwf),
#             dif_brx = mean(dif_brx))
# 
# 
# 
# df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill
# 
# df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill
# 
# df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill
# 
# plotdf <- melt(df_sum[, c('incqnt', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])
# 
# 
# 
# ggplot(plotdf) +
#   geom_col(aes(x = incqnt, y = value, fill = variable),position = "dodge2") +
#   scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
#   theme_bw() +
#   labs( x = 'Household income quantiles', y = 'Changes in water bill (%)', fill = 'Tariff scheme')
# 
# 
# ggplot(df_sum, aes(x = incqnt, y = avrurban)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avrurban - sdurban, ymax = avrurban + sdurban)) +
#   theme_bw() +
#   labs(y = 'Urban type', x = 'Household income quantiles')
# 
# ggplot(df_sum, aes(x = incqnt, y = avrhhs)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
#   theme_bw() +
#   labs(y = 'Household size', x = 'Household income quantiles')
# 
# ggplot(df) +
#   geom_boxplot(aes(x = incqnt, y = bill_14)) +
#   theme_bw() +
#   labs(y = 'Water bill in 2014', x = 'Household income quantiles')
# 
# ggplot(df_sum, aes(x = incqnt, y = rwtank)) +
#   geom_col() +
#   theme_bw() +
#   labs(y = 'Proportion with rainwater tank', x = 'Household income quantiles')
# 
# 
# 
# ggplot(df) +
#   geom_boxplot(aes(x = incqnt, y = dif_ln)) +
#   theme_bw() +
#   labs(y = 'Difference in water bill', x = 'Household income quantiles')
# 
# ggplot(df_sum, aes(x = incqnt, y = dif_ln)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Household income quantiles') +
#   ylim(-20,20)
# 
# ggplot(df_sum, aes(x = incqnt, y = dif_ln_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Household income quantiles') +
#   ylim(-8,8)
# 
# 
# 
# 
# 
# ggplot(df_sum, aes(x = incqnt, y = dif_lnwf)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Household income quantiles') +
#   ylim(-35,35)
# 
# ggplot(df_sum, aes(x = incqnt, y = dif_lnwf_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Household income quantiles') +
#   ylim(-12,12)
# 
# 
# 
# ggplot(df_sum, aes(x = incqnt, y = dif_brx)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Household income quantiles') +
#   ylim(-35,35)
# 
# ggplot(df_sum, aes(x = incqnt, y = dif_brx_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Household income quantiles') +
#   ylim(-12,12)
# 
# ## 4.3. by income cat-----
# 
# 
# df$inccat <- factor(df$inccat, levels =lvls(df$inccat)[c(4,3,1,2)])
# 
# df <- df[!(is.na(df$inccat)),]
# 
# 
# df_sum <- df %>%
#   group_by(inccat) %>%
#   summarise(count = n(),
#             avrbill = mean(bill_14, na.rm = T),
#             sdbill = sd(bill_14, na.rm = T),
#             rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
#             avrhhs = mean(hhs, na.rm = T),
#             sdhhs = sd(hhs, na.rm = T),
#             avrinc = mean(inceqa, na.rm = T),
#             sdinc = sd(income, na.rm = T),
#             avrurban = mean(urban, na.rm = T),
#             sdurban = sd(urban, na.rm = T),
#             dif_ln = mean(dif_ln),
#             dif_lnwf = mean(dif_lnwf),
#             dif_brx = mean(dif_brx))
# 
# 
# 
# df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill
# 
# df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill
# 
# df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill
# 
# plotdf <- melt(df_sum[, c('inccat', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])
# 
# 
# 
# ggplot(plotdf) +
#   geom_col(aes(x = inccat, y = value, fill = variable),position = "dodge2") +
#   scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
#   theme_bw() +
#   labs( x ='Income categories', y = 'Changes in water bill (%)', fill = 'Tariff scheme')
# 
# ggplot(df_sum, aes(x = inccat, y = avrurban)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avrurban - sdurban, ymax = avrurban + sdurban)) +
#   theme_bw() +
#   labs(y = 'Urban type', x = 'Income categories')
# 
# ggplot(df_sum, aes(x = inccat, y = avrhhs)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
#   theme_bw() +
#   labs(y = 'Household size', x = 'Income categories')
# 
# ggplot(df) +
#   geom_boxplot(aes(x = inccat, y = bill_14)) +
#   theme_bw() +
#   labs(y = 'Water bill in 2014', x = 'Income categories')
# 
# ggplot(df_sum, aes(x = inccat, y = rwtank)) +
#   geom_col() +
#   theme_bw() +
#   labs(y = 'Proportion with rainwater tank', x = 'Income categories')
# 
# 
# 
# ggplot(df) +
#   geom_boxplot(aes(x = inccat, y = dif_ln)) +
#   theme_bw() +
#   labs(y = 'Difference in water bill', x = 'Income categories')
# 
# ggplot(df_sum, aes(x = inccat, y = dif_ln)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Income categories') +
#   ylim(-20,20)
# 
# ggplot(df_sum, aes(x = inccat, y = dif_ln_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Income categories') +
#   ylim(-8,8)
# 
# 
# 
# 
# 
# ggplot(df_sum, aes(x = inccat, y = dif_lnwf)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Income categories') +
#   ylim(-35,35)
# 
# ggplot(df_sum, aes(x = inccat, y = dif_lnwf_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Income categories') +
#   ylim(-12,12)
# 
# 
# 
# ggplot(df_sum, aes(x = inccat, y = dif_brx)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Income categories') +
#   ylim(-35,35)
# 
# ggplot(df_sum, aes(x = inccat, y = dif_brx_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Income categories') +
#   ylim(-12,12)
# 
# 
# ## 4.4. by urban -----
# 
# 
# 
# df_sum <- df %>%
#   group_by(urban) %>%
#   summarise(count = n(),
#             avrbill = mean(bill_14, na.rm = T),
#             sdbill = sd(bill_14, na.rm = T),
#             rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
#             avrhhs = mean(hhs, na.rm = T),
#             sdhhs = sd(hhs, na.rm = T),
#             avrinc = mean(inceqa, na.rm = T),
#             sdinc = sd(income, na.rm = T),
#             dif_ln = mean(dif_ln),
#             dif_lnwf = mean(dif_lnwf),
#             dif_brx = mean(dif_brx))
# 
# 
# 
# df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill
# 
# df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill
# 
# df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill
# 
# plotdf <- melt(df_sum[, c('urban', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])
# 
# 
# 
# ggplot(plotdf) +
#   geom_col(aes(x = urban, y = value, fill = variable),position = "dodge2") +
#   scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
#   theme_bw() +
#   labs( x ='Urban density', y = 'Changes in water bill (%)', fill = 'Tariff scheme')
# 
# ggplot(df_sum, aes(x = urban, y = avrhhs)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
#   theme_bw() +
#   labs(y = 'Household size', x = 'Urban density')
# 
# ggplot(df) +
#   geom_boxplot(aes(x = urban, y = bill_14)) +
#   theme_bw() +
#   labs(y = 'Water bill in 2014', x = 'Urban density')
# 
# ggplot(df_sum, aes(x = urban, y = rwtank)) +
#   geom_col() +
#   theme_bw() +
#   labs(y = 'Proportion with rainwater tank', x = 'Urban density')
# 
# 
# 
# ggplot(df) +
#   geom_boxplot(aes(x = urban, y = dif_ln)) +
#   theme_bw() +
#   labs(y = 'Difference in water bill', x = 'Urban density')
# 
# ggplot(df_sum, aes(x = urban, y = dif_ln)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Urban density') +
#   ylim(-20,20)
# 
# ggplot(df_sum, aes(x = urban, y = dif_ln_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Urban density') +
#   ylim(-8,8)
# 
# 
# 
# 
# 
# ggplot(df_sum, aes(x = urban, y = dif_lnwf)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Urban density') +
#   ylim(-35,35)
# 
# ggplot(df_sum, aes(x = urban, y = dif_lnwf_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Urban density') +
#   ylim(-12,12)
# 
# 
# 
# ggplot(df_sum, aes(x = urban, y = dif_brx)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (euro)', x = 'Urban density') +
#   ylim(-40,40)
# 
# ggplot(df_sum, aes(x = urban, y = dif_brx_prc)) +
#   geom_col()  +
#   theme_bw() +
#   labs(y = 'Difference in water bill (%)', x = 'Urban density') +
#   ylim(-18,18)



