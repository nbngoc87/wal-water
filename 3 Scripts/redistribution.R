# notes from last run----------------------



# 1. setup ---------


## 1.1. load functions -----------------

source('3 Scripts/general_functions.R')

loadpackage('dplyr')
loadpackage('ggplot2')
loadpackage('reshape2')
loadpackage('raster')
loadpackage('sf')

## 1.2 load data --------

### data folder
rdir <- '2 Data/1 Raw'
pdir <- '2 Data/2 Processed'

### survey data

surv14 <- read.csv(file = file.path(pdir, "UtiSurv_2014_AWalCEHD_Wal/Survey2014_obs_AquaWal_prd.csv"))

### price data

price <- read.csv(file = file.path(pdir, 'Water_price_AWal_Wal/water_price_Wal_12_17.csv'))


### location 
load(file= file.path(pdir, 'UtiSurv_2014_AWalCEHD_Wal/Addresses/surv14_coord_PICC.Rdata'))

### urbanization

urban_10 <- raster(file.path(pdir, "Urban_5cat_Ahmed_Wal/LU2010_5cls_x25.flt"))

crs(urban_10) <- st_crs(surv14_coords)

# 2. merge data ----------

surv14$year <- 2014

df <- dplyr::left_join(surv14, price)[, c('id', 'CVD', 'CVA', 'csmptv', 'inceqa', 'rwtank', 'hhs_0_19', 'hhs_20_95', 'income', 'inccat')]
df$hhs <- df$hhs_0_19 + df$hhs_20_95

df$csmptv[df$csmptv > 300] <- NA



urban <- cbind.data.frame(id = surv14_coords$id, urban = extract(urban_10, surv14_coords))

urban <- urban[urban$urban > -1,]

df <- inner_join(df, urban)
df$cspc <- df$csmptv/df$hhs

df <- df[!is.na(df$csmptv) & !is.na(df$inceqa) & !is.na(df$CVD),]

# 3. water bills ----
## 3.1. current -----

df$bill_14 <- 1.06*(20*df$CVD + 30*df$CVA) + 1.06*0.0125*df$csmptv + 1.06*0.5*30*df$CVD + 1.06*(df$csmptv - 30)*(df$CVD + df$CVA)

df$bill_14 <- ifelse(df$csmptv <=30, 1.06*(20*df$CVD + 30*df$CVA) + 1.06*0.0125*df$csmptv + 1.06*0.5*df$csmptv*df$CVD, df$bill_14)

## 3.2. current with different fixed  -----

cur_fixed <- sum(1.06*(20*df$CVD + 30*df$CVA)) 
mean_cur_fixed <- mean(1.06*(20*df$CVD + 30*df$CVA))
cur_FSA <- sum(1.06*0.0125*df$csmptv)
cur_sum <- sum(df$bill_14, na.rm = T)
cur_var <- cur_sum - cur_fixed - cur_FSA



for (alpha in seq (0, 2, 0.4)) {
  new_fixed <- sum(alpha * 1.06*(20*df$CVD + 30*df$CVA)) 
  new_var <- cur_sum - new_fixed - cur_FSA
  
  beta <- new_var/cur_var
  
  
  df$bill_new <- alpha*1.06*(20*df$CVD + 30*df$CVA) + 1.06*0.0125*df$csmptv + beta*(1.06*0.5*30*df$CVD + 1.06*(df$csmptv - 30)*(df$CVD + df$CVA))
  
  df$bill_new <- ifelse(df$csmptv <=30, alpha*(1.06*(20*df$CVD + 30*df$CVA)) + 1.06*0.0125*df$csmptv + beta*(1.06*0.5*df$csmptv*df$CVD), df$bill_new)
  
  df$diff <- df$bill_new - df$bill_14
  
  colnames(df)[ncol(df) - 1] <- paste("bill", alpha, "fixed", sep = "_")
  
  colnames(df)[ncol(df)] <- paste("dif", alpha, "fixed", sep = "_")

}



## 3.3. linear with and without fixed -----

unpr <- sum(df$bill_14)/sum(df$csmptv)

df$bill_ln <- df$csmptv*unpr

df$dif_ln <- df$bill_ln - df$bill_14

#### with fixed
fixed <- 1.06*(20*mean(df$CVD) + 30*mean(df$CVA))

unpr1 <- (sum(df$bill_14) - nrow(df)*fixed)/sum(df$csmptv)

df$bill_lnwf <- fixed + df$csmptv*unpr1


df$dif_lnwf <- df$bill_lnwf - df$bill_14



## 3.4. block tariffs per captia -----

# follow Brussels scheme



bl_2_1 <- 3.7696/2.115
bl_3_2 <- 5.5726/3.7696
bl_4_3 <- 8.1338/5.5726


bl1 <- 2.78
bl2 <- bl1*bl_2_1
bl3 <- bl2*bl_3_2
bl4 <- bl3*bl_4_3

df$bill_brx <- 25.23 + (15*bl1 + 15*bl2 + 30*bl3 + (df$cspc - 60)*bl4)*df$hhs

df$bill_brx <- ifelse(df$cspc <= 60 & df$cspc >30, 25.23 + (15*bl1 + 15*bl2 + (df$cspc - 30)*bl3)*df$hhs, df$bill_brx)

df$bill_brx <- ifelse(df$cspc <= 30 & df$cspc > 15, 25.23 + (15*bl1 + (df$cspc - 15)*bl2)*df$hhs, df$bill_brx)

df$bill_brx <- ifelse(df$cspc <=  15, 25.23 + (df$cspc*bl1)*df$hhs, df$bill_brx)

sum(df$bill_brx)
sum(df$bill_14)

df$dif_brx <- df$bill_brx - df$bill_14

## 3.5. rainwater tank tax ---------




# 4. plots -----------



## 4.1. by income per capita -----


df <- df %>% mutate(ipcqnt = ntile(inceqa, 10))
df$ipcqnt <- as.factor(df$ipcqnt)



df_sum <- df %>%
  group_by(ipcqnt) %>%
  summarise(count = n(),
            avrbill = mean(bill_14, na.rm = T),
            sdbill = sd(bill_14, na.rm = T),
            rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
            avrhhs = mean(hhs, na.rm = T),
            sdhhs = sd(hhs, na.rm = T),
            avrinc = mean(inceqa, na.rm = T),
            sdinc = sd(income, na.rm = T),
            avrurban = mean(urban, na.rm = T),
            sdurban = sd(urban, na.rm = T),
            avr_ln = mean(bill_ln),
            dif_ln = mean(dif_ln),
            avr_lnwf = mean(bill_lnwf),
            dif_lnwf = mean(dif_lnwf),
            avr_brx = mean(bill_brx),
            dif_brx = mean(dif_brx))



df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill

df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill

df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill


plotdf <- melt(df_sum[, c('ipcqnt', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])



ggplot(plotdf) +
  geom_col(aes(x = ipcqnt, y = value, fill = variable),position = "dodge2") +
  scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
  theme_bw() +
  labs( x = 'Income per equivalent adult quantiles', y = 'Changes in water bill (%)', fill = 'Tariff scheme')


ggplot(df_sum, aes(x = ipcqnt, y = avrurban)) +
  geom_point() +
  geom_errorbar(aes(ymin = avrurban - sdurban, ymax = avrurban + sdurban)) +
  theme_bw() +
  labs(y = 'Urban type', x = 'Income per equivalent adult quantiles')

ggplot(df_sum, aes(x = ipcqnt, y = avrhhs)) +
  geom_point() +
  geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
  theme_bw() +
  labs(y = 'Household size', x = 'Income per equivalent adult quantiles')

ggplot(df) +
  geom_boxplot(aes(x = ipcqnt, y = bill_14)) +
  theme_bw() +
  labs(y = 'Water bill in 2014', x = 'Income per equivalent adult quantiles')

ggplot(df_sum, aes(x = ipcqnt, y = rwtank)) +
  geom_col() +
  theme_bw() +
  labs(y = 'Proportion with rainwater tank', x = 'Income per equivalent adult quantiles')



ggplot(df) +
  geom_boxplot(aes(x = ipcqnt, y = dif_ln)) +
  theme_bw() +
  labs(y = 'Difference in water bill', x = 'Income per equivalent adult quantiles')

ggplot(df_sum, aes(x = ipcqnt, y = dif_ln)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Income per equivalent adult quantiles') +
  ylim(-20,20)

ggplot(df_sum, aes(x = ipcqnt, y = dif_ln_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Income per equivalent adult quantiles') +
  ylim(-8,8)





ggplot(df_sum, aes(x = ipcqnt, y = dif_lnwf)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Income per equivalent adult quantiles') +
  ylim(-20,20)

ggplot(df_sum, aes(x = ipcqnt, y = dif_lnwf_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Income per equivalent adult quantiles') +
  ylim(-8,8)



ggplot(df_sum, aes(x = ipcqnt, y = dif_brx)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Income per equivalent adult quantiles') +
  ylim(-35,35)

ggplot(df_sum, aes(x = ipcqnt, y = dif_brx_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Income per equivalent adult quantiles') +
  ylim(-12,12)


## 4.2. by income -----

df <- df %>% mutate(incqnt = ntile(income, 10))
df$incqnt <- as.factor(df$incqnt)



df_sum <- df %>%
  group_by(incqnt) %>%
  summarise(count = n(),
            avrbill = mean(bill_14, na.rm = T),
            sdbill = sd(bill_14, na.rm = T),
            rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
            avrhhs = mean(hhs, na.rm = T),
            sdhhs = sd(hhs, na.rm = T),
            avrinc = mean(inceqa, na.rm = T),
            sdinc = sd(income, na.rm = T),
            avrurban = mean(urban, na.rm = T),
            sdurban = sd(urban, na.rm = T),
            dif_ln = mean(dif_ln),
            dif_lnwf = mean(dif_lnwf),
            dif_brx = mean(dif_brx))



df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill

df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill

df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill

plotdf <- melt(df_sum[, c('incqnt', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])



ggplot(plotdf) +
  geom_col(aes(x = incqnt, y = value, fill = variable),position = "dodge2") +
  scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
  theme_bw() +
  labs( x = 'Household income quantiles', y = 'Changes in water bill (%)', fill = 'Tariff scheme')


ggplot(df_sum, aes(x = incqnt, y = avrurban)) +
  geom_point() +
  geom_errorbar(aes(ymin = avrurban - sdurban, ymax = avrurban + sdurban)) +
  theme_bw() +
  labs(y = 'Urban type', x = 'Household income quantiles')

ggplot(df_sum, aes(x = incqnt, y = avrhhs)) +
  geom_point() +
  geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
  theme_bw() +
  labs(y = 'Household size', x = 'Household income quantiles')

ggplot(df) +
  geom_boxplot(aes(x = incqnt, y = bill_14)) +
  theme_bw() +
  labs(y = 'Water bill in 2014', x = 'Household income quantiles')

ggplot(df_sum, aes(x = incqnt, y = rwtank)) +
  geom_col() +
  theme_bw() +
  labs(y = 'Proportion with rainwater tank', x = 'Household income quantiles')



ggplot(df) +
  geom_boxplot(aes(x = incqnt, y = dif_ln)) +
  theme_bw() +
  labs(y = 'Difference in water bill', x = 'Household income quantiles')

ggplot(df_sum, aes(x = incqnt, y = dif_ln)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Household income quantiles') +
  ylim(-20,20)

ggplot(df_sum, aes(x = incqnt, y = dif_ln_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Household income quantiles') +
  ylim(-8,8)





ggplot(df_sum, aes(x = incqnt, y = dif_lnwf)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Household income quantiles') +
  ylim(-35,35)

ggplot(df_sum, aes(x = incqnt, y = dif_lnwf_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Household income quantiles') +
  ylim(-12,12)



ggplot(df_sum, aes(x = incqnt, y = dif_brx)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Household income quantiles') +
  ylim(-35,35)

ggplot(df_sum, aes(x = incqnt, y = dif_brx_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Household income quantiles') +
  ylim(-12,12)

## 4.3. by income cat-----


df$inccat <- factor(df$inccat, levels =lvls(df$inccat)[c(4,3,1,2)])

df <- df[!(is.na(df$inccat)),]


df_sum <- df %>%
  group_by(inccat) %>%
  summarise(count = n(),
            avrbill = mean(bill_14, na.rm = T),
            sdbill = sd(bill_14, na.rm = T),
            rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
            avrhhs = mean(hhs, na.rm = T),
            sdhhs = sd(hhs, na.rm = T),
            avrinc = mean(inceqa, na.rm = T),
            sdinc = sd(income, na.rm = T),
            avrurban = mean(urban, na.rm = T),
            sdurban = sd(urban, na.rm = T),
            dif_ln = mean(dif_ln),
            dif_lnwf = mean(dif_lnwf),
            dif_brx = mean(dif_brx))



df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill

df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill

df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill

plotdf <- melt(df_sum[, c('inccat', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])



ggplot(plotdf) +
  geom_col(aes(x = inccat, y = value, fill = variable),position = "dodge2") +
  scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
  theme_bw() +
  labs( x ='Income categories', y = 'Changes in water bill (%)', fill = 'Tariff scheme')

ggplot(df_sum, aes(x = inccat, y = avrurban)) +
  geom_point() +
  geom_errorbar(aes(ymin = avrurban - sdurban, ymax = avrurban + sdurban)) +
  theme_bw() +
  labs(y = 'Urban type', x = 'Income categories')

ggplot(df_sum, aes(x = inccat, y = avrhhs)) +
  geom_point() +
  geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
  theme_bw() +
  labs(y = 'Household size', x = 'Income categories')

ggplot(df) +
  geom_boxplot(aes(x = inccat, y = bill_14)) +
  theme_bw() +
  labs(y = 'Water bill in 2014', x = 'Income categories')

ggplot(df_sum, aes(x = inccat, y = rwtank)) +
  geom_col() +
  theme_bw() +
  labs(y = 'Proportion with rainwater tank', x = 'Income categories')



ggplot(df) +
  geom_boxplot(aes(x = inccat, y = dif_ln)) +
  theme_bw() +
  labs(y = 'Difference in water bill', x = 'Income categories')

ggplot(df_sum, aes(x = inccat, y = dif_ln)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Income categories') +
  ylim(-20,20)

ggplot(df_sum, aes(x = inccat, y = dif_ln_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Income categories') +
  ylim(-8,8)





ggplot(df_sum, aes(x = inccat, y = dif_lnwf)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Income categories') +
  ylim(-35,35)

ggplot(df_sum, aes(x = inccat, y = dif_lnwf_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Income categories') +
  ylim(-12,12)



ggplot(df_sum, aes(x = inccat, y = dif_brx)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Income categories') +
  ylim(-35,35)

ggplot(df_sum, aes(x = inccat, y = dif_brx_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Income categories') +
  ylim(-12,12)


## 4.4. by urban -----


df$urban <- factor(df$urban)
df$urban <- car::recode(df$urban, "c('0', '1') = 'low'; c('2','3') = 'medium'; c('4','5') = 'high' ")

df$urban <- factor(df$urban, levels = lvls(df$urban)[c(2,3,1)])

df_sum <- df %>%
  group_by(urban) %>%
  summarise(count = n(),
            avrbill = mean(bill_14, na.rm = T),
            sdbill = sd(bill_14, na.rm = T),
            rwtank = sum(rwtank %in% 'yes', na.rm = T)*100/n(),
            avrhhs = mean(hhs, na.rm = T),
            sdhhs = sd(hhs, na.rm = T),
            avrinc = mean(inceqa, na.rm = T),
            sdinc = sd(income, na.rm = T),
            dif_ln = mean(dif_ln),
            dif_lnwf = mean(dif_lnwf),
            dif_brx = mean(dif_brx))



df_sum$dif_ln_prc <- df_sum$dif_ln*100/df_sum$avrbill

df_sum$dif_lnwf_prc <- df_sum$dif_lnwf*100/df_sum$avrbill

df_sum$dif_brx_prc <- df_sum$dif_brx*100/df_sum$avrbill

plotdf <- melt(df_sum[, c('urban', "dif_ln_prc", "dif_lnwf_prc", "dif_brx_prc")])



ggplot(plotdf) +
  geom_col(aes(x = urban, y = value, fill = variable),position = "dodge2") +
  scale_fill_discrete(labels = c('linear', 'linear with fixed', 'block per capita with fixed')) +
  theme_bw() +
  labs( x ='Urban density', y = 'Changes in water bill (%)', fill = 'Tariff scheme')

ggplot(df_sum, aes(x = urban, y = avrhhs)) +
  geom_point() +
  geom_errorbar(aes(ymin = avrhhs - sdhhs, ymax = avrhhs + sdhhs)) +
  theme_bw() +
  labs(y = 'Household size', x = 'Urban density')

ggplot(df) +
  geom_boxplot(aes(x = urban, y = bill_14)) +
  theme_bw() +
  labs(y = 'Water bill in 2014', x = 'Urban density')

ggplot(df_sum, aes(x = urban, y = rwtank)) +
  geom_col() +
  theme_bw() +
  labs(y = 'Proportion with rainwater tank', x = 'Urban density')



ggplot(df) +
  geom_boxplot(aes(x = urban, y = dif_ln)) +
  theme_bw() +
  labs(y = 'Difference in water bill', x = 'Urban density')

ggplot(df_sum, aes(x = urban, y = dif_ln)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Urban density') +
  ylim(-20,20)

ggplot(df_sum, aes(x = urban, y = dif_ln_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Urban density') +
  ylim(-8,8)





ggplot(df_sum, aes(x = urban, y = dif_lnwf)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Urban density') +
  ylim(-35,35)

ggplot(df_sum, aes(x = urban, y = dif_lnwf_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Urban density') +
  ylim(-12,12)



ggplot(df_sum, aes(x = urban, y = dif_brx)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (euro)', x = 'Urban density') +
  ylim(-40,40)

ggplot(df_sum, aes(x = urban, y = dif_brx_prc)) +
  geom_col()  +
  theme_bw() +
  labs(y = 'Difference in water bill (%)', x = 'Urban density') +
  ylim(-18,18)



