Redistribution effects of water tariffs
================
Nguyen Bich Ngoc,
02 December 2021

# Introduction

    ##    dtbtor   CVA
    ## 1    SWDE 1.745
    ## 2    CILE 1.745
    ## 11  IECBW 1.745

![](redistribution_files/figure-gfm/changefixed-1.png)<!-- -->![](redistribution_files/figure-gfm/changefixed-2.png)<!-- -->![](redistribution_files/figure-gfm/changefixed-3.png)<!-- -->![](redistribution_files/figure-gfm/changefixed-4.png)<!-- -->

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.0000  0.0000  0.3846  1.0000  1.0000

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   28.57  167.70  273.18  305.18  405.28 1630.66

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   102.5   169.1   272.4   305.2   396.0  1202.4

    ## [1] 468142.9

    ## [1] 468142.9

``` r
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
```

    ## Joining, by = c("id", "CVD", "CVA", "csmptv", "inceqa", "rwtank", "hhs_0_19", "hhs_20_95", "income", "inccat", "dtbtor", "hhs", "urban", "cspc", "ab30", "bill_cur", "bill_fixed_0", "difab_fixed_0", "difpc_fixed_0", "bill_fixed_40", "difab_fixed_40", "difpc_fixed_40", "bill_fixed_80", "difab_fixed_80", "difpc_fixed_80", "bill_fixed_120", "difab_fixed_120", "difpc_fixed_120", "bill_fixed_160", "difab_fixed_160", "difpc_fixed_160", "bill_fixed_200", "difab_fixed_200", "difpc_fixed_200", "bl2", "bl3", "bl4", "bill_brx")

``` r
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
```

    ## Warning: `fun.y` is deprecated. Use `fun` instead.

![](redistribution_files/figure-gfm/changerwtt-1.png)<!-- -->

``` r
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
```

    ## Warning: `fun.y` is deprecated. Use `fun` instead.

![](redistribution_files/figure-gfm/changerwtt-2.png)<!-- -->

``` r
test <- df[,c("id", "urban" , grep("difab_rwtt", colnames(df), value = T))]

test_lg <- melt(test, id.vars = c("id", "urban"))
test_lg$rwtt <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_wrap(.~ rwtt) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "urbanization", y = "Changes in water bill (%)")
```

![](redistribution_files/figure-gfm/changerwtt-3.png)<!-- -->

``` r
test <- df[,c("id", "urban" , grep("difpc_rwtt", colnames(df), value = T))]

test_lg <- melt(test, id.vars = c("id", "urban"))
test_lg$rwtt <- as.numeric(gsub(".*_", "", test_lg$variable))

ggplot(test_lg) +
  geom_boxplot(aes(x = urban, y = value)) +
  facet_wrap(.~ rwtt) +
  geom_hline(yintercept = 0, col = 'red', linetype = "longdash") +
  theme_bw() +
  labs(x = "urbanization", y = "Changes in water bill (%)")
```

![](redistribution_files/figure-gfm/changerwtt-4.png)<!-- -->

``` r
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
```
