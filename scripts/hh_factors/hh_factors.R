#' ---
#' title: "Household level determinants"
#' author: "Nguyen Bich Ngoc, Cedric Prevedello, Mario Cools, Jacques Teller"
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

pwp_f <- function(x, y) {
  tryCatch({
    f <- summary(lm(x ~ y))$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail = F)
  }, warning = function(w) {
    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

hhsize <-
  function(year = 2014,
           birth_year = by_df,
           occupancy = ocp_df,
           age_brks = c(0, 20, 65),
           ocp = T) {
    
    age <- year - birth_year
    age[age < 0] <- NA
    
    data <- data.frame(year = rep(year, nrow(age)))
    age_brks <- c(age_brks, max(age, na.rm = T) + 1)
    n <- length(age_brks)
    
    if (ocp) {
      for (i in seq_len(n - 1)) {
        agr_mat <- age >= age_brks[i] & age < age_brks[i + 1]
        npers <- apply(agr_mat * occupancy, 1, sum, na.rm = T)
        data <- cbind(data, npers)
        colnames(data)[ncol(data)] <-
          paste("hhspo", age_brks[i], age_brks[i + 1] - 1, sep = "_")
      }
      data$hhspo_tot <-
        apply(data[, grep("hhspo_", colnames(data))], 1, sum, na.rm = T)
      data[data$hhspo_tot %in% 0, grep("hhspo_", colnames(data))] <-
        NA
      
    } else {
      for (i in seq_len(n - 1)) {
        agr_mat <- age >= age_brks[i] & age < age_brks[i + 1]
        npers <- apply(agr_mat, 1, sum, na.rm = T)
        data <- cbind(data, npers)
        colnames(data)[ncol(data)] <-
          paste("hhs", age_brks[i], age_brks[i + 1] - 1, sep = "_")
      }
      data$hhs_tot <-
        apply(data[, grep("hhs_", colnames(data))], 1, sum, na.rm = T)
      data[data$hhs_tot %in% 0, grep("hhs_", colnames(data))] <- NA
      
    }
    data
  }

cvlm_f <- function(formula, data, seed = 1, fold =10) {
  set.seed(seed)
  v <- sample(1:fold, nrow(data), replace=T)
  RMSE <- rep(NA, fold)
  for(i in 1:fold) {
    train <- data[v != i,]
    vald <- data[v == i,]
    fit <- lm(formula, data = train, weights = weight, na.action = 'na.exclude')
    predict <- predict(fit, newdata = vald, weights = weight)
    RMSE [i] <- sqrt(mean((predict - vald$csmptv)^2, na.rm = T))
  }
  RMSE
}

### packages ----------


loadpackage("here")
loadpackage("ggplot2")
loadpackage("scico")
loadpackage("Hmisc")
loadpackage("knitr")
loadpackage("dplyr")
loadpackage("ggcorrplot")
loadpackage("reshape2")
loadpackage("cowplot")
loadpackage("sf")
loadpackage("spdep")
loadpackage("ggspatial")
loadpackage("raster")
loadpackage("exactextractr")
loadpackage("car")
loadpackage("MASS")
loadpackage("robustbase")

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

### built-up densities 2k5 -------------------

bltup2k5 <- raster(here(rdir, "urban_2k5_Ahmed_Wal", "cat2010.flt"))

bltup2k5[bltup2k5 == -9999] <- NA

crs(bltup2k5) <- st_crs(muni)$proj4string


# 2. data processing --------------------

## 2.1. survey data -----------

### derived living area -----------

us$livapc <- us$livara/us$hhs_tot

us$livasc <- as.numeric(scale(us$livara))

### built-up density 5 cat -----------------


us$pointsp <- factor(us$pointsp)
us$buf1k_mode <- factor(us$buf1k_mode)
us$buf300_mode <- factor(us$buf300_mode)
us$buf1k_max <- factor(us$buf1k_max)
us$buf300_max <- factor(us$buf300_max)

us$pointsp_rlv <- car::recode(us$pointsp, "c('0', '1') = 'low'; c('2', '3') = 'medium'; c('4', '5') = 'high'")

us$pointsp_rlv <- factor(us$pointsp_rlv, lvls(us$pointsp_rlv)[c(2, 3, 1)])

### built-up density 2k5 ---------

us_coord$bltup2k5 <- extract(bltup2k5, us_coord)
us_coord$id <- as.integer(us_coord$id)

us <- left_join(us, st_drop_geometry(us_coord[, c("id", "bltup2k5")]))

### relevel some factors ------------------


us$nbktch <- car::recode(us$nbktch, "c('2', '3 or more') = '2 or more'")

us$dtbtor <- factor(us$dtbtor, lvls(us$dtbtor)[c(4, 1, 3, 2, 5)])

us$dfpay[us$dfpay %in% "not user"] <- NA

 
### household size --------------

us$hhspo_18_95[us$hhs_18_95 < 1 & !is.na(us$hhs_tot)] <- NA
us$hhs_18_95[us$hhs_18_95 < 1 & !is.na(us$hhs_tot)] <- NA

us$hhs_tot <- us$hhs_18_95 + us$hhs_0_17
us$hhspo_tot <- us$hhspo_18_95 + us$hhspo_0_17

us$nadtrc <- us$hhs_18_95 - 1

### parcel & built area -----------

hist(us$bldarea[us$bldarea < 800])

us$bldarea[us$bldarea > 800] <- NA

us$noblar <- us$parcarea - us$bldarea

## 2.2. municipality level ----------
### aggregating survey data ------------

us_mun <- us %>%
  group_by(municd) %>%
  summarise(count = n(),
            csmptv = mean(csmptv, na.rm = T),
            rwt_prop = sum(rwusbn %in% "yes")/n(),
            gard_prop = sum(garden %in% "yes")/n(),
            inceqa = mean(inceqa, na.rm = T),
            hhs_tot = mean(hhs_tot, na.rm = T))

mun_wal <- left_join(mun_wal, us_mun)

### built-up 2k5 ----------------


mun_wal$bltup2k5 <- exact_extract(bltup2k5, mun_wal, fun = "mean", weights = "area")

# 3. descriptive statistics ------------

vartype <- unlist(sapply(us, class))

itvar <- us[, vartype %in% c("integer", "numeric")]
ftvar <- us[, vartype %in% c("character", "factor")]

## 3.1. quantitative variables ------------

#+ itsum, echo = F, message = F, results = "asis"

itsum <- data.frame(variable = colnames(itvar))

itsum$mean <- apply(itvar, 2, function(x) wtd.mean(x, na.rm = T, us$weight))
itsum$sd <- apply(itvar, 2, function(x) sqrt(wtd.var(x, na.rm = T, us$weight)))
itsum$min <- apply(itvar, 2, function(x) min(x, na.rm = T))
itsum$max <- apply(itvar, 2, function(x) max(x, na.rm = T))
itsum$missing <- apply(itvar, 2, function(x) sum(is.na(x)))

cat("\n")
kable(itsum)
cat("\n")

## 3.2. qualitative variables ---------


#+ ftstab, echo = F, message = F, results = "asis"

ftsum <- data.frame(vars = character(0), levels = character(0), count = integer(), freq = numeric(0))
for (i in 1: length(ftvar)) {
  n <- nlvls(ftvar[,i])
  if (n < 20) {
    vars <- rep(colnames(ftvar)[i],n + 1)
    levels <- c(lvls(ftvar[,i]), "missing")
    count <- c(as.data.frame(table(droplevels(as.factor(ftvar[,i]))))[,2], sum(is.na(ftvar[,i])))
    freq <- count*100/sum(count)
    temp <- cbind.data.frame(vars, levels, count, freq)
    ftsum <- rbind.data.frame(ftsum, temp)
    ftsum <- ftsum[ftsum$count !=0, ]
  }
}

cat("\n")
kable(ftsum)
cat("\n")


## 3.3. correlation matrix ------

#+ crmfpl, echo = F, message = F, fig.width = fig_d2, fig.height = fig_d1


cormatfull <- round(cor(itvar[, -(1:3)], use = "pairwise.complete.obs"),2)

ggcorrplot(cormatfull, insig = "blank", ggtheme = theme_kat(legend.position = "right"), tl.cex = 4, colors = c(scico(1, palette = pal_div, begin = pal_end), "white", scico(1, palette = pal_div, begin = pal_bg)))

#+ cormatpl, echo = F, message = F, fig.width = fig_d2, fig.height = fig_d1

corvar <- us[, c("csmptv",  "hhspo_18_95", "hhspo_0_17", "rpage", "inceqa",  "livara", "pointsp", "buf300_mean", "buf300_mode", "buf1k_mean", "buf1k_mode", "bltup2k5", "buf300_max")]

corvar$pointsp <- as.numeric(corvar$pointsp)
corvar$buf300_mode <- as.numeric(corvar$buf300_mode)
corvar$buf1k_mode <- as.numeric(corvar$buf1k_mode)
corvar$buf300_max <- as.numeric(corvar$buf300_max)
colnames(corvar)[1:7] <- c("consumption",  "# adults", "# children", "reference person age", "income per capita",  "living area", "built-up density")

cormat <- round(cor(corvar, use = "complete.obs"),2)

ggcorrplot(cormat, lab = T, insig = "blank", ggtheme = theme_kat(legend.position = "right"), tl.cex = 6, lab_size = 2, colors = c(scico(1, palette = pal_div, begin = pal_end), "white", scico(1, palette = pal_div, begin = pal_bg)))

## 3.4. pairwise p ------------------

#+ pwptab, echo = F, message = F, results = "asis"

pwp_v <- apply(us[, -(1:6)], 2, pwp_f, x = us$csmptv)

pwp_df <- data.frame(variable = names(pwp_v), pvalue = pwp_v)

pwp_df <- pwp_df[order(pwp_df$pvalue),]


cat("\n")
kable(pwp_df, row.names = F)
cat("\n")

## 3.5. other water sources -------------

#+ wsstat

prop.table(table(us$rwusbn))

prop.table(table(us$pvwlbn))

#+ rwupl, echo = F, message = F, fig.width = fig_d1, fig.height = fig_d1


rwuse <- us[, grep("uos_.*_rain", colnames(us))]
rwuse <- melt(rwuse, id.vars = NULL)

rwuse$variable <- gsub("uos_", "", gsub("_rain", "", rwuse$variable))

plotdf <- data.frame(prop.table(table(rwuse$variable, rwuse$value), 1))

plotdf$Freq <- plotdf$Freq * 100
plotdf$Var1 <- factor(plotdf$Var1, levels = c("drink", "coffee", "meal", "dishes", "hyg", "washing", "idclean", "WC", "garden", "odclean", "car", "pool"))

plotdf %>%
  filter(Var2 == "yes") %>%
  ggplot() +
  geom_col(aes(x = Var1, y = Freq), fill = col1_light) +
  theme_kat() +
  labs(x = "Purposes", y = "Percentage of households") +
  scale_x_discrete(labels = c("meal" = "meal preparation", "dishes" = "dish washing", "hyg" = "hygiene", "washing" = "laundry", "idclean" = "interior cleaning", "WC" = "toilet", "odclean" = "exterior cleaning")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

## 3.6. builtup density ----------------------

#+ csdspl, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d3

p1 <- us %>%
  filter(!(is.na(pointsp))) %>%
  ggplot(aes(x = pointsp, y = csmptv)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(us$csmptv, c(0.1, 0.9), na.rm = T)) +
  theme_kat() +
  labs(x = "Built-up density categories (100x100)", y = expression(Household~water~consumption~(m^3/year)))

p2 <- us %>%
  filter(!(is.na(buf300_mode))) %>%
  ggplot(aes(x = buf300_mode, y = csmptv)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(us$csmptv, c(0.1, 0.9), na.rm = T)) +
  theme_kat() +
  labs(x = "Built-up density categories (300x300 mode)", y = expression(Household~water~consumption~(m^3/year)))

p3 <- us %>%
  filter(!(is.na(buf1k_mode))) %>%
  ggplot(aes(x = buf1k_mode, y = csmptv)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(us$csmptv, c(0.1, 0.9), na.rm = T)) +
  theme_kat() +
  labs(x = "Built-up density categories (1kmx1km mode)", y = expression(Household~water~consumption~(m^3/year)))

p4 <-
  us %>%
  filter(!(is.na(buf300_mean))) %>%
  ggplot(aes(x = buf300_mean, y = csmptv)) +
  geom_point() +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = quantile(us$csmptv, c(0.1, 0.9), na.rm = T)) +
  theme_kat() +
  labs(x = "Built-up density categories (300x300 mean)", y = expression(Household~water~consumption~(m^3/year)))

p5 <-
  us %>%
  filter(!(is.na(bltup2k5))) %>%
  ggplot(aes(x = bltup2k5, y = csmptv)) +
  geom_point() +
  geom_smooth(col = "red") +
  scale_y_continuous(limits = quantile(us$csmptv, c(0.1, 0.9), na.rm = T)) +
  theme_kat() +
  labs(x = "Built-up density 2k5 (100x100 mean)", y = expression(Household~water~consumption~(m^3/year)))



p6 <- us %>%
  filter(!(is.na(pointsp_rlv))) %>%
  ggplot(aes(x = pointsp_rlv, y = csmptv)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(us$csmptv, c(0.1, 0.9), na.rm = T)) +
  theme_kat() +
  labs(x = "Built-up density categories (100x100)", y = expression(Household~water~consumption~(m^3/year)))

p7 <- us %>%
  filter(!(is.na(pointsp_rlv))) %>%
  ggplot(aes(x = pointsp_rlv, y = cspppd)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(us$csmptv, c(0.1, 0.9), na.rm = T)) +
  theme_kat() +
  labs(x = "Built-up density categories (100x100)", y = expression(Household~water~consumption~(m^3/year)))

plot_grid(p1, p2, p3, p4, p5, p6, p7, nrow = 4)



## 3.7. Moran I and maps ---------

#+ mriprp

mun_coord <- st_coordinates(st_centroid(mun_wal))

knn <- knn2nb(knearneigh(mun_coord, k = 1), sym = T)
max_dist <- max(unlist(nbdists(knn, mun_coord)))
nb_dist <- dnearneigh(mun_coord, 0, (max_dist + 0.01), row.names = mun_wal$municd)
w_dist <- nb2listw(nb_dist, style = "W")

moran.test(x = mun_wal$csmptv, listw = w_dist, na.action = na.exclude, zero.policy = T)

cor.test(mun_wal$bltup2k5, mun_wal$rwt_prop, method = "spearman")
cor.test(mun_wal$bltup2k5, mun_wal$gard_prop, method = "spearman")

moran.test(x = mun_wal$rwt_prop, listw = w_dist, na.action = na.exclude, zero.policy = T)

moran.test(x = mun_wal$inceqa, listw = w_dist, na.action = na.exclude, zero.policy = T)
moran.test(x = mun_wal$hhs_tot, listw = w_dist, na.action = na.exclude, zero.policy = T)


#+ csmnmap, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d2 

mun_wal$csm_cat <- cut(mun_wal$csmptv, breaks = c(0, 55, 65, 75, 85, 300), right = F)

ggplot(data = neighbor) +
  geom_sf(fill = c("white", rep("grey90", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  geom_sf(data = mun_wal, aes(fill = csm_cat)) +
  scale_fill_scico_d(palette = pal_div, begin = pal_bg, end = pal_end, direction = -1, labels = c("[0,55)", "[55,65)", "[65,75)", "[75,85)", "[85,300)", "No data"), na.value = "white") +
  geom_sf(data = maincity) +
  geom_sf_text(data = maincity, aes(label = muninm), fontface = "bold", size = 3, nudge_y = -2500) +
  annotation_scale(location = "bl", width_hint = 0.15, pad_x = unit(2.3, "in"), pad_y = unit(0.5, "in")) +
  annotation_north_arrow(location = "bl", height = unit(0.5, "in"), width = unit(0.5, "in"), which_north = "true", pad_x = unit(2.65, "in"), pad_y = unit(0.7, "in"), style = north_arrow_fancy_orienteering) +
  theme_void() +
  labs(fill = expression(Comsumption~(m^3))) +
  theme(legend.position = c(0.18,0.25), plot.margin = unit(rep(-0.9,4), "cm"))


## 3.8. other stats -----------------

#+ othstat


cat("percentage of single member family")

sum(us$hhs_tot %in% 1, na.rm = T)*100/sum(!is.na(us$hhs_tot))


cat("percentage of families with child(ren)")

sum(us$hhs_0_17 > 0 & us$hhs_18_95 > 1, na.rm = T)*100/sum(!is.na(us$hhs_tot))

cat("percentage of couples without children")

sum(us$hhs_0_17 %in% 0 & us$hhs_18_95 %in% 2, na.rm = T)*100/sum(!is.na(us$hhs_tot))

cat("use distribution water for garden")

prop.table(table(us$garden))

cat("presence of washing machine")

prop.table(table(us$wasmch))

cat("presence of dishwashing")

prop.table(table(us$dshwas))

cat("(a) dual-flush or low-flush toilet(s)")

prop.table(table(us$eftoil))

cat("low flow shower head")

prop.table(table(us$efshhd))

cat("confident")

prop.table(table(us$cfdiwq))

cat("pay per volume")

prop.table(table(us$ppusvl))


# 4. baseline model ---------------

## 4.1. fitting general -------------

### prepare data --------

#+ fitprp, include = F, message = F


us$csmptv[us$csmptv > 300 | us$csmptv < 5] <- NA

### null model ------------

#+ null
rmse_null <- cvlm_f(csmptv ~ 1,
           data = us, fold = 100, seed = mfn)

mean(rmse_null)


### full model ---------------

#+ full

full <-
  lm(
    csmptv ~ avrprc + nadtrc + inceqa + parcarea + hhs_0_17 + nbbdrm + livapc + rwtuse + otsrid + rpjob + dshw3c + by_p1 + nbbtrm + dwowsh + pldisw + by_p2 + nbtoil + bthshw + buf300_max + rpgen + dfpay  + garden + prvncd + dwltyp + wasm3c + otsrod + rped4c + dwcsy9 + bldarea + efterp + eftech + nblvrm + ppusvl + dwelocp + btshrp + pvwlid + bdgmtr + hlpwtk + fseaid + nbktch + cfdiwq,
    data = us,
    na.action = na.exclude,
    weights = weight
  )

anova(full)

rmse_full <- cvlm_f(csmptv ~ avrprc + nadtrc + inceqa + parcarea + hhs_0_17 + nbbdrm + livapc + rwtuse + otsrid + rpjob + dshw3c + by_p1 + nbbtrm + dwowsh + pldisw + by_p2 + nbtoil + bthshw + buf300_max + rpgen   + garden + prvncd + dwltyp + wasm3c + otsrod + rped4c + dwcsy9 + bldarea + efterp + eftech + nblvrm + ppusvl + dwelocp + btshrp + pvwlid  + hlpwtk + fseaid + nbktch + cfdiwq + dfpay, data = us, fold = 100, seed = mfn)

mean(rmse_full)

### partial residual plots ---------

#+ partial, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d3

crPlots(full, ask = F, main = NULL)

## 4.2. consider each predictor group ---------

#+ meanrmse, include = F, message = F


### hhs -------------

rmse_01a <- cvlm_f(csmptv ~ hhs_0_17 + hhs_18_65 + hhs_66_95, data = us, fold = 100, seed = mfn)


rmse_01b <- cvlm_f(csmptv ~ hhspo_0_17 + hhspo_18_65 + hhspo_66_95, data = us, fold = 100, seed = mfn)


rmse_01c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc, data = us, fold = 100, seed = mfn)


by_df <- us[,grep("by_", colnames(us))]
ocp_df <- us[,grep("ocp_", colnames(us))]

hhswteen <- hhsize(year = 2014,
                   birth_year = by_df,
                   occupancy = ocp_df,
                   age_brks = c(0, 12, 20, 66),
                   ocp = F)

us1d <- cbind.data.frame(csmptv = us$csmptv, weight = us$weight, hhswteen)

rmse_01d <- cvlm_f(csmptv ~ hhs_0_11 + hhs_12_19 + hhs_20_65 + hhs_66_95, data = us1d, fold = 100, seed = mfn)

fit1d <- lm(csmptv ~ hhs_0_11 + hhs_12_19 + hhs_20_65 + hhs_66_95,
           data = us1d,
           na.action = na.exclude,
           weights = weight)

summary(fit1d)

hhsco20 <- hhsize(year = 2014,
                   birth_year = by_df,
                   occupancy = ocp_df,
                   age_brks = c(0, 20),
                   ocp = F)

us1e <- cbind.data.frame(csmptv = us$csmptv, weight = us$weight, hhsco20)

rmse_01e <- cvlm_f(csmptv ~ hhs_0_19 + hhs_20_95, data = us1e, fold = 100, seed = mfn)


hhsco14 <- hhsize(year = 2014,
                  birth_year = by_df,
                  occupancy = ocp_df,
                  age_brks = c(0, 14),
                  ocp = F)

us1f <- cbind.data.frame(csmptv = us$csmptv, weight = us$weight, hhsco14)

rmse_01f <- cvlm_f(csmptv ~ hhs_0_13 + hhs_14_95, data = us1f, fold = 100, seed = mfn)

hhsevr5 <- hhsize(year = 2014,
                  birth_year = by_df,
                  occupancy = ocp_df,
                  age_brks = seq(0,90,5),
                  ocp = F)

us1g <- cbind.data.frame(csmptv = us$csmptv, weight = us$weight, hhsevr5[,-c(1,21)])

rmse_01g <- cvlm_f(csmptv ~ ., data = us1g, fold = 100, seed = mfn)


fit1g <- lm(csmptv ~  .,
            data = us1g,
            na.action = na.exclude,
            weights = weight)

summary(fit1g)

## CONCLUSION: All of them perform more or less the same. Select 1c since it makes my life easier & the difference in water consumption by pensioner and working person is not that much

### income -------------------

rmse_02a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + income, data = us, fold = 100, seed = mfn)

rmse_02b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inceqa, data = us, fold = 100, seed = mfn)

rmse_02c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat, data = us, fold = 100, seed = mfn)

rmse_02d <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + iceqac1, data = us, fold = 100, seed = mfn)

rmse_02e <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + iceqac2, data = us, fold = 100, seed = mfn)

## CONCLUSION: income categories seems to have similar predictive value, but much more easier to explain

### other source of water -------------

rmse_03a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtank, data = us, fold = 100, seed = mfn)

rmse_03b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + otsrid, data = us, fold = 100, seed = mfn)

rmse_03c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat  + otsrod, data = us, fold = 100, seed = mfn)

rmse_03d <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + otsrid + otsrod, data = us, fold = 100, seed = mfn)

rmse_03e <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse, data = us, fold = 100, seed = mfn)

rmse_03f <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + prvwel, data = us, fold = 100, seed = mfn)

## CONCLUSION: variables which separate indoor/outdoor uses improve model much better. There's not much different among models with only rainwater, rainwater + well, or all alternative sources => select model with rainwater use

### living area  -----------------

rmse_04a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc, data = us, fold = 100, seed = mfn)

fit4a <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc, data = us, na.action = na.exclude, weights = weight)

summary(fit4a)
crPlots(fit4a)

rmse_04b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livapc, data = us, fold = 100, seed = mfn)

rmse_04c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + lareqa, data = us, fold = 100, seed = mfn)

rmse_04d <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + bldarea, data = us, fold = 100, seed = mfn)

fit4d <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + bldarea, data = us, na.action = na.exclude, weights = weight)

summary(fit4d)
crPlots(fit4d)

rmse_04e <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + parcarea, data = us, fold = 100, seed = mfn)

fit4e <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + parcarea, data = us, na.action = na.exclude, weights = weight)

summary(fit4e)
crPlots(fit4e)

rmse_04f <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + noblar, data = us, fold = 100, seed = mfn)

## CONCLUSION: RMSE are almost the same. choose livasc since it's easier to explain and have significant F-test




### price -----------------

rmse_05a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + avrprc, data = us, fold = 100, seed = mfn)

fit5a <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + avrprc, data = us, na.action = na.exclude, weights = weight)

summary(fit5a)


rmse_05b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + mgnprc, data = us, fold = 100, seed = mfn)

fit5b <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + mgnprc, data = us, na.action = na.exclude, weights = weight)

summary(fit5b)

rmse_05c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + bill70, data = us, fold = 100, seed = mfn)

fit5c <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + bill70, data = us, na.action = na.exclude, weights = weight)

summary(fit5c)


## CONCLUSION: marginal price is significant and improve model quite a lot, but can't say this is effect of price or effect of region/dtbtor? Can only check this using time series data in the next part of project. If marginal price still important then calculate price elastic for the redistributive part will be a good idea. Actually both avrprc and mgnprc highly correlate with water consumption due to the IBT. Then to actual measure the effect of price bill70 or both block price should be used??? read Lavin 2017 for more information?

### nb rooms ----------------------

rmse_06a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + nbbdrm, data = us, fold = 100, seed = mfn)

rmse_06b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + nbbtrm, data = us, fold = 100, seed = mfn)

rmse_06c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + nbtoil, data = us, fold = 100, seed = mfn)

rmse_06d <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + nbktch, data = us, fold = 100, seed = mfn)

rmse_06e <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + nblvrm, data = us, fold = 100, seed = mfn)

## CONCLUSION: all of them increase average RMSE

### refperson --------------------

rmse_07a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + rpage + rped4c + rpgen + rpjob, data = us, fold = 100, seed = mfn)

rmse_07b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + by_p1 + by_p2 + rped4c + rpgen + rpjob, data = us, fold = 100, seed = mfn)

## CONCLUSION: refperson info doesn't improve predictive power. Also, since oldest person was chosen as the refperson, there's no strong reason for their characteristics influence water consumption of the whole household.

### tech ----------


rmse_08a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dshw3c , data = us, fold = 100, seed = mfn)

rmse_08b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + aoi, data = us, fold = 100, seed = mfn)

rmse_08c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + wasm3c, data = us, fold = 100, seed = mfn)

rmse_08d <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + eftech + efterp , data = us, fold = 100, seed = mfn)

## CONCLUSION: nothing improve RMSE, nothing significant

### other dwelling properties ---------

rmse_09a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh, data = us, fold = 100, seed = mfn)

fit9a <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh, data = us, na.action = na.exclude, weights = weight)

anova(fit9a)


rmse_09b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh  + dwltyp, data = us, fold = 100, seed = mfn)

summary(lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + dwltyp, data = us, na.action = na.exclude, weights = weight))


rmse_09c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + dwcsy9, data = us, fold = 100, seed = mfn)

summary(lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + dwcsy9, data = us, na.action = na.exclude, weights = weight))


## CONCLUSION: dwowsh significant and slightly improve model => considering keeping it. Explainaition, if looking at the esitmate, big difference happen in renter-social/public housing => higher consumption might be explained by worse dwelling situation, potential of leak, lower possibility to invest in efficient tech. Did check contigency table of dwowsh with some variables and there're acutally big differences between renter and owner in almost all aspects.

### pool --------------------

rmse_10a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol, data = us, fold = 100, seed = mfn)

fit10a <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol, data = us, na.action = na.exclude, weights = weight)

anova(fit10a)

rmse_10b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pldisw, data = us, fold = 100, seed = mfn)

fit10b <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pldisw, data = us, na.action = na.exclude, weights = weight)

anova(fit10b)
summary(fit10b)


rmse_10c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol + tmppol, data = us, fold = 100, seed = mfn)

fit10c <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol + tmppol , data = us, na.action = na.exclude, weights = weight)

anova(fit10c)
summary(fit10c)

table(paste(us$pmnpol, us$tmppol))
### garden -------------

rmse_11a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol + garden, data = us, fold = 100, seed = mfn)

fit11a <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol+ garden, data = us, na.action = na.exclude, weights = weight)

anova(fit11a)
summary(fit11a)

### bath/shower --------------

rmse_12a <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol + garden + bthshw + btshrp, data = us, fold = 100, seed = mfn)

fit12a <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol+ garden  + bthshw + btshrp, data = us, na.action = na.exclude, weights = weight)

anova(fit12a)
summary(fit12a)

rmse_12b <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol + garden + bath, data = us, fold = 100, seed = mfn)

fit12b <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol+ garden + bath, data = us, na.action = na.exclude, weights = weight)

anova(fit12b)
summary(fit12b)

rmse_12c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol + garden + bath3c, data = us, fold = 100, seed = mfn)


### all others ----------

rmse_13a <- rmse_12c <- cvlm_f(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol + garden + bath + dfpay + ppusvl + hlpwtk + fseaid + cfdiwq, data = us, fold = 100, seed = mfn)

fit13a <- lm(csmptv ~ hhs_0_17 + nadtrc + inccat + rwtuse + livasc + dwowsh + pmnpol+ garden + bath + dfpay + ppusvl + hlpwtk + fseaid + cfdiwq, data = us, na.action = na.exclude, weights = weight)

## CONCLUSION although dfpay both significant and improve RMSE but it has no prediction power since we don't know if any family will have difficulty or not. also more importantly if there a causal relationship it should be the other way around, high consumption lead to difficult to pay, not the other way around. Have checked, and actually the family have difficulties in paying bill are mainly big and poor family

### reconsider parcel area --------

## 4.3. RMSE ----------------


rmse <- cbind.data.frame(mget(ls(pattern = "rmse_")))

rmselg <- melt(rmse, id.vars = NULL)

#+ rmse1, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

ggplot(rmselg, aes(x = variable, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean", col = "red") +
  scale_y_continuous(limits = quantile(rmselg$value, c(0.25, 0.75), na.rm = T)) + 
  theme_kat()

#+ rmse2, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

rmselg %>%
  filter(!(variable %in% c("rmse_null", "rmse_full"))) %>% 
  ggplot(aes(x = variable, y = value)) +
  stat_summary(geom = "point", fun = "mean", size = 8) + 
  stat_summary(data = rmselg[rmselg$variable %in% "rmse_12b",], aes(x = variable, y = value), geom = "point", fun = "mean", size = 8, col = "red")+
  theme_kat()

## 4.4. model diagnostic ------------

#+ base_model

base_model <- fit12b <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol, data = us, na.action = na.exclude, weights = weight)
plot(base_model, ask = F)
summary(base_model)
anova(base_model)

base_coef <- as.data.frame(summary(base_model)$coefficients)
base_coef$terms <- row.names(base_coef)

base_res <- cbind.data.frame(ind = seq_len(nrow(us)),id = us$id, csmptv = us$csmptv, stdres = stdres(base_model), studres = studres(base_model), pred = predict(base_model))

ggplot(base_res, aes(x = ind, y = stdres)) +
  geom_point() +
  geom_text(aes(label = ifelse(stdres > 2.5 | stdres < -2.5, as.character(id), "")), hjust = -0.1) +
  geom_hline(yintercept = c(-2.5, 2.5), col = "red") + theme_kat() +
  labs(x = "Index", y = "Standardized residuals")

ggplot(base_res, aes(x = ind, y = studres)) +
  geom_point() +
  geom_text(aes(label = ifelse(studres > 2.5 | studres < -2.5, as.character(id), "")), hjust = -0.1) +
  geom_hline(yintercept = c(-2.5, 2.5), col = "red") + theme_kat() +
  labs(x = "Index", y = "Studentized residuals")


ggplot(base_res, aes(x = csmptv, y = pred)) +
  geom_point() +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1, col = "red", size = 0.8) + 
  theme_kat() +
  labs(x = "Observed household consumption", y = "Predicted household consumption")

vif(base_model)

## 4.5. outliers -----------------------------

#+ robustcode, include = F, message = F

### single case ------------------

p <- length(base_model$coefficients) - 1
n <- length(base_model$residuals)

base_res$influence <- influence(base_model)$hat
base_res$dffits <- dffits(base_model)
base_res$dfbetas <- apply(dfbetas(base_model), 1, function(x) sum(abs(x) > 2/sqrt(n)))
base_res$cookd <- cooks.distance(base_model)

est_inf <- update(base_model, data = us[abs(base_res$influence) <= 2*p/n,])$coefficients
est_dffits <- update(base_model, data = us[abs(base_res$dffits) <= 2*sqrt(p/n),])$coefficients
est_dfbetas <- update(base_model, data = us[base_res$dfbetas < 1,])$coefficients
est_cookd <- update(base_model, data = us[base_res$cookd <= 4/n,])$coefficients

base_coef$est_inf <- c(est_inf[1:7], NA, est_inf[8:15])
base_coef$est_dffits <- est_dffits
base_coef$est_dfbetas <- est_dfbetas
base_coef$est_cookd <- est_cookd

### lmrob fit ----------

lmrob_fit <- lmrob(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol, data = us, na.action = na.exclude, weights = weight)

base_coef$est_MM <- lmrob_fit$coefficients

#+ robusttab, echo = F, message = F, results = "asis"

cat("\n")
kable(base_coef, row.names = F)
cat("\n")


# 5. built-up density ---------------------

fwd_1 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + pointsp, data = us, na.action = na.exclude, weights = weight)
summary(fwd_1)
anova(fwd_1)
rmsed_1 <- cvlm_f(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + pointsp, data = us, fold = 100, seed = mfn )

mean(rmsed_1)
mean(rmse_12b)
crPlot(fwd_1, "pointsp")


fwd_2 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + buf300_mean, data = us, na.action = na.exclude, weights = weight)

anova(fwd_2)
summary(fwd_2)
crPlot(fwd_2, "buf300_mean")

fwd_3 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + buf300_max, data = us, na.action = na.exclude, weights = weight)

summary(fwd_3)

fwd_4 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + buf300_mode, data = us, na.action = na.exclude, weights = weight)

summary(fwd_4)


fwd_5 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + buf1k_mean, data = us, na.action = na.exclude, weights = weight)

summary(fwd_5)


fwd_6 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + buf1k_mode, data = us, na.action = na.exclude, weights = weight)

summary(fwd_6)

crPlot(fwd_6, "buf1k_mode")
table(us$buf1k_mode)

fwd_7 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + buf1k_max, data = us, na.action = na.exclude, weights = weight)

summary(fwd_7)
crPlot(fwd_7, "buf1k_max")
table(us$buf1k_max)

table(us$pointsp)

table(us$buf300_max)

table(us$buf300_mode)

fwd_8 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + pointsp_rlv, data = us, na.action = na.exclude, weights = weight)

summary(fwd_8)

fwd_9 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + bltup2k5, data = us, na.action = na.exclude, weights = weight)

crPlot(fwd_9, "bltup2k5")
summary(fwd_9)

summary(us$bltup2k5[us$pointsp == "5"])

us$bud3c <- cut(us$bltup2k5, breaks = c(0,24,102, 499, 2500))

fwd_10 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + bud3c, data = us, na.action = na.exclude, weights = weight)

crPlot(fwd_10, "bud3c")
summary(fwd_10)
anova(fwd_10)
anova(fwd_9)
anova(fwd_1)
anova(fwd_2)
anova(fwd_3)
anova(fwd_4)
anova(fwd_5)
anova(fwd_6)
anova(fwd_7)
anova(fwd_8)
anova(fwd_9)

us$bf300max_rlv <- car::recode(us$buf300_max, "c('0', '1') = 'low'; c('2', '3') = 'medium'; c('4', '5') = 'high'")

fwd_11 <- lm(csmptv ~ nadtrc + hhs_0_17 +  inccat + rwtuse + dwowsh + livasc + bath + garden + pmnpol + bf300max_rlv, data = us, na.action = na.exclude, weights = weight)

anova(fwd_11)
summary(fwd_11)
table(us$buf300_max)






# 6. add random intercept -----------------


# testing -----------------

knitr::knit_exit()

#+ notcorrectedyet

ggplot(us, aes(x = dwcsy5, y = csmptv)) + geom_boxplot()

ggplot(us, aes(x = dwcsy5, y = livara)) + geom_boxplot()
