#' ---
#' title: "Population synthesis"
#' author: "Nguyen Bich Ngoc, Ismail Saadi, Ahmed Mustafa, Jacques Teller"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---
#' 
#' 

#+ r setup, include = F, message = F
# notes from last run----------------------





# rmarkdown::render("scripts/cons_proj/wcons_snros.R",output_file=paste0("wcons_snros_", format(Sys.time(), "%y%m%d_%H%M"),".md"))

# 1. set up -------------------

## 1.1. load functions -------------

### new functions ------------



loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}
### packages ----------
loadpackage("here")
loadpackage("scico")
loadpackage("ggplot2")
loadpackage("dplyr")
loadpackage("ggspatial")
loadpackage("sf")
loadpackage("cowplot")
loadpackage("reshape2")
loadpackage("raster")
loadpackage("xlsx")
loadpackage("ggpmisc")
loadpackage("lubridate")




source(here("scripts", "general_functions.R"))

### plot params -----------

col1_dark <- scico(1, palette = "lapaz", begin = 0.2)

col1_light <- scico(1, palette = "lapaz", begin = 0.4)

pal_div <- "roma"
pal_con <- "oslo"
pal_disc <- "batlow"

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

### administrative borders -----------

load(file = here(
  pdir,
  "admin_border_Be/admin_Be.Rdata"
))

stst_wal <- stst[stst$regicd %in% 3000, ]
stst_wal$municd <- as.character(stst_wal$municd)

mun_wal <- muni[muni$regicd == 3000,]
mun_wal$municd <- as.character(mun_wal$municd)

### historical consumption --------

load(file = here(pdir, "water_histcons_Wal", "water_histcons.Rdata"))

### utility survey ----------

us <-
  read.csv(file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_AquaWal_prd.csv"
  ))

### landuse 2010 2k5 ------------

lu102k5 <- raster(here(rdir, "urban_2k5_Ahmed_Wal", "cat2010.flt"))

crs(lu102k5) <- st_crs(mun_wal)$proj4string

### statistical sectors ------------

load(file = here(pdir, "sode_ss_Be", "sode_ss_Be.Rdata"))

hhs_ss_wal <- hhs_ss[hhs_ss$regicd %in% 3000,]

age_ss_wal <- age_ss[age_ss$regicd %in% 3000,]
age_ss_wal$municd <- as.character(age_ss_wal$municd)
age_ss_wal <- 
  age_ss_wal[, c(grep("age_", colnames(age_ss_wal), value = T), "municd", "ststcd", "year", "population")]

age_ss_wal$hhs_0_19 <- apply(age_ss_wal[,1:4], 1, sum, na.rm = T)
age_ss_wal$hhs_20_95 <- apply(age_ss_wal[,5:17], 1, sum, na.rm = T)



### rainwater tank ------------

rwt_mun <- read.csv(here(pdir, "raintank_muni_Be", "raintank_muni_Be.csv"))
rwt_mun$municd <- as.character(rwt_mun$municd)

### consumption at mun -----------

load(file = here(pdir, "water_histcons_Wal", "water_consum_mun.Rdata"))
summary(csmun$cons_lt250_2017*100/csmun$cons_tot_2017)

### projected IWEPS ----------

#### nhh
prjnhh <- read.xlsx(here(rdir, "sode_proj_IWEPS_Wal", "244601.xlsx"), sheetIndex = 1, startRow = 11)[,c(1:2, 6:8, 5)]
colnames(prjnhh) <- c("municd", "type", "nhh_2020", "nhh_2025", "nhh_2030", "nhh_2035")


prjnhh <- prjnhh[prjnhh$type %in% "Commune", c(1,3:6)]
prjnhh$municd <- as.character(prjnhh$municd)

#### pop


prjpop <- read.xlsx(here(rdir, "sode_proj_IWEPS_Wal", "244600.xlsx"), sheetIndex = 1, startRow = 11)[,c(1:2, 6:8, 5)]
colnames(prjpop) <- c("municd", "type", "pop_2020", "pop_2025", "pop_2030", "pop_2035")


prjpop <- prjpop[prjpop$type %in% "Commune", c(1,3:6)]
prjpop$municd <- as.character(prjpop$municd)

### synthetic data ----------------

load(file = here(pdir, "cons_proj_Wal", "sim_agg_1119.Rdata"))
sim_agg_1119 <- sim_agg
load(file = here(pdir, "cons_proj_Wal", "hhs_sim_2040.Rdata"))

### projected demand -----------

file_ls <-
  list.files(path = here(pdir, "cons_proj_Wal", "results 220416"), pattern = "sim_agg_")


for (a in file_ls) {
  load(file = here(pdir, "cons_proj_Wal", "results 220416", a))
  assign(gsub(".Rdata", "", a), sim_agg, envir = .GlobalEnv)
}

rm(sim_agg)

#2. merging data ---------------



snros <- ls(pattern = "sim_agg_[a-zA-Z]", envir = .GlobalEnv)


res <- lapply(snros, function (a) {
  df <- get(a, envir = .GlobalEnv)
  df$cspc <- df$cons/df$pop

  res <- df %>%
    group_by(municd, year) %>%
    summarise(
      mean_cons = mean(cons, na.rm = T),
      lwr_cons = quantile(cons, na.rm = T, probs = 0.025),
      upr_cons = quantile(cons, na.rm = T, probs = 0.975),
      mean_pop = mean(pop, na.rm =T)
    )
  
  res$snros <- gsub("sim_agg_", "", a)
  
  res
})

sumdf <- bind_rows(res)
sumdf$blt_snro <- gsub("_[^_]+$", "", sumdf$snros)
sumdf$rwt_snro <- gsub(".*_", "", sumdf$snros)


sumdf$mean_cspc <- sumdf$mean_cons/sumdf$mean_pop
sumdf$lwr_cspc <- sumdf$lwr_cons/sumdf$mean_pop
sumdf$upr_cspc <- sumdf$upr_cons/sumdf$mean_pop

# 3. plots/maps ----------

majormun <-mun_wal[mun_wal$municd %in% c("62063", "52011", "92094", "53053", "55022"), c("municd", "muninm")] 

majormun <- majormun[c(5, 3, 1, 4, 2),]

## 3.1. rainwater tank maps ---------------

#+ rwt01, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d2



summary(rwt_mun$rwt_prop)

plotdf <- left_join(mun_wal, rwt_mun[, c("municd", "rwt_prop")])

ggplot(data = neighbor) +
  geom_sf(fill = c("white", rep("grey90", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  geom_sf(data = plotdf, aes(fill = rwt_prop)) +
  scale_fill_scico(palette = pal_con, begin = pal_bg, end = 0.9, direction = -1) +
  geom_sf(data = maincity) +
  geom_sf_text(data = maincity, aes(label = muninm), fontface = "bold", size = 3, nudge_y = -2500) +
  annotation_scale(location = "bl", width_hint = 0.15, pad_x = unit(2.3, "in"), pad_y = unit(0.5, "in")) +
  annotation_north_arrow(location = "bl", height = unit(0.5, "in"), width = unit(0.5, "in"), which_north = "true", pad_x = unit(2.65, "in"), pad_y = unit(0.7, "in"), style = north_arrow_fancy_orienteering)  +
  geom_text(x = 95000, y = 50000, label = "Rainwater use", size = 8) +
  theme_void() +
  labs(fill = "") +
  theme(legend.position = c(0.18,0.135), plot.margin = unit(rep(-0.9,4), "cm"),legend.direction="horizontal") 

## 3.2. proportion household size at stst -----------

#+ hhsprss, echo = F, message = F, fig.width = fig_d1, fig.height = fig_d1

plotdf <- hhs_ss_wal[hhs_ss_wal$ststcd %in% c("25005A001", "53053G214") & hhs_ss_wal$year == 2016, c("ststcd", grep("hhs_\\d", colnames(hhs_ss_wal), value = T))]



probmat <- t(apply(plotdf[, -1], 1, function(x) x/sum(x)))



plotdf <- cbind.data.frame(ststcd = plotdf$ststcd, probmat)

plotdf <- melt(plotdf, id.vars = "ststcd")

ggplot(plotdf, aes(x = variable, y = value, fill = ststcd)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5+")) +
  scale_fill_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  labs(x = "Household size", y = "Proportion (%)", fill = "Statistical unit") +
  theme_kat() +
  theme(legend.position = c(0.8, 0.8)) 



## 3.3. landuse 2010 2500lvls -------------

#+ lu102k5, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d2

# plotdf <- as.data.frame(lu102k5, xy = T) %>% na.omit()
# 
# 
# ggplot(data = plotdf) +
#   geom_sf(data = neighbor, fill = c("white", rep("grey90", 4))) +
#   geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
#   geom_sf(data = mun_wal) + 
#   geom_tile(data = plotdf , aes(x =x, y = y, fill = cat2010)) +
#   scale_fill_scico(palette = pal_con, direction = -1) +
#   geom_sf(data = maincity) +
#   geom_sf_text(data = maincity, aes(label = muninm), fontface = "bold", size = 3, nudge_y = -2500) +
#   annotation_scale(location = "bl", width_hint = 0.15, pad_x = unit(2.3, "in"), pad_y = unit(0.5, "in")) +
#   annotation_north_arrow(location = "bl", height = unit(0.5, "in"), width = unit(0.5, "in"), which_north = "true", pad_x = unit(2.65, "in"), pad_y = unit(0.7, "in"), style = north_arrow_fancy_orienteering)  +
#   geom_text(x = 95000, y = 50000, label = "Builtup density", size = 8) +
#   theme_void() +
#   labs(fill = "") +
#   theme(legend.position = c(0.18,0.135), legend.text = element_text(size= 7),legend.direction="horizontal") 

## 3.4. historical consumption -------------

#+ hconshh, echo = F, message = F, fig.width = fig_d1, fig.height = fig_d1

test <- fitdf[fitdf$hhs_20_100 == 1 & fitdf$hhs_0_19 ==0,]

plotdf <- fitdf[fitdf$qnb %in% c("191", "56") & fitdf$year > 2009,]


ggplot(plotdf, aes(x = year, y = cspd, color = as.factor(qnb))) +
  geom_line(size = 1)  +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  labs(x = "Year", y = "Consumption (L/day)", color = "Family") +
  scale_x_continuous(breaks = seq(2011, 2019, 2))+
  theme_kat() +
  ylim(0, 600)  + 
  theme(legend.position = c(0.6, 0.8)) 

## 3.5. validate nhhs projection -----------
#+ valnhh, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

hhs_sim_mun <- hhs_sim_2040 %>%
  group_by(municd, year) %>%
  summarise(tot = sum(nbhh_prv, na.rm = T))

plotdf <- melt(prjnhh, id.vars = "municd")
plotdf$year <- as.numeric(gsub("nhh_", "", plotdf$variable))

plotdf <- inner_join(hhs_sim_mun, plotdf)

ggplot(plotdf, aes(x = tot, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)  +
  facet_grid(. ~ year) +
  stat_poly_eq(formula = y ~ x, aes(label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")", sep = "")), parse = TRUE) +
  labs(x = "Projected # households by this study", y = "Projected # households by IWVEPS") +
  theme_kat()


## 3.6. validate nad/nch 1119 -----------
#+ valhhcps1119, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

sim_1119_mun <- sim_agg_1119 %>%
  group_by(municd, year) %>%
  summarise(nad = mean(nad, na.rm = T),
            nch = mean(nch, na.rm = T))

age_mun_wal <- age_ss_wal %>%
  group_by(municd, year) %>%
  summarise(nad_rcd = sum(hhs_20_95, na.rm = T),
            nch_rcd = sum(hhs_0_19, na.rm = T)) 

plotdf <- inner_join(sim_1119_mun, age_mun_wal)


p1 <- ggplot(plotdf, aes(x = nad, y = nad_rcd)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.8)   +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  labs(x = "Simulated number of adults", y = "Recorded number of adults", color = "Year") +
  theme_kat()

p2 <- ggplot(plotdf, aes(x = nch, y = nch_rcd)) +
  geom_point(aes(color = as.factor(year)), size = 2) +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x)  +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.8)   +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)+
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  labs(x = "Simulated number of children", y = "Recorded number of children") +
  theme_kat()

legend <- get_legend(p1)
pg <- plot_grid(p1 + theme(legend.position = "none"),
                p2 + theme(legend.position = "none"),
                align = "hv",
                labels = "AUTO")

plot_grid(pg, legend, rel_heights = c(1, 0.15), nrow = 2)


## 3.7. validate pop projection -----------
#+ valpop, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

pop_sim_mun <- sim_agg_BL_1 %>%
  group_by(municd, year) %>%
  summarise(pop = mean(pop, na.rm = T))

plotdf <- melt(prjpop, id.vars = "municd")
plotdf$year <- as.numeric(gsub("pop_", "", plotdf$variable))

plotdf <- inner_join(pop_sim_mun, plotdf)

ggplot(plotdf, aes(x = pop, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.8)  +
  facet_grid(. ~ year) +
  stat_poly_eq(formula = y ~ x, aes(label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")", sep = "")), parse = TRUE) +
  labs(x = "Projected population by this study", y = "Projected population by IWVEPS") +
  theme_kat()

## 3.8. summary fitdf ------------

nlvls(fitdf$qnb)

dscstat <- fitdf %>%
  group_by(year) %>%
  summarise(mean = mean(cspd, na.rm = T),
            sd = sd(cspd, na.rm = T))
write.table(dscstat, "clipboard", sep = "\t")

mean(us$hhs_0_19, na.rm = T)

## 3.9. plots water demand 10-19 mun lvls ------------

#+ histmun, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1


plotdf <- melt(csmun[, c("municd", grep("cons_", colnames(csmun), value =T))], id.vars = "municd")
plotdf$variable <- gsub("cons_", "", plotdf$variable)
plotdf$cstp <- gsub("_.*", "", plotdf$variable)
plotdf$year <- as.numeric(gsub(".*_", "", plotdf$variable))

test <- plotdf %>%
  group_by(cstp, year) %>%
  summarise(count = n(),
            tot = sum(value, na.rm = T))

test$tot[1:4]/test$tot[5:8]

1 - (test$tot[4]/test$tot[1])^(1/6)

1 - (test$tot[8]/test$tot[5])^(1/6)


plotdf <- inner_join(plotdf, majormun)

ggplot(plotdf, aes(x = year, y = value/1e6, color = cstp)) +
  geom_point() +
  geom_line(size = 0.8) +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end, labels = c("Small consumers", "Total")) +
  ylim(0,10) +
  facet_grid(.~factor(muninm, levels = c("Liege",       "Charleroi" ,  "Namur"    ,   "Mons"   ,     "La Louviere"))) +
  labs(x = "Year", y = expression(Demand~(Million~m^3)), color = "Consumption types") +
  theme_kat()


## 3.10. plots water demand 10-19 hhs lvls ------------
#+ histhh, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

fitdf$cspy <- fitdf$cspd*yday(as.Date(paste0(fitdf$year, "-12-31")))/1000

p1 <- 
  ggplot(fitdf[fitdf$year > 2009,], aes(x = year, y = cspy)) +
  geom_smooth(color = col1_dark) +
  scale_x_continuous(breaks = seq(2011, 2019, 2)) +
  labs(x = "Year", y = expression(Household~annual~consumption~(m^3/year))) +
  theme_kat()

fitdf$neqad <- 1+(fitdf$hhs_20_100-1) * 0.5 + fitdf$hhs_0_19*0.3

p2 <- ggplot(fitdf[fitdf$year > 2009,], aes(x = year, y = cspd/neqad)) +
  geom_smooth(color = col1_dark) +
  scale_x_continuous(breaks = seq(2011, 2019, 2))+
  labs(x = "Year", y = "Consumption per equivalent adult (L/person/day)") +
  theme_kat()

plot_grid(
  p1 ,p2 ,
  nrow = 1,
  align = "hv",
  labels = "AUTO"
)

test <- fitdf %>%
  group_by(year) %>%
  summarise(count = n(),
            cspy = mean(cspy, na.rm = T),
            cspd = mean(cspd, na.rm = T))
1 - (68.57499/73.8258)^(1/4)
 
## 3.11. household size evolution 11-19 ---------

#+ nhh1119, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1
plotdf <- hhs_ss_wal[,c("municd", "ststcd", "year", "nbhh_prv", "hhs_1")]

ststls <- unique(plotdf$ststcd)

n <- sapply(ststls, function(x) {
  df <- plotdf[plotdf$ststcd == x,]
  df <- df[!is.na(df$nbhh_prv) & !is.na(df$hhs_1),]
  res <- nrow(df)
})

summary(n)
ststls <- (ststls[n == 9])


 
hhs_y_wal <-  hhs_ss_wal[hhs_ss_wal$ststcd %in% ststls,]%>%
  group_by(year) %>%
  summarise(nNAnhh = sum(is.na(nbhh_prv)),
            count = n(),
            nbhh_prv = sum(nbhh_prv, na.rm = T),
            prop1 = sum(hhs_1, na.rm = T)/sum(nbhh_prv, na.rm = T))

hhs_y_wal$rel_nhh <- hhs_y_wal$nbhh_prv/14085.81

p1 <- ggplot(hhs_y_wal, aes(x = year, y = rel_nhh)) +
  geom_point() +
  geom_smooth(color = col1_dark) +
  scale_x_continuous(breaks = seq(2011, 2019, 2))+
  labs(x = "Year", y = "Relative number of households") +
  theme_kat()


p2 <- ggplot(hhs_y_wal, aes(x = year, y = prop1)) +
  geom_point() +
  geom_smooth(color = col1_dark) +
  scale_x_continuous(breaks = seq(2011, 2019, 2))+
  labs(x = "Year", y = "Proportion of single-member households (%)") +
  theme_kat()

plot_grid(
  p1 ,p2 ,
  nrow = 1,
  align = "hv",
  labels = "AUTO"
)


## 3.12. Wal projection plots -----------

#+ wconsbs, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d1

write.table(sim_agg_DH_1 %>%
  group_by(rep) %>%
  summarise(n = sum(n),
            pop = sum(pop),
            cons = sum(cons)) %>%
  summarise(n = mean(n),
            pop = mean(pop),
            cons = mean(cons)), "clipboard", sep = "\t")

plotdf <- sumdf[,2:9] %>%
  group_by(year, snros, blt_snro, rwt_snro) %>%
  summarise_all(sum, na.rm = T)
plotdf$mean_cspc <- plotdf$mean_cons/plotdf$mean_pop
plotdf$lwr_cspc <- plotdf$lwr_cons/plotdf$mean_pop
plotdf$upr_cspc <- plotdf$upr_cons/plotdf$mean_pop

plotdf <- plotdf[plotdf$rwt_snro == 1 & plotdf$blt_snro %in% c("BH", "BL", "DH", "DL"),]

plotdf$blt_snro <- factor(plotdf$blt_snro, levels =  lvls(plotdf$blt_snro)[c(2,1,4,3)])

p1 <- ggplot(plotdf, aes(x = year, y = mean_cons/1e6, color = blt_snro, fill = blt_snro)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = lwr_cons/1e6, ymax = upr_cons/1e6), alpha = 0.3, color = NA) +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  scale_fill_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  labs(x = "Year", y = expression(Projected~demand~(Million ~m^3)), color = "Urbanization scenarios") + 
  guides(fill= "none") +
  theme_kat(legend.position = "bottom")




p2 <- ggplot(plotdf, aes(x = year, y = mean_cspc, color = blt_snro, fill = blt_snro)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = lwr_cspc, ymax = upr_cspc), alpha = 0.3, color = NA) +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  scale_fill_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  labs(x = "Year", y = expression(Projected~demand~per~capita~(m^3/year)), color = "Urbanization scenarios") + 
  guides(fill= "none") +
  theme_kat(legend.position = "bottom")

legend <- get_legend(p1)

pg <-
  plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    nrow = 1,
    align = "hv",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, 0.1))

test <- dcast(plotdf[, c("year", "snros", "mean_cspc")], snros ~ year, value.var = "mean_cspc")

test$change <- (test$`2040` - test$`2020`)*100/test$`2020`

summary(test$change)

#+ wconsrs, echo = F, message = F, fig.width = fig_d2, fig.height = fig_d1
 
plotdf <- sumdf[,2:9] %>%
  group_by(year, snros, blt_snro, rwt_snro) %>%
  summarise_all(sum, na.rm = T) %>%
  filter(blt_snro == "BL")

ggplot(plotdf, aes(x = year, y = mean_cons/1e6, color = rwt_snro, fill = rwt_snro)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = lwr_cons/1e6, ymax = upr_cons/1e6), alpha = 0.3, color = NA) +
  scale_color_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end, labels = c("80%", "90%", "100%", "110%", "120%")) +
  scale_fill_scico_d(palette = pal_disc, begin = pal_bg, end = pal_end) +
  labs(x = "Year", y = expression(Projected~demand~(Million ~m^3)), color = "Rainwater use scenarios") + 
  guides(fill= "none") +
  theme_kat(legend.position = "right")

test <- dcast(plotdf[, c("year", "snros", "mean_cons")], snros ~ year, value.var = "mean_cons")

test$change <- (test$`2040` - test$`2020`)*100/test$`2020`

summary(test$change)

#+ cons40bl1, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d2

### bl1 

snro <- "BL_1"


plotdf <- left_join(mun_wal, sumdf[sumdf$year == 2040 & sumdf$snros == snro, c("municd","mean_cspc")])

ggplot(data = neighbor) +
  geom_sf(fill = c("white", rep("grey90", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  geom_sf(data = plotdf, aes(fill = mean_cspc)) +
  scale_fill_scico(palette = pal_div, begin = pal_bg, end = pal_end, direction = -1) +
  geom_sf(data = maincity) +
  geom_sf_text(data = maincity, aes(label = muninm), fontface = "bold", size = 3, nudge_y = -2500) +
  annotation_scale(location = "bl", width_hint = 0.15, pad_x = unit(2.3, "in"), pad_y = unit(0.5, "in")) +
  annotation_north_arrow(location = "bl", height = unit(0.5, "in"), width = unit(0.5, "in"), which_north = "true", pad_x = unit(2.65, "in"), pad_y = unit(0.7, "in"), style = north_arrow_fancy_orienteering)  +
  geom_text(x = 95000, y = 50000, label = "Demand per capita", size = 8) +
  theme_void() +
  labs(fill = "") +
  theme(legend.position = c(0.18,0.135), plot.margin = unit(rep(-0.9,4), "cm"),legend.direction="horizontal") 

st_bbox(plotdf)
colnames(sumdf)


## 3.12. maps # btl_snros --------------

#+ dif2040bl1, echo = F, message = F, fig.width = fig_d3, fig.height = fig_d2

### bl1 

snro <- "BL_1"

df_40 <- sumdf[sumdf$year == 2040 & sumdf$snros == snro, c(1,3)]
colnames(df_40)[2] <- "prj40"
df_20 <- sumdf[sumdf$year == 2020 & sumdf$snros == snro, c(1,3)]
colnames(df_20)[2] <- "prj20"

plotdf <- left_join(df_20, df_40)
plotdf$dif <- (plotdf$prj40 - plotdf$prj20)*100/plotdf$prj20

plotdf <- left_join(mun_wal, plotdf)

ggplot(data = neighbor) +
  geom_sf(fill = c("white", rep("grey90", 4))) +
  geom_sf_text(data = neighbor, aes(label = NAME_ENGL), size = 4, nudge_x = c(0, -7000, 0, 0, 0), nudge_y = c(0, -30000, 0, 0, 0)) +
  geom_sf(data = plotdf, aes(fill = dif)) +
  scale_fill_scico(palette = pal_div, begin = pal_bg, end = pal_end, direction = -1, limits=c(-100,100)) +
  geom_sf(data = maincity) +
  geom_sf_text(data = maincity, aes(label = muninm), fontface = "bold", size = 3, nudge_y = -2500) +
  annotation_scale(location = "bl", width_hint = 0.15, pad_x = unit(2.3, "in"), pad_y = unit(0.5, "in")) +
  annotation_north_arrow(location = "bl", height = unit(0.5, "in"), width = unit(0.5, "in"), which_north = "true", pad_x = unit(2.65, "in"), pad_y = unit(0.7, "in"), style = north_arrow_fancy_orienteering)  +
  theme_void() +
  labs(fill = "Changes in demand (%)") +
  theme(legend.position = c(0.18,0.25), plot.margin = unit(rep(-0.9,4), "cm"))


