#' ---
#' title: "Wallonie weather"
#' author: "Nguyen Bich Ngoc, Huynh Chi Cuong, Jacques Teller"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---


#+ r setup, include = F, message = F
# notes from last run----------------------

# rmarkdown::render("scripts/weather_Wal/weather_nasapw_Wal.R",output_file=paste0("weather_nasapw_Wal_", format(Sys.time(), "%y%m%d_%H%M"),".md"))

# 1. setup ---------


## 1.1. load functions -----------------

### new functions ------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

loaddata <- function(path, a) {
  tmp <- read.csv(here(path, a), na.strings = c("", "#REF!", '#VALUE!'), strip.white = TRUE, encoding = "UTF-8", skip = 16, header = T)
  colnames(tmp) <- tolower(colnames(tmp))
  tmp$lat <- round(tmp$lat, digits = 4)
  tmp$lon <- round(tmp$lon, digits = 4)
  tmp$date <- as.Date((tmp$doy - 1), origin = paste(tmp$year, "01-01", sep = "-"))
  tmp$month <- month(tmp$date)
  tmp$ym <- format(tmp$date, "%Y-%m")
  tmp
}

agg_bm <- function(df) {
  tmp <- df %>% 
    group_by(ym) %>%
    summarise(mnt = mean(t2m, na.rm = T),
              mnt_min = mean(t2m_min, na.rm = T),
              mnt_max = mean(t2m_max, na.rm = T),
              mnrh = mean(rh2m, na.rm = T),
              ttprec = sum(prectot, na.rm = T),
              ndrf = sum(prectot > 0, na.rm = T),
              ndsrf = sum(prectot >=2.5, na.rm = T))
  tmp$month <- format(as.Date(paste(tmp$ym, '01', sep = '-')), '%b')
  tmp$month <- factor(tmp$month, levels = month.abb)
  tmp$year <- gsub('-.*', '', tmp$ym)
  tmp
}

### packages -----------

loadpackage("here")
loadpackage("ggplot2")
loadpackage("scico")
loadpackage("dplyr")
loadpackage("lubridate")

source(here("scripts", "general_functions.R"))

### plot params -----------

col1_dark <- scico(1, palette = "oslo", begin = 0.2)



col1_light <- scico(1, palette = "oslo", begin = 0.4)

pal_div <- "roma"
pal_con <- "oslo"
pal_disc <- "batlow"

pal_bg <- 0.2
pal_end <- 0.8

fig_d1 <- 3.54331
fig_d2 <- 5.51181
fig_d3 <- 7.48031

mfn <- 2512

knitr::opts_chunk$set(fig.width = fig_d1,
                      fig.height = fig_d1,
                      dpi = 1000)

## 1.2 load data --------

### data folder ---------
rdir <- "data/raw"
pdir <- "data/processed"


### weather data path -----------------

path <- here(rdir, "weather_general_NasaPower_Be")

filels <- list.files(path)

### municipal centroid Wal ------------------

centroid <- read.csv(here(pdir, "admin_border_Be", "centroid_coord_4326.csv"), na.strings = c("", "#NULL!"), strip.white = TRUE)
colnames(centroid) <- c("municd", "muninm", "lat", "lon")



# 2. processing -----------

## 2.1. format daily weather -----------

# a <- filels[[2]]

daily_ls <- lapply(filels, loaddata, path = path)

## 2.2. monthly data -----------------

tmp <- daily_ls[[1]]

test <- do.call(data.frame, aggregate(t2m_min ~ lat + lon + ym, data = tmp, FUN = function(x) c(mn = mean(x, na.rm = T), sd = sd(x, na.rm = T))))
