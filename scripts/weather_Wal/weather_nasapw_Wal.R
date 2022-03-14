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

loaddata <- function(path, a, centroid) {
  tmp <- read.csv(here(path, a), na.strings = c("", "#REF!", '#VALUE!'), strip.white = TRUE, encoding = "UTF-8", skip = 16, header = T)
  colnames(tmp) <- tolower(colnames(tmp))
  tmp$lat <- round(tmp$lat, digits = 4)
  tmp$lon <- round(tmp$lon, digits = 4)
  tmp$date <- as.Date((tmp$doy - 1), origin = paste(tmp$year, "01-01", sep = "-"))
  tmp$month <- month(tmp$date)
  tmp$ym <- format(tmp$date, "%Y-%m")
  tmp <- left_join(tmp, centroid)
  tmp
}

agg_bt <- function(df, groupby) {
  groupby <- enquo(groupby)
  tmp <- df %>% 
    group_by(!!groupby) %>%
    summarise(mnt = mean(t2m, na.rm = T),
              mnt_min = mean(t2m_min, na.rm = T),
              mnt_max = mean(t2m_max, na.rm = T),
              mnrh = mean(rh2m, na.rm = T),
              ttprec = sum(prectot, na.rm = T),
              ndrf = sum(prectot > 0, na.rm = T),
              ndsrf = sum(prectot >=2.5, na.rm = T))
  tmp
}

drought <- function(mdf = monthly_ls[[11]], spei = T, k = 3) {
  mdf <- mdf[order(mdf$ym),]
  
  
  if (spei) {
    pet <- hargreaves(Tmin = mdf$mnt_min, Tmax = mdf$mnt_max, lat = unique(mdf$lat), Pre = mdf$ttprec)
    bal <- mdf$ttprec - pet
    dr_ind <- spei(bal, k)$fitted
  } else {
    dr_ind <- spi(mdf$ttprec, k)$fitted
  }
  
  criteria <- as.numeric(dr_ind) < -1
  ex <- rle(criteria)
  ind <- rep(seq_along(ex$lengths), ex$lengths)
  s <- split(seq_len(nrow(mdf)), ind)
  event <- lapply(s[ex$values %in% T], function(x) data.frame(start = min(x), end = max(x), duration = max(x) - min(x) + 1))
  event <- do.call(rbind.data.frame, event)
  event <- event[event$duration >=2,]
  
  for (i in seq_len(nrow(event))) {
    ind_tmp <- event$start[i]
    
    while (ind_tmp <= nrow(mdf) & dr_ind[ind_tmp] < 0) {
      event$end[i] <- ind_tmp
      ind_tmp <- ind_tmp + 1
    }
  }
  
  event <- event[order(event$start),]
  event <- event[!duplicated(event$end),]
  event$duration <- event$end - event$start + 1
  
  event_lg <- apply(event, 1, function(x) {
    res <- data.frame(ind = x[1]:x[2])
    min_dr_ind <- min(dr_ind[res$ind])
    res$intcat <- ifelse(min_dr_ind >= -2, 1, 2)
    res
  })
  
  event_lg <- do.call(rbind.data.frame, event_lg)
  
  mdf$dr_intcat <- 0
  mdf$dr_intcat[event_lg$ind] <- event_lg$intcat
  mdf$dr_ind <- dr_ind
  
  res <- mdf %>% 
    group_by(year) %>%
    summarise(ndrm  = sum(dr_intcat > 0, na.rm = T),
              mn_drind = mean(dr_ind, na.rm = T),
              cum_drind = sum(dr_ind, na.rm = T),
              mn_drint = mean(dr_ind[dr_intcat > 0], na.rm = T),
              cum_drint = sum(dr_ind[dr_intcat > 0], na.rm = T),
              nintcat1 = sum(dr_intcat %in% 1, na.rm = T),
              nintcat2 = sum(dr_intcat %in% 2, na.rm = T))
  
  res$year <- as.integer(as.character(res$year))
  res
}

### packages -----------

loadpackage("here")
loadpackage("ggplot2")
loadpackage("scico")
loadpackage("dplyr")
loadpackage("lubridate")
loadpackage("heatwaveR")
loadpackage("SPEI")

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

## 2.1. format centroid ---------------

centroid$lat <- round(centroid$lat, digits = 4)

centroid$lon <- round(centroid$lon, digits = 4)

## 2.2. format daily weather -----------

# a <- filels[[2]]

daily_ls <- lapply(filels, loaddata, path = path, centroid = centroid)

names(daily_ls) <- sapply(daily_ls, function(x) unique(x$municd))

save(daily_ls, file = here(pdir, "weather_general_NasaPower_Be", "weather_daily_NasaPower_Be.Rdata"))

# load(file = here(pdir, "weather_general_NasaPower_Be", "weather_daily_NasaPower_Be.Rdata"))

## 2.2. monthly data -----------------

monthly_ls <- lapply(daily_ls, function(x) {
  tmp <- agg_bt(x, groupby = ym)
  tmp$month <- format(as.Date(paste(tmp$ym, '01', sep = '-')), '%b')
  tmp$month <- factor(tmp$month, levels = month.abb)
  tmp$year <- gsub('-.*', '', tmp$ym)
  tmp$municd <- unique(x$municd)
  tmp$lon <- unique(x$lon)
  tmp$lat <- unique(x$lat)
  tmp
})

save(monthly_ls, file = here(pdir, "weather_general_NasaPower_Be", "weather_monthly_NasaPower_Be.Rdata"))

# load(file = here(pdir, "weather_general_NasaPower_Be", "weather_monthly_NasaPower_Be.Rdata"))


## 2.3. yearly data ----------------------

yearly_ls <- lapply(1:length(daily_ls), function(x) {
  
  ddf <- daily_ls[[x]]
  mdf <- monthly_ls[[x]]
  sm_months = c(6,7,8)
  wt_months = c(12, 1, 2)
  
  # yearly average
  
  res <- agg_bt(ddf, groupby = year)
  
  # summer
  
  summer <- agg_bt(ddf[ddf$month %in% sm_months,], groupby = year)
  colnames(summer)[2:ncol(summer)] <- paste(colnames(summer)[2:ncol(summer)], "sm", sep = "_")
  
  # winter 
  
  winter <- agg_bt(ddf[ddf$month %in% wt_months,], groupby = year)
  colnames(winter)[2:ncol(winter)] <- paste(colnames(winter)[2:ncol(winter)], "wt", sep = "_")
  
  # heatwave
  # using default values of arguments <=> using Hobday et al. (2016) definition of heatwave
  
  clmdf <- ts2clm(data = ddf, x = date, y = t2m_max, climatologyPeriod = c(min(ddf$date), max(ddf$date)))
  evls <- detect_event(clmdf, x = date, y = t2m_max,  minDuration = 5, joinAcrossGaps = T)
  heatwave <- block_average(evls, x = date, y = t2m_max)[, c("year", "count", "total_days", "total_icum")]
  colnames(heatwave)[2:4] <- paste(colnames(heatwave)[2:4], "hw", sep = "_")
  heatwave[is.na(heatwave)] <- 0
  
  # coldspell 
  
  clmdf <- ts2clm(data = ddf, x = date, y = t2m_min, climatologyPeriod = c(min(ddf$date), max(ddf$date)), pctile = 10)
  evls <- detect_event(clmdf, x = date, y = t2m_min,  minDuration = 5, joinAcrossGaps = T, coldSpells = T)
  coldspell <- block_average(evls, x = date, y = t2m_min)[, c("year", "count", "total_days", "total_icum")]
  colnames(coldspell)[2:4] <- paste(colnames(coldspell)[2:4], "cs", sep = "_")
  coldspell[is.na(coldspell)] <- 0
  
  # drought 
  # definition of drought following https://www.sciencedirect.com/science/article/pii/S0921818116301801#bb0300
  
  spei <- drought(mdf, spei = T)
  colnames(spei)[2:ncol(spei)] <- paste(colnames(spei[2:ncol(spei)]), "spei", sep = "_")
  spi <- drought(mdf, spei = F)
  colnames(spi)[2:ncol(spi)] <- paste(colnames(spi[2:ncol(spi)]), "spi", sep = "_")
  
  # combine everything

  res <- Reduce(left_join, list(res, summer, winter, heatwave, coldspell, spei, spi))
  
  res$municd <- unique(ddf$municd)
  res
  
})


names(yearly_ls) <- sapply(yearly_ls, function(x) unique(x$municd))
save(yearly_ls, file = here(pdir, "weather_general_NasaPower_Be", "weather_yearly_NasaPower_Be.Rdata"))

# load(file = here(pdir, "weather_general_NasaPower_Be", "weather_yearly_NasaPower_Be.Rdata"))
