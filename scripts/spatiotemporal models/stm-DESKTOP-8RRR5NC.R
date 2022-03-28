#' ---
#' title: "Spatio-temporal models"
#' author: "Nguyen Bich Ngoc, Mario Cools, Jacques Teller"
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

### packages ----------


loadpackage("here")
loadpackage("ggplot2")
loadpackage("scico")
loadpackage("sf")
loadpackage("raster")

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


### historical consumption ----------

load(file = here(
  pdir,
  "water_histcons_Wal", "swde.Rdata"
))

load(file = here(
  pdir,
  "water_histcons_Wal", "CILE", "cile.Rdata"
))

load(file = here(
  pdir,
  "water_histcons_Wal", "inbw.Rdata"
))
