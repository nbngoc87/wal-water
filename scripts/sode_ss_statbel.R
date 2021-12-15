

# notes from the last run -----------------

# 1. setup ---------

## 1.1. load functions -----------------

### new functions -------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

### packages -----------

loadpackage("here")
loadpackage("sf")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder ----------
rdir <- "data/raw"
pdir <- "data/processed"

### age -------------------------------

path <-  here(pdir, "sode_ss_Be")
age_ss <- data.frame()

ls.file <- list.files(path = path, pattern = "age")


for (a in ls.file) {
  temp <-
    read.csv(
      here(path, a),
      na.strings = c("NA", "#NULL!", "#DIV/0!", "*"),
      strip.white = TRUE
    )
  colnames(temp) <-
    c(
      "municd",
      "ststcd",
      "population",
      "males",
      "females",
      paste("age", seq(0, 75, 5), seq(4, 79, 5), sep = "_"),
      "age_80+",
      "ststnm",
      "muninm"
    )
  temp$ststcd <- as.factor(temp$ststcd)
  temp <- temp[!is.na(temp$ststcd), ]
  temp$year <-
    as.numeric(gsub(".csv", "", unlist(strsplit(a, split = "_"))[3]))
  age_ss <- rbind.data.frame(age_ss, temp)
}

age_ss <- age_ss[order(age_ss$ststcd, age_ss$year), ]

### household size -------------------------------

hhs_ss <- data.frame()

ls.file <- list.files(path = path, pattern = "hhsize")


for (a in ls.file) {
  temp <-
    read.csv(
      here(path, a),
      na.strings = c("NA", "#NULL!", "#DIV/0!", "*"),
      strip.white = TRUE
    )[, 1:20]
  colnames(temp) <-
    c(
      "municd",
      "ststcd",
      "ststnm",
      "muninm",
      "nbhh",
      "nbhh_prv",
      "hh_1mem",
      "hh_married_0chld",
      "hh_married_wchld",
      "hh_cohab_0chld",
      "hh_cohab_wchld",
      "hh_single_parent",
      "hh_other",
      "nbhh_colective",
      "hhs_1",
      "hhs_2",
      "hhs_3",
      "hhs_4",
      "hhs_5+",
      "hhs_NA"
    )
  temp$ststcd <- as.factor(temp$ststcd)
  temp <- temp[!is.na(temp$ststcd), ]
  temp$ststcd <- paste0(temp$municd, temp$ststcd)
  temp$year <-
    as.numeric(gsub(".csv", "", unlist(strsplit(a, split = "_"))[3]))
  hhs_ss <- rbind.data.frame(hhs_ss, temp)
}

hhs_ss <- hhs_ss[order(hhs_ss$ststcd, hhs_ss$year), ]

### income ------------------------------------------


income_ss <-
  read.csv(here(path, "sode_income_2005_2017.csv"))[, c(1:2, 4:8)]

colnames(income_ss) <-
  c(
    "year",
    "municd",
    "ststcd",
    "nb_nonzero_inc",
    "taxa_inc_tot",
    "taxa_inc_avr",
    "taxa_inc_med"
  )

income_ss <- income_ss[order(income_ss$ststcd, income_ss$year), ]

### statistical sectors info -------

load(file = here(pdir, "admin_border_Wal/admin_ss_Wal.Rdata"))


# 2. select for wallonia ----------------

age_ss_wal <-
  dplyr::left_join(st_drop_geometry(wal_stst), age_ss[, !colnames(age_ss) %in% c("ststnm", "municd", "muninm")])

hhs_ss_wal <-
  dplyr::left_join(st_drop_geometry(wal_stst), hhs_ss[, !colnames(hhs_ss) %in% c("ststnm", "municd", "muninm")])

income_ss_wal <-
  dplyr::left_join(st_drop_geometry(wal_stst), income_ss[, !colnames(income_ss) %in% c("ststnm", "municd", "muninm")])



# 3. save data ----------------------------


save(age_ss, hhs_ss, income_ss, file = here(path, "sode_ss_Be.Rdata"))

save(age_ss_wal,
     hhs_ss_wal,
     income_ss_wal,
     file = here(path, "sode_ss_Wal.Rdata"))
