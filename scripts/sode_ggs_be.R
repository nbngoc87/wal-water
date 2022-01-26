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
loadpackage("haven")

source(here("scripts", "general_functions.R"))

## 1.2. load data --------

### data folder ----------
rdir <- "data/raw"
pdir <- "data/processed"

### load from .sav ----------

tmp <-
  read_sav(
    here(
      rdir,
      "sode_GGS_GGP_Be",
      "GGS_Wave1_Belgium_V.4.4",
      "GGS_Wave1_Belgium_V.4.4.sav"
    )
  )

# 2. process data ---------

ggs_wal <-
  tmp[tmp$aregion %in% 2303, c(
    "arid",
    "amonth",
    "ayear",
    "aweight",
    "aweight_2303",
    "adwell",
    "a119",
    "a122",
    "a1009",
    "ahhsize",
    "ahhtype",
    grep("ahg3_|ahg4_|ahg5_", colnames(tmp), value = T)
  )]

notempty <-
  apply(is.na(ggs_wal), 2, function(x)
    sum(x) < nrow(ggs_wal))

ggs_wal <- ggs_wal[, notempty]

ggs_wal$ayear <- zap_labels(ggs_wal$ayear)
ggs_wal$amonth <- zap_labels(ggs_wal$amonth)

ggs_wal$adwell <- droplevels(as_factor(ggs_wal$adwell))

ggs_wal$a119 <- zap_labels(ggs_wal$a119)

ggs_wal$a122 <- droplevels(as_factor(ggs_wal$a122))

ggs_wal$a1009 <- droplevels(as_factor(ggs_wal$a1009))

ggs_wal$ahhtype <- droplevels(as_factor(ggs_wal$ahhtype))

ggs_wal[, grep("ahg3_|ahg4_", colnames(ggs_wal), value = T)] <-
  droplevels(as_factor(ggs_wal[, grep("ahg3_|ahg4_", colnames(ggs_wal), value = T)]))

ggs_wal[, grep("ahg5_", colnames(ggs_wal), value = T)] <-
  zap_labels(ggs_wal[, grep("ahg5_", colnames(ggs_wal), value = T)])

# 3. save data ---------------------



save(ggs_wal, file = here(pdir, "sode_GGS_GGP_Be/sode_GGS_Be.Rdata"))


