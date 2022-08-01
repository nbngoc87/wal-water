
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
loadpackage("xlsx")


source(here("scripts", "general_functions.R"))  

## 1.2 load data --------

### data folder ----------
rdir <- "data/raw"
pdir <- "data/processed"

### water consumption municipalities ----------

csmun <- read.xlsx(here(rdir, "water_consum_Aquawal_Wal", "Consommation par commune - consolidé pluriannuel.xls"), sheetIndex = 1, startRow = 2)

csmun <- csmun[, c(1:6, 9:12, 15:18, 22, 21, 25, 24)]

colnames(csmun) <- c("municd", "muninm", paste(rep(c("cons_lt250", "nmet_lt250", "cons_tot", "nmet_tot"), 4), rep(seq(2017, 2011, -2), each = 4), sep = "_"))


# 2. data format ------------------

csmun$municd <- as.character(csmun$municd)

csmun[, grep("nmet", colnames(csmun))] <- apply(csmun[, grep("nmet", colnames(csmun))], 2, function(x) as.integer(round(x)))

# 3. save data -----------

save(csmun, file = here(pdir, "water_histcons_Wal", "water_consum_mun.Rdata"))
