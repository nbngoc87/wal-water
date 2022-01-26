
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

### rainwater ----------

rw01 <- read.xlsx(here(rdir, "dwel_raintank_Aquawal_Be", "F_TEA_0045Af2_ses2001.xls"), sheetIndex = 1)

# 2. processing --------

rw01 <- rw01[4:653, c(1:4, 10:15)]

colnames(rw01) <- c('municd', 'muninm', 'level', 'total', 'rwt_no', 'rwt_yes', 'rwt_na', 'garden_no', 'garden_yes', 'garden_na')
str(rw01)
rw01 <- rw01[rw01$level %in% '6',]

rw01[,4:10] <- apply(rw01[,4:10], 2, function(x) as.numeric(gsub(',', '', x)))

rw01$rwt_nm <- rw01$rwt_no + rw01$rwt_yes

rw01$rwt_prop <- rw01$rwt_yes*100/rw01$rwt_nm

rw01$rwt_propna <- rw01$rwt_na*100/rw01$total

# 3. save output ----------


write.csv(
  rw01,
  here(pdir, "raintank_muni_Be", "raintank_muni_Be.csv"),
  row.names = F
)


