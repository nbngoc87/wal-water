# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------
### functions -----

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
  
}

### packages --------

loadpackage("here")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"

### paths to digitized data ----------

path_noga <- here(rdir, "water_consum_CILE_CILE", "Cnoga")
path_gritte <- here(rdir, "water_consum_CILE_CILE", "Ngritte")

lsf_noga <- list.files(path = path_noga)
lsf_gritte <- list.files(path = path_gritte)


## 2. compare digitized data ----------

compare <- data.frame(matrix(NA, nrow = 325, ncol = 15))
colnames(compare) <- c('annexe', 'Noga', 'Gritte', 'same', 'ncoldif', 'contract', 'date', 'record', 'rlconsp', 'days', 'billrec', 'bill', 'cr', 'mr', 'ims')

for (i in 1:nrow(compare)){
  compare$annexe[i] <- i
  loc1 <- grep(paste('annexe', i, '.csv', sep = ''), lsf_noga)
  if (length(loc1) ==1) {
    compare$Noga[i] <- 1
    df1 <- read.csv(paste(path_noga,lsf_noga[loc1],sep = '/'),sep=";", na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = "UTF-8")[,1:10]
    colnames(df1) <- c('contract', 'date', 'record', 'rlconsp', 'days', 'billrec', 'bill', 'cr', 'mr', 'ims') 
    df1 <- df1[rowSums(is.na(df1)) != ncol(df1), ]
    df1[,3:9] <- apply(df1[, 3:9], 2, function(x) as.numeric(gsub('\\.', '', gsub('\\.000$', '', x))))
    df1$ims <- tolower(df1$ims)
  } else {
    compare$Noga[i] <- 0
  }
  loc2 <- grep(paste('annexe', i, '.csv', sep = ''), lsf_gritte)
  if (length(loc2) ==1) {
    compare$Gritte[i] <- 1
    df2 <- read.csv(paste(path_gritte,lsf_gritte[loc2],sep = '/'),sep=";", na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = "UTF-8")[,1:10]
    colnames(df2) <- c('contract', 'date', 'record', 'rlconsp', 'days', 'billrec', 'bill', 'cr', 'mr', 'ims')
    df2 <- df2[rowSums(is.na(df2)) != ncol(df2), ]
    df2[,3:9] <- apply(df2[, 3:9], 2, function(x) as.numeric(gsub('\\.', '', gsub('\\.000$', '', x))))
    df2$ims <- tolower(df2$ims)
  } else {
    compare$Gritte[i] <- 0
  }
  if (compare$Noga[i] == 1 & compare$Gritte[i] == 1) {
    dif <- base::setdiff(df1, df2)
    if (length(dif) == 0) {
      compare$same[i] = 1
    } else {
      compare$same[i] = 0
      compare$ncoldif[i] = length(colnames(dif))
      for (j in colnames(dif)) {
        compare[i,j] <- paste(which(df1[,j] != df2[,j] | (!is.na(df1[,j]) & is.na(df2[,j])) | (is.na(df1[,j]) & !is.na(df2[,j]))), collapse = ', ')
      }
    }
  }
}


difference <- compare[compare$same %in% c(NA,0),]

write.csv(difference, here(pdir, "water_histcons_Wal", "CILE", "digitized_errors.csv"), na = '', row.names = F)

