# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------
### functions -----

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
  
}

# troublesome ones = 92, 200, 83, 145

loadannex <-
  function(path = path_csvs, filename = "annexe181.csv") {
    res <-
      read.csv(
        here(path, filename),
        sep = ";",
        na.strings = c("", "#NULL!"),
        strip.white = TRUE,
        encoding = "UTF-8"
      )[, 1:10]
    colnames(res) <-
      c('contract',
        'date',
        'record',
        'rlconsp',
        'days',
        'billrec',
        'bill',
        'cr',
        'mr',
        'ims')
    
    res <- res[rowSums(is.na(res)) != ncol(res),]
    res$date <- as.Date(res$date, format = "%d.%m.%Y")
    res[, 3:9] <-
      apply(res[, 3:9], 2, function(x)
        as.numeric(gsub('\\.', '', gsub('\\.000$', '', x))))
    res$ims <- tolower(res$ims)
    
    res$newcode <- 0
    res$newcode[res$mr %in% '22' | res$ims %in% 'ie'] <- 3
    res$newcode[res$mr %in% '21' | res$ims %in% 'ip'] <- 4
    res$newcode[res$mr %in% '3' | res$ims %in% c('dc', 'dv')] <- 1
    res$newcode[res$mr %in% '6' | res$ims %in% c('ec', 'ev')] <- 2
    res <- res[order(res$date, res$newcode), ]
    
    res <- res[!duplicated(res[, 1:3]), ]
    res$annex <- gsub("annexe", "", gsub(".csv", "", filename))
    res$cons_recal <- c(NA, diff(res$record))
    res$cons_dif <- res$cons - res$rlconsp
    res$ndays_recal <- c(NA, diff(res$date))
    ## in 2015, 2016 they change their system, when moving data, they marked it as if the family relocation but it's not true => needed this code to fix it
    
    n <- nrow(res)
    res$contrl1 <- c(res$contract[n], res$contract[1:(n - 1)])
    
    res$newcode[res$newcode %in% 1 & is.na(res$contract)] <- 0
    res$newcode[res$newcode %in% 2 & is.na(res$contrl1)] <- 0
    
    newfam <- c(F, (res$newcode %in% 1)[1:(n - 1)])
    res$famid <- as.numeric(as.factor(cumsum(newfam)))
    res <- res[!duplicated(res[, 2:3]), ]
    res
  }

annualize <- function(tmp) {
  n <- nrow(tmp)
  
  res <- data.frame(
    year = integer(),
    ndays = numeric(),
    csmpt = numeric(),
    famid = integer()
  )
  
  if (n > 1) {
    for (i in 2:n) {
      if (tmp$newcode[i] != 4) {
        cons <- tmp$record[i] - tmp$record[i - 1]
        ndays <- as.numeric(tmp$date[i] - tmp$date[i - 1])
        cspd <- cons / ndays
        date_seq <-
          seq(tmp$date[i - 1], (tmp$date[i] - 1), by = "1 day")
        restmp <- data.frame(table(year(date_seq)))
        colnames(restmp) <- c("year", "ndays")
        restmp$csmpt <- restmp$ndays * cspd
        restmp$famid <- tmp$famid[i]
        restmp$cspd <- cspd
        res <- rbind.data.frame(res, restmp)
        
      }
    }
    res <- res %>%
      group_by(year) %>%
      summarise(
        ndays = sum(ndays, na.rm = T),
        csmpt = sum(csmpt, na.rm = T),
        famid = paste(unique(famid), collapse = ",")
      )
    res$annex <- unique(tmp$annex)
    res$year <- as.integer(as.character(res$year))
    res$movin <- min(res$year)
    res$movout <- max(res$year)
    res$cspd <- res$csmpt * 1000 / res$ndays
  }
  res
}

### packages --------

loadpackage("here")
loadpackage("xlsx")
loadpackage("dplyr")
loadpackage("lubridate")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"


### historical consumption -----------

path_csvs <- here(pdir, "water_histcons_Wal", "CILE", "Corrected")

lsfile <- list.files(path = path_csvs)



### matricule ------------------

matcode <-
  read.xlsx(here(rdir, "water_consum_CILE_CILE", "ULG.xlsx"),
            sheetIndex = 1)[, c(1, 3:11, 13:16)]

colnames(matcode) <-
  c(
    'contract',
    'id',
    'qnb',
    'matricule',
    'annex',
    'cp',
    'mun',
    'city',
    'prov',
    'add',
    'install',
    'readmonth',
    'dateemmen',
    'ims'
  )

# 2. process -----------

annex_ls <- lapply(lsfile, loadannex, path = path_csvs)

## 2.1. data cleaning -------

### duplicate date but different record

dup_date <- sapply(annex_ls, function(x) {
  test <- x$ndays_recal %in% 0 & x$newcode != 4
  if (sum(test) > 0) {
    unique(x$annex)
  } else {
    NA
  }
})

dup_date[!(is.na(dup_date))] ## annex 192

annex_ls[[grep("annexe192.csv", lsfile)]] <-
  annex_ls[[grep("annexe192.csv", lsfile)]][-7, ]


### negative consumption

neg_cons <- sapply(annex_ls, function(x) {
  test <- x$cons_recal < 0  & x$newcode != 4
  if (sum(test, na.rm = T) > 0) {
    unique(x$annex)
  } else {
    NA
  }
})

neg_cons[!is.na(neg_cons)] ## annex 181, 34, 46

annex_ls[[grep("annexe34.csv", lsfile)]] <-
  annex_ls[[grep("annexe34.csv", lsfile)]][-(3:4), ]

annex_ls[[grep("annexe46.csv", lsfile)]] <-
  annex_ls[[grep("annexe46.csv", lsfile)]][-(10:11), ]

annex_ls[[grep("annexe181.csv", lsfile)]] <-
  annex_ls[[grep("annexe181.csv", lsfile)]][-16, ]

## 2.2. annualization --------

### consider each annex as a family

cile1 <- lapply(annex_ls, annualize)
cile1_df <- Reduce(rbind.data.frame, cile1)
cile1_df$annex <- as.numeric(cile1_df$annex)

cile1_df <- cile1_df[order(cile1_df$annex, cile1_df$year), ]

cile1_df <- left_join(cile1_df, matcode[, c("qnb", "annex")])

### split each annex into multiple families


family_ls <- lapply(annex_ls, function(x) {
  split(x, f = x$famid)
})

family_ls <- unlist(family_ls, recursive = F)


cile2 <- lapply(family_ls, annualize)
cile2_df <- Reduce(rbind.data.frame, cile2)
cile2_df$annex <- as.numeric(cile2_df$annex)
cile2_df$famid <- as.numeric(cile2_df$famid)


cile2_df <-
  cile2_df[order(cile2_df$annex, cile2_df$famid, cile2_df$year), ]

## remove duplicate qnb but different matricule

matcode <- matcode[!duplicated(matcode[, c("qnb", "matricule")]),]

dup_qnb <- matcode$qnb[duplicated(matcode$qnb)]

matcode <- matcode[!(matcode$qnb %in% dup_qnb),]

cile2_df <- inner_join(cile2_df, matcode[, c("qnb", "annex")])

cile2_df <- cile2_df[cile2_df$movin < 2014 & cile2_df$movout >2014,]



# 3. save data -------------


save(cile1_df,
     cile2_df,
     file = here(pdir, "water_histcons_Wal", "CILE", "cile.Rdata"))
