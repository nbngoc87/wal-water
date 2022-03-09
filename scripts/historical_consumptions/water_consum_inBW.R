# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------
### functions -----

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
  
}

annualize_f <- function(tmp = hcsmpt_ls[[1]]) {
  res <-
    data.frame(year = integer(),
               ndays = numeric(),
               csmpt = numeric())
  
  for (j in 2:nrow(tmp)) {
    cspd <- tmp$csmpt[j] / tmp$ndays[j]
    date_seq <-
      seq(tmp$date[j - 1], (tmp$date[j] - 1), by = "1 day")
    restmp <- data.frame(table(year(date_seq)))
    colnames(restmp) <- c("year", "ndays")
    restmp$csmpt <- restmp$ndays * cspd
    res <- rbind.data.frame(res, restmp)
  }
  
  res <- res %>%
    group_by(year) %>%
    summarise(ndays = sum(ndays, na.rm = T),
              csmpt = sum(csmpt, na.rm = T))
  res$refnb <- unique(tmp$refnb)
  res$client <- unique(tmp$client)
  res$qnb <- unique(tmp$qnb)
  res <-
    res[, c("qnb", "refnb", "client", "year", "ndays", "csmpt")]
  res$year <- as.integer(as.character(res$year))
  res$movin <- min(res$year)
  res$movout <- max(res$year)
  res
}

### packages --------

loadpackage("here")
loadpackage("xlsx")
loadpackage("lubridate")
loadpackage("dplyr")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"


### historical consumption -----------

hcsmpt <-
  read.xlsx(
    here(
      rdir,
      "water_consum_inBW_inBW",
      "in BW - Répondants pour ULiege v3 final.xlsx"
    ),
    sheetIndex = 2
  )

hcsmpt <- hcsmpt[, c(1, 2, 4, 7, 9)]

colnames(hcsmpt) <- c("refnb", "client", "date", "csmpt", "ndays")

### relocation --------------

reloc <-
  read.xlsx(
    here(
      rdir,
      "water_consum_inBW_inBW",
      "in BW - Répondants pour ULiege v3 final.xlsx"
    ),
    sheetIndex = 1
  )

reloc <- reloc[, c(1, 6, 14:16)]

colnames(reloc) <- c("qnb", "client", "name_ident", "add_ident", "nameadd_ident")



# 2. process ------------


## 2.1. reformat variables -------------

hcsmpt$date <- as.Date(hcsmpt$date)

hcsmpt <- left_join(hcsmpt, reloc[, 1:2])

hcsmpt_ls <- split(hcsmpt, hcsmpt$client)

hcsmpt_ls <- lapply(hcsmpt_ls, function(x) {
  x <- x[order(x$date), ]
  x <- x[!duplicated(x), ]
  x
})




## 2.2. remove questionable data ------------

## one client number which links to two different ref numbers

nref <- sapply(hcsmpt_ls, function(x)
  nlvls(x$refnb))

hcsmpt_ls <- hcsmpt_ls[nref %in% 1]


## ndays doesn't match the difference between 2 consecutive dates

ndays_nomat <- sapply(hcsmpt_ls, function(x) {
  sum(x$ndays[2:nrow(x)] != diff(x$date))
})

names(hcsmpt_ls)[ndays_nomat > 0] # "63383"  "73049" "114134" "134396" "147830"


hcsmpt_ls <- lapply(hcsmpt_ls, function(x) {
  ndays_nomat <- x$ndays[2:nrow(x)] != diff(x$date)
  ind <- which(ndays_nomat == T)[1]
  if (!is.na(ind)) {
    x <- x[-(ind + 1), ]
  }
  x
})

## 2.3. annualization ------------

## create row for previous date

hcsmpt_ls <- lapply(hcsmpt_ls, function(x) {
  x <- rbind(x[1, ], x)
  x$csmpt[1] <- NA
  x$ndays[1] <- NA
  x$date[1] <- x$date[2] - x$ndays[2]
  x$year <- year(x$date)
  x
})


## annulization

inBW_ls <- lapply(hcsmpt_ls, annualize_f)

## 2.4. remove families with changed info ---------
## family which might have moved due to different address or name on contracts


reloc_client <- lvls(reloc$client[reloc$add_ident %in% 0])

inbw1_ls <- inBW_ls[!(names(inBW_ls) %in% reloc_client)]


inbw1_df <- Reduce(rbind.data.frame, inbw1_ls)

inbw1_df$cspd <- inbw1_df$csmpt * 1000 / inbw1_df$ndays

reloc_client <- lvls(reloc$client[reloc$nameadd_ident %in% 0])

inbw2_ls <- inBW_ls[!(names(inBW_ls) %in% reloc_client)]


inbw2_df <- Reduce(rbind.data.frame, inbw2_ls)

inbw2_df$cspd <- inbw2_df$csmpt * 1000 / inbw2_df$ndays


# 3. save data ----------



save(inbw1_df, inbw2_df, file = here(pdir, "water_histcons_Wal", "inbw.Rdata"))
