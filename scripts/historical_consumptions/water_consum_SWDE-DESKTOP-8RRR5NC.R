# notes from last run----------------------



# 1. setup ---------

## 1.1. load functions -----------------
### functions -----

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
  
}

reloc_f <- function(a,
                    hcsmptdf = hcsmpt_lg,
                    relocdf = reloc) {
  tmp <- hcsmptdf[hcsmptdf$installation %in% a,]
  tmp <- tmp[order(tmp$year),]
  tmp$fam_id <- 1
  fam_id <- 1
  
  reloc_tmp <- relocdf[relocdf$installation %in% a,]
  
  if (nrow(reloc_tmp) > 0) {
    reloc_tmp <- reloc_tmp[order(reloc_tmp$reloc_date),]
    for (j in seq_len(nrow(reloc_tmp))) {
      reloc_y <- year(reloc_tmp$reloc_date[j])
      n <- nrow(tmp[tmp$year < (reloc_y + 1),])
      
      if (n < nrow(tmp)) {
        tmp <-
          rbind.data.frame(tmp[1:n,], tmp[n,],  tmp[(n + 1):nrow(tmp),])
        
      } else {
        tmp <- rbind.data.frame(tmp[1:n,], tmp[n,])
      }
      
      tmp$ndays[n] <-
        as.numeric(reloc_tmp$reloc_date[j] - as.Date(paste(reloc_y, "01-01", sep = "-")))
      tmp$ndays[n + 1] <-
        as.numeric(as.Date(paste((reloc_y + 1), "01-01", sep = "-")) - reloc_tmp$reloc_date[j])
      tmp[(n + 1):nrow(tmp), "fam_id"] <- fam_id + 1
      fam_id <- fam_id + 1
    }
  }
  
  tmp$movin <- min(tmp$year)
  tmp$movout <- max(tmp$year)
  for (i in lvls(tmp$fam_id)) {
    tmp$movin[tmp$fam_id %in% i] <- min(tmp$year[tmp$fam_id %in% i])
    tmp$movout[tmp$fam_id %in% i] <-
      max(tmp$year[tmp$fam_id %in% i])
  }
  
  tmp
}



### packages --------

loadpackage("here")
loadpackage("xlsx")
loadpackage("reshape2")
loadpackage("ggplot2")
loadpackage("scico")
loadpackage("lubridate")

source(here("scripts", "general_functions.R"))

## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"


### historical consumption -----------

hcsmpt <-
  read.xlsx(here(rdir, "water_consum_SWDE_SWDE", "Données ULG_2.xlsx"),
            sheetIndex = 1)




### relocate -----------


reloc <-
  read.xlsx(here(rdir, "water_consum_SWDE_SWDE", "Données ULG_2.xlsx"),
            sheetIndex = 2)


# 2. process data -------------

## 2.1. historical consumption -------------

colnames(hcsmpt) <-
  c(
    "qnb",
    "cp",
    "muninm",
    "address",
    "installation",
    "partenaire",
    paste("cs", 2009:2019, sep = "_")
  )

hcsmpt <- hcsmpt[!is.na(hcsmpt$installation),]

dup_ins <-
  lvls(hcsmpt$installation[duplicated(hcsmpt$installation)])

hcsmpt <- hcsmpt[!(hcsmpt$installation %in% dup_ins),]

hcsmpt_lg <-
  reshape2::melt(hcsmpt,
                 id.vars =  c("qnb", "cp", "muninm", "address", "installation", "partenaire"))

hcsmpt_lg$year <- as.integer(gsub("cs_", "", hcsmpt_lg$variable))

hcsmpt_lg$csmpt <- as.numeric(hcsmpt_lg$value)

hcsmpt_lg$installation <- as.character(hcsmpt_lg$installation)

sample <- sample(lvls(hcsmpt_lg$installation), 10)
sample_df <- hcsmpt_lg[hcsmpt_lg$installation %in% sample, ]

ggplot(hcsmpt_lg, aes(x = year, y = csmpt))  +
  stat_summary(
    geom = "line",
    fun = "mean",
    size = 2,
    col = "black"
  ) +
  geom_line(data = sample_df, aes(x = year, y = csmpt, color = installation)) +
  theme_kat(legend.position = "bottom") +
  scale_x_continuous(breaks = 2009:2019) +
  scale_color_scico_d(palette = "lapaz",
                      begin = 0.2,
                      end = 0.8)

hcsmpt_lg <-
  hcsmpt_lg[order(hcsmpt_lg$installation, hcsmpt_lg$year),]

hcsmpt_lg$ndays <-
  as.numeric(yday(as.Date(paste(
    hcsmpt_lg$year, "12-31", sep = "-"
  ))))

hcsmpt_lg$cspd <- hcsmpt_lg$csmpt * 1000 / hcsmpt_lg$ndays

swde1_df <- hcsmpt_lg[, c("qnb",
                          "installation",
                          "partenaire",
                          "year",
                          "csmpt",
                          "ndays",
                          "cspd")]

## 2.2. taken into account relocation -------------------
colnames(reloc) <- c("partenaire", "installation", "reloc_date")

reloc$reloc_date <- as.Date(reloc$reloc_date, format = "%d/%m/%Y")


res <- lapply(lvls(hcsmpt_lg$installation), reloc_f)

res_df <- Reduce(rbind.data.frame, res)

swde2_df <-
  res_df[, c(
    "qnb",
    "installation",
    "partenaire",
    "year",
    "csmpt",
    "ndays",
    "cspd",
    "fam_id",
    "movin",
    "movout"
  )]

swde2_df <- swde2_df[swde2_df$movin <=2014 & swde2_df$movout >=2014,]


# 3. save results ------------------

save(swde1_df, swde2_df, file = here(pdir, "water_histcons_Wal", "swde.Rdata"))

