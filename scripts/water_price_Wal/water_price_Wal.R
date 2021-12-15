#' ---
#' title: "Wallonie water tariffs"
#' author: "Nguyen Bich Ngoc, Jacques Teller"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: github_document
#' always_allow_html: true
#' ---


#+ r setup, include = F, message = F
# notes from last run----------------------

# rmarkdown::render("scripts/water_price_Wal/water_price_Wal.R",output_file=paste0("water_price_Wal_", format(Sys.time(), "%y%m%d_%H%M"),".md"))

# 1. setup ---------

knitr::opts_chunk$set(fig.width = 3.543,
                      fig.height = 3.543,
                      dpi = 300)

## 1.1. load functions -----------------

### new functions ------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}

### packages ------

loadpackage("here")
loadpackage("xlsx")
loadpackage("dplyr")
loadpackage("reshape2")
loadpackage("stringi")
loadpackage("ggplot2")
loadpackage("cowplot")
loadpackage("scico")

source(here("scripts", "general_functions.R"))



## 1.2 load data --------

### data folder ---------
rdir <- "data/raw"
pdir <- "data/processed"


### price -----

sheetnames <- as.character(seq(2012, 2017))

# a <- sheetnames[4]

price <-
  data.frame(
    dtbtor = character(),
    CVD = numeric(),
    CVA = numeric(),
    year = numeric()
  )

for (a in sheetnames) {
  df <-
    read.xlsx(here(
      rdir,
      "water_price_Aquawal_Wal/Distributeur et CVD par an.xlsx"
    ),
    sheetName = a)
  
  df$Commune <- stri_trans_general(df$Commune, "Latin-ASCII")
  df$Commune[df$Commune %in% "Kelmis"] <- "La Calamine"
  df$Commune[df$Commune %in% "Amel"] <- "Ambleve"
  df$Commune[df$Commune %in% "Meix-devant-Virton"] <-
    "Meix-Devant-Virton"
  df$Commune[df$Commune %in% "Bullingen"] <- "Bullange"
  df$Commune[df$Commune %in% "Vresse-sur-Semois"] <-
    "Vresse-Sur-Semois"
  
  cvd_ac <-
    df[!(df$Commune %in% c("TOTAUX", "CVD")), c("Commune", "CVD.AC", "Admin..Communale")]
  colnames(cvd_ac) <- c("dtbtor", "CVD", "ac")
  cvd_ac <- cvd_ac[!is.na(cvd_ac$ac), ]
  cvd_ac <- cvd_ac[complete.cases(cvd_ac), 1:2]
  
  cvd_md <-
    df[df$Commune %in%  "CVD", c(
      "Commune",
      "SWDE",
      "AIEC",
      "AIEM",
      "CIESAC",
      "CILE",
      "IDEA",
      "IDEN",
      "IECBW",
      "IEG",
      "INASEP"
    )]
  cvd_md_lg <- melt(cvd_md, id.vars = "Commune")[, 2:3]
  colnames(cvd_md_lg) <- c("dtbtor", "CVD")
  
  tariff_md <- cvd_md_lg
  tariff_md$CVA <- as.numeric(names(which.max(table(df$CVA))))
  
  tariff_ac <-
    dplyr::left_join(cvd_ac, df[, c("Commune", "CVA")], by = c("dtbtor" = "Commune"))
  restmp <- rbind(tariff_ac, tariff_md)
  restmp$CVD <- as.numeric(restmp$CVD)
  restmp$year <- as.numeric(a)
  price <- rbind(price, restmp)
}

price$CVD[price$CVD %in% 0] <- NA
price$dtbtor[price$dtbtor %in% "IECBW"] <- "inBW"

### inflation rates ---------

inflation <-
  read.csv(
    here(
      rdir,
      "finance_inflation_Eurostat_Be/estat_tec00118_filtered_en.csv"
    ),
    na.strings = c("", "#NULL!"),
    strip.white = TRUE
  )[, 7:8]

colnames(inflation) <- c("year", "inflation")
inflation <- inflation[order(inflation$year, decreasing = T),]



# 2. value in 2018  ---------------
# inflation base year 2018 - last year in historical consumption data


inflation <- inflation[inflation$year < 2019,]

inflation$value18 <- 1

for (i in 2:nrow(inflation)) {
  inflation$value18[i] <-
    inflation$value18[i - 1] * (1 + inflation$inflation[i] / 100)
}

price <- merge(price, inflation[, c(1, 3)])

price$CVA18 <- price$CVA * price$value18

price$CVD18 <- price$CVD * price$value18


# 3. bill 70m3 ----------------------------

price$bill70 <-
  1.06 * ((20 * price$CVD + 30 * price$CVA) + 70 * 0.0125 + 30 * 0.5 * price$CVD + 40 * (price$CVD + price$CVA)
  )

price$bill7018 <- price$bill70 * price$value18

# 4. save output ------------

price <- price[order(price$dtbtor, price$year),]


write.csv(
  price,
  here(pdir, "water_price_Aquawal_Wal/water_price_Wal_12_17.csv"),
  row.names = F
)

# 5. plots ---------------

plotdf <-
  price[price$dtbtor %in%  c("CILE", "inBW", "INASEP", "SWDE"), ]

plotdf$dtbtor <-
  factor(plotdf$dtbtor, lvls(plotdf$dtbtor)[c(4, 1, 3, 2)])

## price without adjustment for inflation
#+ prwoinf, echo = F, message = F, fig.width =7.48, fig.height = 3.543

p1 <-
  ggplot(plotdf) +
  geom_line(aes(x = year, y = CVA, linetype = "CVA"), size = 0.8) +
  geom_line(aes(
    x = year,
    y = CVD,
    linetype = "CVD",
    col = dtbtor
  ), size = 0.8)  +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_scico_d(palette = "batlow",
                      end = 0.9,
                      begin = 0.1) +
  theme_kat() +
  labs(
    x = "Year",
    y = "Water tariff components (€)",
    color = "Utilities",
    linetype = "Tariff components"
  ) +
  ylim(0, 3)

p2 <-
  ggplot(plotdf, aes(x = year, y = bill70, col = dtbtor)) +
  geom_line(size = 0.8) +
  theme_kat() +
  scale_color_scico_d(palette = "batlow",
                      begin = 0.1,
                      end = 0.9) +
  labs(x = "Year", y = "Water bill for an example family (€)", color = "Utilities")  +
  ylim(100, 420)


legend <- get_legend(p1 +  guides(color = guide_legend(nrow = 1)))

pg <-
  plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    nrow = 1,
    align = "hv",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, 0.135))



## price with adjustment for inflation
#+ prwinf, echo = F, message = F, fig.width = 7.48, fig.height = 3.543

p1 <-
  ggplot(plotdf) +
  geom_line(aes(x = year, y = CVA18, linetype = "CVA"), size = 0.8) +
  geom_line(aes(
    x = year,
    y = CVD18,
    linetype = "CVD",
    col = dtbtor
  ), size = 0.8)  +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_scico_d(palette = "batlow",
                      end = 0.9,
                      begin = 0.1) +
  theme_kat() +
  labs(
    x = "Year",
    y = "Water tariff components (€)",
    color = "Utilities",
    linetype = "Tariff components"
  ) +
  ylim(0, 3)

p2 <-
  ggplot(plotdf, aes(x = year, y = bill7018, col = dtbtor)) +
  geom_line(size = 0.8) +
  theme_kat() +
  scale_color_scico_d(palette = "batlow",
                      begin = 0.1,
                      end = 0.9) +
  labs(x = "Year", y = "Water bill for an example family (€)", color = "Utilities")  +
  ylim(100, 420)


legend <- get_legend(p1  + guides(color = guide_legend(nrow = 1)))

pg <-
  plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    nrow = 1,
    align = "hv",
    labels = "AUTO"
  )

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, .135))



# compare with and wo adjustment for inflation
#+ cpwwoinf1, echo = F, message = F
ggplot(plotdf) +
  geom_line(aes(
    x = year,
    y = CVD,
    linetype = "CVD",
    col = dtbtor
  ), size = 0.8) +
  geom_line(aes(
    x = year,
    y = CVD18,
    linetype = "CVD18",
    col = dtbtor
  ), size = 0.8) +
  geom_line(aes(x = year, y = CVA, linetype = "CVA"), size = 0.8) +
  geom_line(aes(x = year, y = CVA18, linetype = "CVA18"), size = 0.8)  +
  scale_color_scico_d(palette = "batlow",
                      begin = 0.1,
                      end = 0.9) +
  theme_kat() +
  labs(
    x = "Year",
    y = "Water tariff components (€)",
    linetype = "Tariffs",
    color = "Utilities"
  )

#+ cpwwoinf2, echo = F, message = F

ggplot(plotdf, aes(x = year, col = dtbtor)) +
  geom_line(aes(y = bill70, linetype = "No"), size = 0.8) +
  geom_line(aes(y = bill7018, linetype = "Yes"), size = 0.8) +
  theme_kat() +
  scale_color_scico_d(palette = "batlow",
                      begin = 0.1,
                      end = 0.9) +
  labs(
    x = "Year",
    y = "Water bill for an example family (€)",
    color = "Utilities",
    linetype = "Inflation adjustment"
  )
