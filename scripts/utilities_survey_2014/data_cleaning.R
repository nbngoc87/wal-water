# notes from the last run -----------------

# 1. setup ---------

## 1.1. load functions -----------------
### new functions ------

loadpackage <- function(x) {
  if (!suppressWarnings(require(x, character.only = TRUE)))
    list(install.packages(x, dep = TRUE), library(x, character.only = TRUE))
}


hhsize <-
  function(year = 2014,
           data = processed,
           age_brks = c(0, 20, 65),
           ocp = T) {
    birth_year <- data[, grep("by_", colnames(data))]
    occupancy <- data[, grep("ocp_", colnames(data))]
    age <- year - birth_year
    age[age < 0] <- NA
    
    data$year <- rep(year, nrow(data))
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
      res <- data[, grep("hhspo_", colnames(data))]
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
      res <- data[, grep("hhs_", colnames(data))]
    }
    res
  }



nb_room <- function(char) {
  df <- org[, grep(pattern = paste0("Q18_.*_", char), names(org))]
  df <- df == "Oui"
  sumtrue <- apply(df, 1, sum)
  maxpos <- apply(df, 1, function(x)
    which.max(rev(x)))
  res <-
    factor(maxpos,
           levels = lvls(maxpos)[4:1],
           labels = c("0", "1", "2", "3 or more"))
  res[sumtrue < 1] <- NA
  res
}

tech3lvs <- function(x, y) {
  res <- as.character(x)
  res[x %in% "yes" & y %in% "yes"] <- "recent_replaced"
  res <- factor(res, levels = c("no", "yes", "recent_replaced"))
}

### packages --------

loadpackage("here")
loadpackage("stringi")
loadpackage("sf")
loadpackage("dplyr")
loadpackage("ggplot2")
loadpackage("reshape2")

source(here("scripts", "general_functions.R"))



## 1.2 load data --------

### data folder
rdir <- "data/raw"
pdir <- "data/processed"

### survey data ------------

var <-
  read.csv(
    here(
      pdir,
      "utilities_survey_Aquawal_CEHD_Wal/surv14_var_AquaWal_slt.csv"
    ),
    na.strings = c("", "#NULL!"),
    strip.white = TRUE
  ) # import csv file with variables information

slvar <- var$Location # columnes number of needed variables

org <-
  read.csv(
    here(
      pdir,
      "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_AquaWal.csv"
    ),
    na.strings = c("", "#NULL!"),
    strip.white = TRUE
  )[, slvar] # import original data Cedric sent, select only needed columns using slvar

org <- org[order(org$id), ]

### coordinates ------------

surv14_coord <-
  st_read(
    here(
      pdir,
      "utilities_survey_Aquawal_CEHD_Wal/addresses/coordinates/surv14_coordinates/surv14_coordinates.shp"
    )
  )

surv14_coord <-
  st_drop_geometry(surv14_coord[, c("id", "CD_SECTOR")])

surv14_coord$id <- as.integer(surv14_coord$id)

colnames(surv14_coord)[2] <- "ststcd"


### parcel areas ----------

parcel <-
  read.csv(here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_parcel.csv"
  ))

### statistical sectors -----------

load(file = here(pdir, "admin_border_Wal/admin_ss_Wal.Rdata"))

### water price -----------

price <-
  read.csv(here(pdir, "water_price_Aquawal_Wal/water_price_Wal_12_17.csv"))

price <-
  price[price$year %in% 2014, c("dtbtor", "CVD", "CVA", "bill70")]

### built-up density -------------

builtup <-
  load(file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_builtup.Rdata"
  ))

bltup10 <- builtup_ls[names(builtup_ls) == "LU2010_5cls_x25"][[1]]

bltup10$id <- as.integer(bltup10$id)


# 2. general --------------------

## 2.1. merging/filtering ---------

### id ---------

processed <- org[, c("id", "Numéro_questionnaire")]

# summary(processed)



### qnb -------

colnames(processed)[2] <- "qnb"


# table(processed[processed$qnb < 1 | processed$qnb > 30000, "qnb"])

# table(processed[as.factor(processed$qnb) %in% factor(processed[duplicated(processed$qnb), "qnb"]), "qnb"]) # qnb 13685 was double



### moving in year ---------

processed$mvinyr <- org$Q20_ann_occup

# summary(processed$mvinyr[processed$mvinyr <= 2014])

# summary(as.factor(processed$mvinyr[processed$mvinyr > 2014]))
# df <- org[processed$mvinyr > 2014 & complete.cases(processed$mvinyr), c("id", "Q20_ann_occup", "Q1_conso_eau", "Consoeauverif" )]

# # some people move in 2015 => cannot say about consumption in 2014

### use for professional purpose ------
processed$prfuse <-
  factor(org$Q11_prof_ville, labels = c("no", "yes"))

# summary(processed$prfuse)

# hist(org[org$Q11_prof_ville %in% "Oui",]$Consoeauverif)
# hist(org[org$Q11_prof_ville %in% "Non",]$Consoeauverif)


## 2.2. verified consumption -------------

processed$csmptv <- org$Consoeauverif

# # remove household who moved in after 2014 or use distribution water for professional purpose

processed$csmptv[processed$prfuse %in% "yes" |
                   processed$mvinyr > 2014] <- NA


# summary(processed$csmptv)


## 2.3. weight -------------------
# # use weight of Cedric, there is good documentation of weights in the rapport-final-aquawal-cehd-v8 pages 21-27
# # after using weight, the proportion of observations per class of province, owner/renter, age of preference person, age of dwelling, dwelling type (4-facade, ..., appartement) seems similar to observed in the whole region from other reports

processed$weight <- org$POIDS_Geo_Stat_AgeP1_AgeL



# 3. other water sources ---------
# # information based mainly on question 11 in the questionnaire, except for the rain water tank was taken from the question 24

## 3.1. use of other sources (uos)--------

uos <- org[, grep("Q11", colnames(org))]
uos <- uos[,-grep("ville", colnames(uos))]

colnames(uos) <-
  stri_replace_all_fixed(
    colnames(uos),
    c(
      "Q11",
      "boiss",
      "cafe",
      "repas",
      "vaiss",
      "linge",
      "nett",
      "jard",
      "voit",
      "ext",
      "pisc",
      "bout",
      "pluie",
      "puits",
      "autre"
    ),
    c(
      "uos",
      "drink",
      "coffee",
      "meal",
      "dishes",
      "washing",
      "idclean",
      "garden",
      "car",
      "odclean",
      "pool",
      "bottle",
      "rain",
      "well",
      "other"
    ),
    vectorize_all = F
  )

uos <-
  apply(uos, 2, function(x)
    stri_replace_all_fixed(x, c("Non", "Oui"), c("no", "yes"), vectorize_all = F))

processed <- cbind.data.frame(processed, uos)

## 3.2. private wel ------------------


### private well binary ---------
# # Cedric created this variable based on whether that house use well water for any purpose (both indoor and outdoor)
# # did check again using original information from question 11, the count of using well water and not using well water are the same
# # pvwlbn = private well binary: yes for using well water (either indoor or outdoor or both), no for not using well water at all

processed$pvwlbn  <- org$Puits

processed$pvwlbn <-
  factor(processed$pvwlbn, labels = c("no", "yes"))

# summary(processed$pvwlbn)

### private well indoor ---------

uos_well <- uos[, grep("well" , colnames(uos))]

pvwlid <- apply(uos_well[, 1:8], 1, function(x)
  sum(x == "yes") != 0)

processed$pvwlid <- factor(pvwlid, labels = c("no", "yes"))

# summary(processed$pvwlid)


### private well outdoor------

pvwlod <-
  apply(uos_well[, 9:12], 1, function(x)
    sum(x == "yes") != 0)

processed$pvwlod <- factor(pvwlod, labels = c("no", "yes"))

# summary(processed$pvwlod)

### private well 4 levels --------

processed$prvwel <- paste0(processed$pvwlid, processed$pvwlod)

processed$prvwel <-
  factor(processed$prvwel, labels = c("no", "outdoor", "indoor", "both")) # 2018 no, 40 outdoor, 6 indoor, 55 both

# summary(processed$prvwel)

## 3.3. rain water tank ------------

### rainwater tank --------

processed$rwtank <-
  factor(org$Q24_cit_pres, labels = c("no", "yes")) # whether they have rain water tank

# summary(processed$rwtank)

### rainwater tank replace -----------

processed$rwtkrp <-
  factor(org$Q24_cit_rempl, labels = c("no", "yes")) # whether the rain water tank replace after 2009

# summary(processed$rwtkrp)
#
# df <- processed[processed$rwtank %in% "no" & processed$rwtkrp %in% "yes",]
# write.table(t(df$id), "clipboard", sep = "\t", row.names = F, col.names = F) # id of household without rain water tank but saying that they replaced rain water tank since 2009

### rainwater use binary ----------

uos_rain <- uos[, grep("rain" , colnames(uos))]

rwusbn <- apply(uos_rain, 1, function(x)
  sum(x == "yes") != 0)

processed$rwusbn <- factor(rwusbn, labels = c("no", "yes"))

# summary(rwusbn)
# summary(processed$rwusbn)

# write.table(table(processed$rwtank, processed$rwusbn), "clipboard", sep = "\t", row.names = T, col.names = T)
# chisq.test(processed$rwtank, processed$rwusbn, correct = F)


### rainwater use indoor --------

rwusid <- apply(uos_rain[, 1:8], 1, function(x)
  sum(x == "yes") != 0)

processed$rwusid <- factor(rwusid, labels = c("no", "yes"))

# summary(processed$rwusid)

### rainwater use outdoor -------------

rwusod <-
  apply(uos_rain[, 9:12], 1, function(x)
    sum(x == "yes") != 0)

processed$rwusod <- factor(rwusod, labels = c("no", "yes"))

# summary(processed$rwusod)

### rainwater use 4 cat ----------

processed$rwtuse <- paste0(processed$rwusid, processed$rwusod)

processed$rwtuse <-
  factor(processed$rwtuse, labels = c("no", "outdoor", "indoor", "both")) # 1111 no, 465 outdoor, 24 indoor, 519 both

# summary(processed$rwtuse)

## 3.4. other sources ------


uos_oth <- uos[,-grep("bottle", colnames(uos))]

### other sources indoor

otsrid <- apply(uos_oth[, 1:24], 1, function(x)
  sum(x == "yes") != 0)

processed$otsrid <- factor(otsrid, labels = c("no", "yes"))

# summary(processed$otsrid)


otsrod <-
  apply(uos_oth[, 25:36], 1, function(x)
    sum(x == "yes") != 0)

processed$otsrod <- factor(otsrod, labels = c("no", "yes"))

# summary(processed$otsrod)


# 4. spatial -----------------

## 4.1 administrative names/codes ----------


processed <- left_join(processed, surv14_coord)

processed <- left_join(processed, st_drop_geometry(wal_stst)[, -1])


# summary(processed)

## 4.2. distributors --------

processed$dtbtor <- org$Distributeur
processed$dtbtor[processed$dtbtor %in% "IECBW"] <- "inBW"
# summary(factor(org$Distributeur))

# # distributer has very few missings - which is a good thing
# # only 4 distributers: CILE, inBW (inBW), INASEP, SWDE
# # 3 houses get their water from commune distributer (Vresse-Sur-Semois, Amel, Theux) although actually only the one in Theux can be mapped.

processed$dtbtor[processed$dtbtor %in% "Commune" &
                   is.na(processed$muninm)] <- NA
processed$dtbtor[processed$muninm %in% "Theux"] <- "Theux"



# # although some family with distributor = SWDE but the addresses do not belong to SWDE service area.
processed$dtbtor[processed$muninm %in% "Ouffet"] <- NA
processed$dtbtor[processed$muninm %in% "Villers-La-Ville"] <- "inBW"

# table(processed$dtbtor)
processed$dtbtor <- as.factor(processed$dtbtor)

## 4.3. built-up density ------------
# 5 categories (actually 6 with 0 built-up), from Ahmed, for 2010

processed <- left_join(processed, bltup10)

# summary(processed[,77:81])


# 5. socio-demographic ------------------

## 5.1. household size ---------------
# # Use only birthyear information for household size because it needed for calculate number of equivalent adults which inturn is used to calculate income per equivalent adult which is a very important (and always significant variable) => even other variable may provide more info for householdsize, these info will be removed anyway when fitting model with income

birth_year <- org[, grep("Q26.*naiss", colnames(org))]
colnames(birth_year)[1:ncol(birth_year)] <-
  paste0("by_p", 1:(ncol(birth_year)))

processed <- cbind.data.frame(processed, birth_year)

# # personal occupation: whether they live in that house the whole week (true for nearly 90% of them) (ocp = occupancy)
occupancy <- org[, grep("Q26.*_occ", colnames(org))]

occupancy <-
  apply(occupancy, 2, function(x)
    as.numeric(stri_replace_all_fixed(
      x,
      c(
        "Toute la semaine",
        "La moitié de la semaine ou plus",
        "Moins de la moitié de la semaine"
      ),
      c("1", "0.75", "0.25"),
      vectorize_all = F
    )))

colnames(occupancy) <- paste0("ocp_p", 1:(ncol(occupancy)))

processed <- cbind.data.frame(processed, occupancy)


hhs14 <-
  hhsize(
    year = 2014,
    data = processed,
    age_brks = c(0, 18, 66),
    ocp = F
  )

hhspo14 <-
  hhsize(
    year = 2014,
    data = processed,
    age_brks = c(0, 18, 66),
    ocp = T
  )

processed <- cbind.data.frame(processed, hhs14, hhspo14)

processed$hhs_18_95 <- processed$hhs_18_65 + processed$hhs_66_95

processed$hhspo_18_95 <- processed$hhspo_18_65 + processed$hhspo_66_95


processed$eqadlt <-
  1 + (processed$hhs_18_95 - 1) * 0.5 + processed$hhs_0_17 * 0.3

processed$eqadpo <-
  ifelse(
    processed$hhspo_18_95 > 1 ,
    1 + (processed$hhspo_18_95 - 1) * 0.5 + processed$hhspo_0_17 * 0.3,
    processed$hhspo_18_95 + 0.3 * processed$hhspo_0_17
  )


## 5.2. household head ------------------------------------

### select ---------

age <- 2014 - birth_year

refper <- apply(age, 1, function(x)
  which.max(x)[1])

# table(refper)

rpage <-
  sapply(1:nrow(age), function(x)
    ifelse(is.na(refper[x]), NA, age[x, refper[x]]))

# summary(rpage)

refper[rpage < 18] <- NA
rpage[rpage < 18] <- NA

processed$refper <- refper

### age -------

processed$rpage <- rpage

# summary(processed$rpage)

### gender -------

rpgen <-
  sapply(1:nrow(age), function(x)
    ifelse(is.na(refper[x]), NA, as.character(org[x, grep(paste0(refper[x], "_sexe") , colnames(org))])))

processed$rpgen <- factor(rpgen, labels = c("f", "m"))


### job ---------

rpjob <-
  sapply(1:nrow(age), function(x)
    ifelse(is.na(refper[x]), NA, as.character(org[x, grep(paste0(refper[x], "_act") , colnames(org))])))

rpjob <-
  stri_replace_all_fixed(
    rpjob,
    c(
      "Aucune des catégories précitées",
      "Cadre",
      "Employé du secteur privé",
      "Fonctionnaire / Enseignant",
      "Indépendant",
      "Ouvrier",
      "Profession libérale (médecin, avocat, etc.)",
      "Sans emploi rémunéré"
    ),
    c(
      "other",
      "manager",
      "private sector",
      "state employee",
      "independent",
      "worker",
      "freelancer",
      "unemployed"
    ),
    vectorize_all = F
  )
table(rpjob)

rpjot <-
  sapply(1:nrow(age), function(x)
    ifelse(is.na(refper[x]), NA, as.character(org[x, grep(paste0(refper[x], "_inact") , colnames(org))])))

lvls(rpjot)

rpjob[rpjot %in% "(Pré)pensionné"] <- "(pre)retired"
rpjob[rpjot %in% "Autre"] <- "other"
rpjob[rpjot %in% "Demandeur d'emploi"] <- "unemployed"
rpjob[rpjot %in% "En incapacité maladie/ invaldité"] <- "incapable"
rpjob[rpjot %in% "Etudiant/Elève"] <- "student"
rpjob[rpjot %in% "Femme/homme au foyer"] <- "housewife/husband"

processed$rpjob <- as.factor(rpjob)

# ggplot(processed) + geom_bar(aes(x= rpjob)) + xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))

### education ----------

rpedu <-
  sapply(1:nrow(age), function(x)
    ifelse(is.na(refper[x]), NA, as.character(org[x, grep(paste0(refper[x], "_dipl") , colnames(org))])))


processed$rped6c <-
  car::recode(
    rpedu,
    "c('Primaire/pas de diplôme','Secondaire inférieur (3ème secondaire)')='before highschool'; 'Secondaire supérieur général (6ème secondaire)' = 'highschool'; 'Professionnel' = 'professional'; 'Technique' = 'technique'; 'Supérieur non universitaire' = 'higher not university'; 'Universitaire' = 'university'"
  )

processed$rped6c <-
  factor(processed$rped6c, levels = lvls(processed$rped6c)[c(1, 3, 5, 4, 2, 6)])

# summary(factor(processed$rped6c))

processed$rped4c <-
  car::recode(
    processed$rped6c,
    "c('professional','technique') = 'vocation'; c('higher not university', 'university') = 'university'"
  )

processed$rped4c <-
  factor(processed$rped4c, levels = lvls(processed$rped4c)[c(1, 2, 4, 3)])


# ggplot(processed) + geom_bar(aes(x= rped4c)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))


## 5.3. income ----------------

# # assign 125 for income < 250, midpoint for other groups, and 5250 for income > 5000

income <-
  factor(org$Q28_rev, lvls(org$Q28_rev)[c(12, 5, 11, 2:4, 6:10, 1)], labels = c(125, 375, seq(750, 5250, 500)))



processed$income <-
  as.numeric(as.character(income)) # convert to numeric

# summary(factor(org$Q28_rev))
# summary(as.factor(processed$income))

processed$inceqa <-
  processed$income * 12 / processed$eqadlt # income per equivalent adults


# # income equivalent cat

# # categorize income base on Wallonia definition of logement moyen (0, 12900, 25700, 39900)

processed$iceqac1 <-
  cut(
    processed$inceqa,
    breaks = c(0, 12900, 25700, 39900, max(processed$inceqa, na.rm = T)),
    labels = c("precarious", "modest", "average", "higher")
  )

# ggplot(processed, aes(x = inceqa)) + geom_histogram() + xlab("income per equivalent adult") + geom_vline(xintercept = c(12900, 25700, 39900), color = "red", linetype = "longdash")+ theme_kat()
#
# ggplot(processed) + geom_bar(aes(x= iceqac1)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))

# categorize income based on Wallonia definition but calculate after tax money
# (using online calculator and an average municipal tax 7%). Cutoff value is
# then 11200, 19600, 26800 can use postcode to change the cut off value - but
# then might be increase number of missing. Also it will complicated my life
# unnecessary

processed$iceqac2 <-
  cut(
    processed$inceqa,
    breaks = c(0, 11200, 19600, 26800, max(processed$inceqa, na.rm = T)),
    labels = c("precarious", "modest", "average", "higher")
  )

# ggplot(processed, aes(x = inceqa)) + geom_histogram(bins = 100, fill = "white", color = "black") + xlab("income per equivalent adult") + geom_vline(xintercept = c(11200, 19600, 27000), color = "red", linetype = "longdash")+ theme_kat()
#
# ggplot(processed) + geom_bar(aes(x= iceqac2)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))

# levels(factor(org$Code_postal))


# # income cat base on single household or multi member households and number of children below 20 years old

inc_woc <- processed$income * 12 - 2400 * processed$hhs_0_17

single <- processed$hhs_18_95 < 2


inc_sin <- single * inc_woc
inc_sin[inc_sin %in% 0] <- NA
inc_sin_cat <-
  cut(
    inc_sin,
    breaks = c(0, 12900, 25700, 39900, max(inc_sin, na.rm = T)),
    labels = c("precarious", "modest", "average", "higher")
  )


inc_fam <- inc_woc * (1 - single)
inc_fam[inc_fam %in% 0] <- NA
inc_fam_cat <-
  cut(
    inc_fam,
    breaks = c(0, 17500, 32100, 48200, max(inc_fam, na.rm = T)),
    labels = c("precarious", "modest", "average", "higher")
  )

processed$inccat <- inc_fam_cat

processed$inccat[is.na(inc_fam_cat)] <-
  inc_sin_cat[is.na(inc_fam_cat)]

# ggplot(processed) + geom_bar(aes(x= iceqac1)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))
#
# ggplot(processed) + geom_bar(aes(x= inccat)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))


## 5.4. difficult paying bill -----

# # whether receive help from Fonds Social de l’Eau

processed$fseaid <- factor(org$Q8_fd_eau, labels = c("no", "yes"))

# # problem paying bill due to financial problem

processed$dfpay <-
  factor(org$Q10_probl_fact_eau, labels = c("not user", "no", "yes"))

# # Taux effort hydrique - percentage of water bill/income (probably calculated by Cedric)

processed$TEH <-
  org$TauxEffortHydrique # Average = 1.28, median = 1.03


# 6. dwelling --------------------

# colnames(org)[c(77:118, 39:42, 119)]
## 6.1. Home-ownership --------

processed$dwowsh <-
  factor(
    org$Q14_stat_occup,
    lvls(org$Q14_stat_occup)[c(3, 4, 2, 1)],
    labels = c(
      "owner",
      "owner - mortgage loan",
      "renter - private sector",
      "renter - social or public"
    )
  )

# ggplot(processed) + geom_bar(aes(x= dwowsh)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))


## 6.2. type --------

processed$dwltyp <-
  factor(
    org$Q15_type_log,
    lvls(org$Q15_type_log)[c(3, 4, 5, 1, 2)],
    labels = c(
      "4 facades",
      "3 facades",
      "2 facades",
      "appartment/studio",
      "other"
    )
  )

# # manually add value for the one with missing Q15_type_log, but have info in Q15_autre_type_log

processed$dwltyp[processed$id %in% c(221, 1495)] <- "2 facades"
processed$dwltyp[processed$id %in% c(886, 1256, 2478, 2871, 2814, 2583, 2304, 2431)] <-
  "4 facades"
processed$dwltyp[processed$id %in% c(1952, 1399, 1884)] <- NA
processed$dwltyp[processed$id %in% 2557] <- "appartment/studio"

# ggplot(processed) + geom_bar(aes(x= dwltyp)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))


## 6.4. construction year ------------


# # dwcsy9 = dwelling construction year 9 classes

processed$dwcsy9 <-
  factor(
    org$Q17_ann_constr,
    lvls(org$Q17_ann_constr)[c(2:9, 1)],
    labels = c(
      "Before 1875",
      "1875-1918",
      "1919-1945",
      "1946-1970",
      "1971-1980",
      "1981-1990",
      "1991-2000",
      "2001-2005",
      "2006 and after"
    )
  )


# # dwcsy6 dwelling construction year 6 classes - based on Cedric advice + histogram
processed$dwcsy6 <-
  car::recode(
    processed$dwcsy9,
    "c('Before 1875', '1875-1918') = 'Before 1919'; c('1971-1980', '1981-1990') = '1971-1990'; c('2001-2005', '2006 and after') = '2001 and after'"
  )

processed$dwcsy6 <-
  factor(processed$dwcsy6, levels = lvls(processed$dwcsy6)[c(6, 1:5)])

# # dwcsy5 dwelling construction year 5 classes - based on Cedric advice
processed$dwcsy5 <-
  car::recode(
    processed$dwcsy9,
    "c('Before 1875', '1875-1918', '1919-1945') = 'Before 1945'; c('1971-1980', '1981-1990') = '1971-1990'; c('2001-2005', '2006 and after') = '2001 and after'"
  )

processed$dwcsy5 <-
  factor(processed$dwcsy5, levels = lvls(processed$dwcsy5)[c(5, 1:4)])

# ggplot(processed) + geom_bar(aes(x= dwcsy9)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))
#
# ggplot(processed) + geom_bar(aes(x= dwcsy6)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))
#
# ggplot(processed) + geom_bar(aes(x= dwcsy5)) +xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))


## 6.5. living area ---------

### reported living area ------

processed$livara <- org$Q16_superf_hab_COR

# # dataset contain the observation which has the reported data != the corrected area

# df <- org[org$Q16_superf_hab != org$Q16_superf_hab_COR & !is.na(org$Q16_superf_hab_COR),]

# # calculate living area per equivalent adults: note that the coefficient 0.5 for an extra adults and 0.3 for an extra children was developped for income and might not be true for living area

### living area per cap ------

processed$lareqa <- processed$livara / processed$eqadlt

# ggplot(processed, aes(x = livara)) + geom_histogram() + xlab("living area") + theme_kat()
#
# ggplot(processed, aes(x = lareqa)) + geom_histogram() + xlab("living area per equivalent adults") + theme_kat()

### built-up area & parcel area -------

processed <- left_join(processed, parcel)

## 6.6. number of room -------------

# # my function to calculate the number of room (0, 1, 2, 3 or more)
# # some household indicate yes for more than 1 options (e.g 0 room & 1 room) => select the first yes from the right
# # some household did not indicate anything => missing???
# # the function could not be used elsewhere, might think to make it more altsrc



processed$nbktch <- nb_room("cuis")
processed$nblvrm <- nb_room("sal")
processed$nbbdrm <- nb_room("cham")
processed$nbbtrm <- nb_room("sdb")
processed$nbtoil <- nb_room("toil")

# ggplot(processed) + geom_bar(aes(x= nbktch)) +xlab("number of kitchen") + theme_kat() + theme(axis.text.x = element_text(angle = 90))
#
# ggplot(processed) + geom_bar(aes(x= nblvrm)) +xlab("number of living room") + theme_kat() + theme(axis.text.x = element_text(angle = 90))
#
# ggplot(processed) + geom_bar(aes(x= nbbdrm)) +xlab("number of bedroom") + theme_kat() + theme(axis.text.x = element_text(angle = 90))
#
# ggplot(processed) + geom_bar(aes(x= nbbtrm)) +xlab("number of bathroom") + theme_kat() + theme(axis.text.x = element_text(angle = 90))
#
# ggplot(processed) + geom_bar(aes(x= nbtoil)) +xlab("number of WC") + theme_kat() + theme(axis.text.x = element_text(angle = 90))

## 6.7. pool ---------------

# # filling up pool and pond using distribution water

processed$pldisw <-
  factor(org$Q11_pisc_ville, labels = c("no", "yes"))

# ggplot(processed) + geom_bar(aes(x= pldisw)) +xlab("using distribution water for pool") + theme_kat() + theme(axis.text.x = element_text(angle = 90))


## having permanant pool

processed$pmnpol <-
  factor(org$Q24_pisc_perm_pres, labels = c("no", "yes"))

# ggplot(processed) + geom_bar(aes(x= pmnpol)) +xlab("having permanant pool") + theme_kat() + theme(axis.text.x = element_text(angle = 90))

# # replace permenant pool since 2009

processed$pmnprp <-
  factor(org$Q24_pisc_perm_rempl, labels = c("no", "yes"))

# ggplot(processed) + geom_bar(aes(x= pmnprp)) +xlab("replace permenant pool since 2009") + theme_kat() + theme(axis.text.x = element_text(angle = 90))

# # check if no pool but yes replace - 10 houses

# df <- processed[processed$pmnpol %in% "no" & processed$pmnprp %in% "yes",]

# # having temporal/inflatable pool

processed$tmppol <-
  factor(org$Q24_pisc_temp_pres, labels = c("no", "yes"))

# ggplot(processed) + geom_bar(aes(x= tmppol)) +xlab("having temporal/inflatable pool") + theme_kat() + theme(axis.text.x = element_text(angle = 90))

# replace temporal/inflatable pool (probably not use)

processed$tmpprp <-
  factor(org$Q24_pisc_temp_rempl, labels = c("no", "yes"))

# ggplot(processed) + geom_bar(aes(x= tmpprp)) +xlab("having temporal/inflatable pool replace") + theme_kat() + theme(axis.text.x = element_text(angle = 90))

# # check if no temp pool but yes replace - 10 houses

# df <- processed[processed$tmppol %in% "no" & processed$tmpprp %in% "yes",]

## 6.8. garden --------------

# using anykind of water for garden

garden_df <- org[, grep(pattern = "jard", colnames(org))]

garden <- apply(garden_df, 1, function(x)
  "Oui" %in% x)

processed$garden <- factor(garden, labels = c("no", "yes"))



# 7. technology -----------------

# colnames(org)[c(123:136)]

## 7.1. dishwasher -------------------

processed$dshwas <-
  factor(org$Q23_vais_pres, labels = c("no", "yes"))

# replace after 2009

processed$dshwrp <-
  factor(org$Q23_vais_rempl, labels = c("no", "yes"))

# 3 levels

processed$dshw3c <- tech3lvs(processed$dshwas, processed$dshwrp)


## 7.2. washing machine -------------------

processed$wasmch <-
  factor(org$Q23_lav_pres, labels = c("no", "yes"))

# replace after 2009

processed$wasmrp <-
  factor(org$Q23_lav_rempl, labels = c("no", "yes"))

processed$wasm3c <- tech3lvs(processed$wasmch, processed$wasmrp)

## 7.3. bath/shower (baignoire & douche separee de baignoire)----------------

# # bath and shower as separate variable and whter replace after 2009 or not

processed$bath <-
  factor(org$Q24_baig_pres, labels =  c("no", "yes"))

processed$bathrp <-
  factor(org$Q24_baig_rempl, labels =  c("no", "yes"))

processed$bath3c <- tech3lvs(processed$bath, processed$bathrp)

processed$shower <-
  factor(org$Q24_dou_pres, labels =  c("no", "yes"))

processed$shwrrp <-
  factor(org$Q24_dou_rempl, labels =  c("no", "yes"))

processed$shwr3c <- tech3lvs(processed$shower, processed$shwrrp)

# # bath and shower as 1 variable with 4 levels none, shower, bath, both
# # same for whether they replace none, replace shower, replace bath, or replace both since 2009

bthshw <- paste0(org$Q24_baig_pres, org$Q24_dou_pres)

processed$bthshw <-
  factor(bthshw, labels = c("none", "shower", "bathtub", "both"))

btshrp <- paste0(org$Q24_baig_rempl, org$Q24_dou_rempl)

processed$btshrp <-
  factor(btshrp, labels = c("none", "shower", "bathtub", "both"))

## 7.4. saving water appliance --------------

# # treat efficient shower head and efficient WC separately

# # efficient shower head (Pommeau de douche economiseur d'eau - aerateur/mousseur)
processed$efshhd <-
  factor(org$Q24_pom_pres, labels = c("no", "yes"))

# # efficient shower head replace since 2009
processed$efshrp <-
  factor(org$Q24_pom_rempl, labels = c("no", "yes"))

processed$efsh3c <- tech3lvs(processed$efshhd, processed$efshrp)

# # efficient toilet (chasse WC avec economiseur - double bouton/ resevoir reduit/ eco-sac)
processed$eftoil <- factor(org$Q24_wc_pres, labels = c("no", "yes"))

# # efficient shower head replace since 2009
processed$eftlrp <-
  factor(org$Q24_wc_rempl, labels = c("no", "yes"))

processed$eftl3c <- tech3lvs(processed$eftoil, processed$eftlrp)

# # efficient tech as the combination of 2 original efficient shower head and efficient WC

eftech <- paste0(org$Q24_pom_pres, org$Q24_wc_pres)

processed$eftech <-
  as.factor(car::recode(eftech, "'NonNon' = 'no'; c('NonOui', 'OuiNon', 'OuiOui') = 'yes'"))

# # replace tech since 2009

efterp <- paste0(org$Q24_pom_rempl, org$Q24_wc_rempl)

processed$efterp <-
  as.factor(car::recode(efterp, "'NonNon' = 'no'; c('NonOui', 'OuiNon', 'OuiOui') = 'yes'"))

## 7.5. dried toilet -------------

processed$drtoil <-
  factor(org$Q24_toi_sech_pres, labels = c("no", "yes"))

# replace dried toilet since 2009

processed$drtlrp <-
  factor(org$Q24_toi_sech_rempl, labels = c("no", "yes"))

processed$drtl3c <- tech3lvs(processed$drtoil, processed$drtlrp)

## 7.6. AOI -------------
# assume that household without washing machine use laundromat => water for cloth washing = 0
# assume that water saving tech reduce 33% water use for that purpose
# assume that on average water use for laundry, shower, toilet are 15%, 40%, 30% respectively
processed$aoi <-
  (0.15 * (processed$wasmch == "yes") + 0.4 * (1 - 0.33 * (processed$efshhd == "yes")) + 0.3 *
     (1 - 0.33 * (processed$eftoil == "yes"))) / 0.85




# pldata <- processed[, grep("3c", names(processed))]
#
# pldata_melted <- melt(as.matrix(pldata))
#
# ggplot(pldata_melted) +
#   geom_bar(aes(x= Var2, fill = value,y = (..count..)/2119 )) +
#   scale_color_discrete() +
#   xlab(NULL) +
#   theme_kat() +
#   theme(axis.text.x = element_text(angle = 90))
#
#


# 8. behaviour --------------

# colnames(org)[c(1,69,219:237)]

## 8.1. financial help to install rain water tank ----------------
# # [Placement d'une citerne d'eau de pluie] Vous bénéficiez pour votre habitation de quelle(s) prime(s) ou aide(s) versée(s) par la Région wallonne pour améliorer les éléments suivants ?


processed$hlpwtk <-
  factor(org$Q29_cit, labels =  c("no", "yes")) # # only 5 yes - very very little

# table(processed$hlpwtk)
## 8.2. trust in water -------------

processed$cfdiwq <-
  factor(
    org$Q12_conf_eau,
    lvls(org$Q12_conf_eau)[c(1, 5, 4, 6, 3, 2)],
    labels = c(
      "confident",
      "rather confident",
      "neither confident nor suspicious",
      "rather suspicious",
      "suspicious",
      "no opinion"
    )
  )

# ggplot(processed) + geom_bar(aes(x= cfdiwq)) + xlab(NULL) + theme_kat() + theme(axis.text.x = element_text(angle = 90))


## 8.3. pay per use -------------

ppusvl <- org$Q7_paye_eau

processed$ppusvl <-
  as.factor(
    car::recode(
      ppusvl,
      "c('Charges locatives', 'Pas usager') = 'no'; c('NSP', 'Par an', 'Tous les 2 mois', 'Tous les 3 mois', 'Tous les 6 mois', 'Tous les mois') = 'yes'"
    )
  )

# summaryf(processed$ppusvl)

## 8.4. compteur type --------------
# # only 3 with compteur budget and 1 with limit eau

processed$bdgmtr <-
  factor(org$Q9_compt_budg_eau, labels =  c("no", "yes"))

processed$lmtmtr <- factor(org$Q9_lim_eau, labels =  c("no", "yes"))


# summary(org[,19:21])


# 9. economics ------------

## 9.1. tariff ---------
processed <- left_join(processed, price)

## 9.2. bill ----------

processed$ab30 <- as.numeric(processed$csmptv > 30)

processed$bill <-
  1.06 * (
    0.0125 * processed$csmptv + (20 * processed$CVD + 30 * processed$CVA)  + 0.5 *
      processed$csmptv * processed$CVD + (processed$csmptv - 30) * (0.5 * processed$CVD + processed$CVA) *
      processed$ab30
  )


## 9.3. price -------

processed$mgnprc <-
  1.06 * (0.0125 + 0.5 * processed$CVD + (0.5 * processed$CVD + processed$CVA) *
            processed$ab30)

processed$avrprc <- processed$bill / processed$csmptv
processed$avrprc[processed$avrprc %in% Inf] <- NA


# summary(processed$avrprc)


# 10. other consumption  --------------------

## 10.1. adjust for dwelling occupancy ----

## water consumption of the household taken Q27 (nb of week not occupate)


processed$dwelocp <- (52 - org$Q27_inocc) / 52

processed$dwelocp[processed$dwelocp > 1] <- NA

# summary(processed$dwelocp)

processed$csptdo <-
  processed$csmptv / processed$dwelocp # consumption with dwelling occupancy - higher value, higher number of missing value

# ggplot(processed,aes(x= csptdo)) + geom_histogram() + xlab("Consumption in 2014 with dwelling occupancy m3") + theme_kat()

## 10.2. per person per day (L/day) -------

### without occupancy --------

processed$cspppd <- (processed$csmptv * 1000 / processed$hhs_tot) / 365

# ggplot(processed,aes(x= cspppd)) + geom_histogram() + xlab("consumption per person per day in 2014 l") + theme_kat()

### with occupancy --------

processed$cspdwo <- (processed$csptdo * 1000 / processed$hhspo_tot) / 365

# ggplot(processed,aes(x= cspdwo)) + geom_histogram() + xlab("consumption/day/person in 2014 with occupancy (l)") + theme_kat()

## 10.3. per equivalent adult per day (L/day)-------

### without occupancy -----

processed$cspeqa <- (processed$csmptv * 1000 / processed$eqadlt) / 365

# ggplot(processed,aes(x= cspeqa)) + geom_histogram() + xlab("consumption/day/equivalent adult in 2014 (l)") + theme_kat()

### with occupancy ------

processed$cspeqo <- (processed$csptdo * 1000 / processed$eqadpo) / 365

# ggplot(processed,aes(x= cspeqo)) + geom_histogram() + xlab("consumption/day/equivalent adult in 2014 with occupancy (l)") + theme_kat()


# 11. save data ---------------

processed <- processed[order(processed$id), ]

write.csv(
  processed,
  file = here(
    pdir,
    "utilities_survey_Aquawal_CEHD_Wal/surv14_obs_AquaWal_prd.csv"
  ),
  row.names = F
)

