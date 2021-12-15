# # citation -------------
citation()
citation(package = 'MuMIn')

 # for knit Pdf with Rmarkdown -------------


install.packages('tinytex')
library(tinytex)
tinytex::install_tinytex()
## knit with date ---------
rmarkdown::render("test.Rmd",output_file=paste0('name I want ', format(Sys.time(), "%y%m%d %H%M%S"),'.html'))
# # check if library has been installed or not and install -----------

if (!suppressWarnings(require(R2WinBUGS))) list(install.packages("R2WinBUGS"), library(R2WinBUGS))

 
## import data -----

var <- read.csv("2 Data/3 Processed/UtilitySurvey2014_AquaWal_CEHD/Survey2014_var_AquaWal_slt.csv", na.strings = c("", "#NULL!"), strip.white = TRUE, encoding = "UTF-8")



## copy result into clipboard ----

write.table(clmliege[clmliege$dm %in% '01/02','T2M'], "clipboard", sep="\t", row.names=T, col.names=T)
str(summary(svfit1)$coefficients)


# # combine 2 levels of a factor to one-----

library(car)
socidemo$rped4c <- recode(socidemo$rped6c, "c('professional','technique') = 'vocation'; c('higher not university', 'university') = 'university'")



# # reorder or reformat lables of factors levels-----

dwelling$dwcsy9 <- factor(dweltemp$Q17_ann_constr, levels(dweltemp$Q17_ann_constr)[c(2:9,1)], labels = c('Before 1875', '1875-1918', '1919-1945', '1946-1970', '1971-1980', '1981-1990', '1991-2000', '2001-2005', '2006 and after'))


# # align 2 plots 1 legend ---------


p1 <-  ggplot(device_melt[!(device_melt$variable %in% 'tank'),], aes(x = variable, y = prop, fill = city)) + geom_col(position = position_dodge2(preserve = 'single')) + ylab('percentage of households')+ ylim(0,100) + xlab('device') + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = 'bottom')

p2 <- ggplot(wtinf_melt, aes(x = variable, y = prop, fill = city)) + geom_col(position = position_dodge2(preserve = 'single')) + ylab('percentage of households') + ylim(0,100) + xlab('recent changes') + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = 'bottom')

legend <- get_legend(p1 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

pg <- plot_grid(p1+ theme(legend.position="none"), p2+ theme(legend.position="none"), nrow = 1, align = 'hv')

plot_grid(pg, legend, ncol = 1, rel_heights = c(1, .1))


# # create not missing matrix, which can be use to multiplied with other----

mat <- !is.na(age)


# # plot histogram or bar for quick view data----

ggplot(consumption, aes(x = csmptv)) + geom_histogram() + xlab('consumption m3/year') + theme_bw()

ggplot(output) + geom_col(aes(x= name, y=Freq, fill = replace)) + xlab(NULL) + theme_bw() + theme(axis.text.x = element_text(angle = 90))

ggplot(dwelling) + geom_bar(aes(x= pldisw)) +xlab('using distribution water for pool') + theme_bw() + theme(axis.text.x = element_text(angle = 90))


# # to count the number of line for each group, probably for 2 variables and merge them together----

df1 <- aggregate(.~ munigb + municd ,spatial[, c('id','municd', 'munigb')], FUN = length)
colnames(df1) <- c('name', 'code', 'munigb')

df2 <- aggregate(.~ munici  ,spatial[, c('id', 'munici')], FUN = length)
colnames(df2) <- c('name', 'munici')

mncpfq <- merge(df1, df2, all=T)





# before merging data using cbind, to make sure that the line order are the same, can use match to reorder one of them----

df1 <- df[match(df0$id, df$id),] # don't need if use merge()


# change colnames----

colnames(WDM) <- c('id', 'cons_verf')


# histogram with normal curve (check for better one in WD)----

hist(g$cons_verf, breaks = 50, probability = T)
curve(dnorm(x, mean = mean(g$cons_verf, na.rm = T), sd = sqrt(var(g$cons_verf, na.rm = T))), add = TRUE, lwd = 2, col="red")

# count number of consecutive NA from the last one: very useful to see when a patient dropout/ or for survival analysis-----------

hhs$size <- apply (hhs, 1, function (x) 10-(which.min(rev(is.na(x)))-1))


# compare 2 string vectors----------
b <- setdiff(a$V1, a$V2)

# trim white space instead strip.white in read.csv does not work------------

a <- as.data.frame(t(apply(ads[,c('Adresse.x', 'Adresse.y')], 1, str_trim)))


# remove accents from VNese/French/German -------
library(stringi)
# remember to read csv with ", encoding="UTF-8""
sptgb$ststgb <- stri_trans_general(sptgb$ststgb,"Latin-ASCII; title")

# link R with the newest Rtools
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

Sys.which("make")