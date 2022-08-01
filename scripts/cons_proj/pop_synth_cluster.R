# readme -------------





# 0. load library ---------


library("lme4")
library("dplyr")
library("parallel")
library("reshape2")
library("lubridate")
# 1. load data  ----------------

args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
btu_snro <- args[2]
rwt_snro <- args[3]
ncores <- args[4]

if(exists("dir") == F){stop("Error: No input directory passed")}


options(dplyr.summarise.inform = FALSE)


# 2. create report -------------

con <- file(file.path(dir, paste0("report_", btu_snro, "_", rwt_snro, ".log")))

sink(con, append = T)
sink(con, append = T, type = 'message')


Sys.time()

# 3. load data ---------


load(file.path(dir, "hhs_sim_2040.Rdata"))

load(file.path(dir, "urban_5cat_ss_Wal.Rdata"))
load(file.path(dir, "fitted_probs.Rdata"))

# 4. sim function ---------------


sim.f <- function(a = "62063", data = df, hhsNA = F, rwt_snros = 1) {
  
  hhsdf <- data[data$municd %in% a,]
  
  ### hhsize ---------------
  
  hhs_sim <- hhsdf[, c("municd", "ststcd", "year", "bltupss", "hhs_1", "hhs_2", "hhs_3", "hhs_4")]
  
  hhs_sim[is.na(hhs_sim)] <- 0
  
  bighh <- lapply(hhsdf$`hhs_5+`, function(x) {
    if (!is.na(x) & x > 0) {
      bhh_v <- data.frame(rmultinom(x, 1, prob = bighh_prob))
      res <- apply(bhh_v, 1, sum)
    } else {
      res <- rep(0, 5)
    }
  })
  
  bighh <- Reduce(rbind.data.frame, bighh)
  colnames(bighh) <- paste0("hhs_", 5:9)
  
  hhs_sim <- cbind.data.frame(hhs_sim, bighh)
  
  hhs_sim$nhh_sim <- apply(hhs_sim[,5:13], 1, sum)
  
  if (hhsNA) {
    # x <- unlist(cbind.data.frame(nbhh_prv = hhsdf$nbhh_prv, hhs_sim[,4:14])[1,])
    hhs_NA <- apply(cbind.data.frame(nbhh_prv = hhsdf$nbhh_prv, hhs_sim[,4:14]), 1, function(x) {
      n_NA <- x[1] - x[10]
      if (!is.na(n_NA) & n_NA > 0) {
        hhs_prob <- x[3:11]/sum(x[3:11])
        
        if (sum(hhs_prob, na.rm = T) == 1) {
          
          hhs_v <- data.frame(rmultinom(n_NA, 1, prob = hhs_prob))
        } else {
          hhs_prob <- hhs_prob_ss[hhs_prob_ss$bltupss == x[2], -10]
          hhs_v <- data.frame(rmultinom(n_NA, 1, prob = hhs_prob))
          
        }
        res <- apply(hhs_v, 1, sum)
      } else {
        res <- rep(0, 9)
      }
    })
    
    
    
    hhs_sim[, 5:13] <- hhs_sim[, 5:13] + t(hhs_NA)
    hhs_sim$nhh_sim <- apply(hhs_sim[, 5:13], 1, sum)    
  }
  
  
  
  
  ### create dataframe with each row is a family
  
  hhs_sim_lg <- melt(hhs_sim[, 1:13], id.vars = c("municd", "ststcd", "year", "bltupss"))
  
  fam_sim <- hhs_sim_lg[rep(1:nrow(hhs_sim_lg), hhs_sim_lg$value), -6]
  rownames(fam_sim) <- 1:nrow(fam_sim)
  
  fam_sim$hhs_tot <- as.integer(gsub("hhs_", "", fam_sim$variable))
  
  fam_sim <- fam_sim[,-5]
  
  ### ref person age ------------------------------
  
  
  fam_sim$rpage_cat <- sapply(fam_sim$hhs_tot, function(x) {
    pr <- p_rpa_hhs[, dimnames(p_rpa_hhs)[[2]] ==x ]
    rpa_v <- rmultinom(1, 1, prob = pr)
    res <- dimnames(p_rpa_hhs)[[1]][rpa_v == 1]
  })
  
  
  
  ### household composition -------------------------------
  
  
  fam_sim$nch <- 0L
  
  fam_sim$nch[fam_sim$hhs_tot > 1] <- apply(fam_sim[fam_sim$hhs_tot > 1, c("rpage_cat", "bltupss", "hhs_tot")], 1, function(x) {
    
    rpa_tmp <- x[1]
    blt_tmp <- x[2]
    hhs_tmp <- x[3]
    
    pr1 <-
      p_chd_rpa_blt_hhs[, dimnames(p_chd_rpa_blt_hhs)[[2]] %in% rpa_tmp, dimnames(p_chd_rpa_blt_hhs)[[3]] %in% blt_tmp, dimnames(p_chd_rpa_blt_hhs)[[4]] %in% hhs_tmp]
    pr2 <-
      p_chd_blt_hhs[, dimnames(p_chd_blt_hhs)[[2]] %in% blt_tmp, dimnames(p_chd_blt_hhs)[[3]] %in% hhs_tmp]
    pr3 <- p_chd_hhs[, dimnames(p_chd_hhs)[[2]] %in% hhs_tmp]
    
    if (sum(pr1, na.rm = T) > 0.999 ){
      nch_v <- rmultinom(1, 1, prob = pr1)
    } else {
      if (sum(pr2, na.rm = T) > 0.999) {
        nch_v <- rmultinom(1, 1, prob = pr2)
      } else {
        nch_v <- rmultinom(1, 1, prob = pr3)
      }
    }
    
    res <- as.integer(dimnames(p_chd_hhs)[[1]][nch_v == 1])
  })
  
  
  
  fam_sim$nad <- fam_sim$hhs_tot - fam_sim$nch
  
  ### rainwater tank -----------
  
  
  fam_sim$rwt_prop <- rwt_mun$rwt_prop[rwt_mun$municd == a]*rwt_snros/100
  
  fam_sim$rwtank <- sapply(fam_sim$rwt_prop, function(x) {
    res <- rbinom(1, 1, prob = x)
  })
  
  fam_sim$rwtank <- ifelse(fam_sim$rwtank ==1, "yes", "no")
  ### water consumption --------------
  
  fam_sim$bltupss <- as.factor(fam_sim$bltupss)
  
  pred.mean <- predict(wcons_fit, newdata = fam_sim, allow.new.levels = T)
  
  cspd <- sapply(pred.mean, function(x) {
    res <- rnorm(n = 1, mean = x, sd = res_sd)
  })
  
  ndays <- yday(as.Date(paste0(fam_sim$year, "-12-31")))
  
  
  fam_sim$cons <- cspd*ndays/1000
  
  ### summarise -------------
  
  res <- fam_sim %>%
    group_by(municd, year) %>%
    summarise(nad = sum(nad, na.rm = T),
              nch = sum(nch, na.rm = T),
              n = n(),
              rwt = sum(rwtank == "yes", na.rm = T)/n(),
              cons = sum(cons, na.rm = T))
}




# 5. job ------------

df <- left_join(hhs_sim_2040, bltupss_sim[, c("ststcd", "year", btu_snro)])

colnames(df)[ncol(df)] <- "bltupss"

mncdls <- levels(as.factor(hhs_sim_2040$municd))

ncores

print("start parallel")
Sys.time()



res <- mclapply(1:100, function(i) {
  res <- lapply(mncdls, function(x) sim.f(a = x, data = df, hhsNA = F, rwt_snros = 1))
  res <- Reduce(rbind.data.frame, res)
}, mc.cores = ncores)


sim_agg <- bind_rows(res, .id = "rep")
sim_agg$pop <- sim_agg$nad + sim_agg$nch


save(sim_agg, file = file.path(dir, paste0("sim_agg_", btu_snro,"_",  rwt_snro, ".Rdata")))


print('finished')
Sys.time()
