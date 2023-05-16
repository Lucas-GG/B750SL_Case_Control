library(parallel)
options(digits = 3)
options(scipen = 1000000)
options(mc.cores = parallel::detectCores())
options(help_type = "html")
set.seed(0203)



df    <- readRDS("data/dat1")
source("models.R")


#===============================================================================
### lasso
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(c(13, 19, 20, 24, 36), \(i) {
    beta  <- pclogit(df[df$rep == i, ], nboot = 200, mc_cores = 20)
    saveRDS(beta, paste0("data/fit_l/s1_fit_plogit_", i))
    })
end <- Sys.time()
end - start

summary(df[df$rep == 13, ])
# 10  in ~2 min
# error with c(13, 19, 20, 24, 36)

list_files <- list.files("data/fit_l", full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, "data/fit_lasso.rds")

#===============================================================================
#Bayesian clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(49:50, \(i) {
    beta  <-  bclogit(df[df$rep == i, ],  4, 1000)
    saveRDS(beta, paste0("data/fit_b/fit", i))
    })
end <- Sys.time()
end - start


# 2 in 30 sec
# error with 48 (thre are 3 unpaired)

list_files <- list.files("data/fit_b", full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, "data/fit_bayes.rds")

#===============================================================================
#ridge clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(1:50, \(i) {
    beta  <- pclogit(df[df$rep == i, ]
    , nboot = 200, mc_cores = 18, alpha = 0)
    saveRDS(beta, paste0("data/fit_r/fit", i))
    })
end <- Sys.time()
end - start



list_files <- list.files("data/fit_r", full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, file = "data/fit_ridge.rds")