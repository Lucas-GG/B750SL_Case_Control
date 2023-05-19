library(parallel)
library(R.utils)
options(digits = 3)
options(scipen = 1000000)
options(mc.cores = parallel::detectCores())
options(help_type = "html")
set.seed(0203)



#df    <- readRDS("data/dat1")
df    <- readRDS("data2/dat3")
folder_location <- "data2/scenario3/"
# Lucas, I loaded dat4 to run the below. Added folder for "scenario4" and saved results there
source("models.R")


#===============================================================================
### lasso
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(c(1:50), \(i) {
      #adding timeout which should produce null if thing takes more than 600 seconds.
      #If it doesn't then it will save the beta results as usual.
    beta  <- withTimeout({pclogit(df[df$rep == i, ], nboot = 200, mc_cores = 20)}, timeout = 2000, onTimeout= "warning")
    if(!is.null(beta)){
      saveRDS(beta, paste0(folder_location,"fit_l/s1_fit_plogit_", i))
    } else{print(paste("timeout", i))}
    })
end <- Sys.time()
end - start

#summary(df[df$rep == 13, ])
# 10  in ~2 min
# error with c(13, 19, 20, 24, 36)

# scenario 4 round 2 time took 8.77 hours with the following timeouts:
# [1] "timeout 3"
# [1] "timeout 4"
# [1] "timeout 7"
# [1] "timeout 11"
# [1] "timeout 14"
# [1] "timeout 16"
# [1] "timeout 30"
# [1] "timeout 32"
# [1] "timeout 36"
# [1] "timeout 38"
# [1] "timeout 39"
# [1] "timeout 42"
# [1] "timeout 44"
# [1] "timeout 46"
# [1] "timeout 48"

list_files <- list.files(paste0(folder_location,"fit_l"), full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, paste0(folder_location,"fit_lasso.rds"))

#===============================================================================
#Bayesian clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(1:50, \(i) {
    beta  <-  bclogit(df[df$rep == i, ],  2, 2000) # changed to 2 chains and 2000 iter just for timing for now
    saveRDS(beta, paste0(folder_location,"fit_b/fit", i))
    })

end <- Sys.time()
end - start

# did 20 in 15 minutes for scenario 2

# 2 in 30 sec
# error with 48 (thre are 3 unpaired)

list_files <- list.files(paste0(folder_location,"fit_b"), full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, paste0(folder_location,"fit_bayes.rds"))

#===============================================================================
#ridge clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(1:50, \(i) {
    beta  <- pclogit(df[df$rep == i, ]
    , nboot = 200, mc_cores = 18, alpha = 0)
    saveRDS(beta, paste0(folder_location,"/fit_r/fit", i))
    })
end <- Sys.time()
end - start

#scenario 2 took 59.1 minutes 

#scenario 4 took 35.6 minutes


list_files <- list.files(paste0(folder_location,"fit_r"), full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, file = paste0(folder_location,"fit_ridge.rds"))
