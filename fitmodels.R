library(parallel)
library(R.utils)
options(digits = 3)
options(scipen = 1000000)
options(mc.cores = parallel::detectCores())
options(help_type = "html")
set.seed(0203)



#df    <- readRDS("data/dat1")
df    <- readRDS("data/dat2")
folder_location <- "data/scenario2/"
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
    }
    print(paste("timeout", i))
    })
end <- Sys.time()
end - start

summary(df[df$rep == 13, ])
# 10  in ~2 min
# error with c(13, 19, 20, 24, 36)

# scenario 4: error with 3, 6, 7??

#scenario 4:
# error with 35, 42

list_files <- list.files(paste0(folder_location,"fit_l"), full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, paste0(folder_location,"fit_lasso.rds"))

#===============================================================================
#Bayesian clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(2:3, \(i) {
    beta  <-  bclogit(df[df$rep == i, ],  2, 500) # changed to 2 chains and 500 iter just for timing for now
    saveRDS(beta, paste0(folder_location,"fit_b/fit", i))
    })

end <- Sys.time()
end - start


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

#scenario 2 took 35.6 minutes 

#scenario 4 took 35.6 minutes


list_files <- list.files(paste0(folder_location,"fit_r"), full.names = TRUE)
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, file = paste0(folder_location,"fit_ridge.rds"))
