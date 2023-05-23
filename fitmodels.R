library(parallel)
library(R.utils)
options(digits = 3)
options(scipen = 1000000)
options(mc.cores = parallel::detectCores())
options(help_type = "html")

#===============================================================================
#simulate data
#===============================================================================
set.seed(1906)

source("simulation1.R")

dat1 <- map_df(101:200, s1)
summary(dat1)
dat2 <- map_df(101:200, s2)
summary(dat2)
dat3 <- map_df(101:200, s3)
summary(dat3)
dat4 <- map_df(101:200, s4)
summary(dat4)

saveRDS(dat1, "data3/dat1")
saveRDS(dat2, "data3/dat2")
saveRDS(dat3, "data3/dat3")
saveRDS(dat4, "data3/dat4")



#===============================================================================



df    <- readRDS("data3/dat4")
folder_location <- "data3/scenario4/"
# Lucas, I loaded dat4 to run the below. Added folder for "scenario4" and saved results there
source("models.R")
set.seed(1906)

#===============================================================================
### lasso
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(101:200, \(i) {
      #' adding timeout which
      #' should produce null if thing takes more than 600 seconds.
      #' If it doesn't then it will save the beta results as usual.
    beta  <- withTimeout(
      {pclogit(df[df$rep == i, ], nboot = 200, mc_cores = 18) }
      , timeout = 60
      , cpu = 100000
      , elapsed = 60
      , onTimeout = "warning")
    if(!is.null(beta)){
      saveRDS(beta, paste0(folder_location, "fit_l/fit", i))
    } else {print(paste("timeout", i))}
    })
end <- Sys.time()
end - start



#===============================================================================
#Bayesian clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    mclapply(101:200, \(i) {
    beta  <-  bclogit(df[df$rep == i, ],  4, 2000)
    # changed to 2 chains and 2000 iter just for timing for now
    saveRDS(beta, paste0(folder_location, "fit_b/fit", i))
    }, mc.cores = 5)

end <- Sys.time()
end - start

# did 20 in 15 minutes for scenario 2

# 2 in 30 sec
# error with 48 (thre are 3 unpaired)



#===============================================================================
#ridge clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    lapply(101:200, \(i) {
    beta  <- pclogit(df[df$rep == i, ]
    , nboot = 200, mc_cores = 18, alpha = 0)
    saveRDS(beta, paste0(folder_location, "/fit_r/fit", i))
    })
end <- Sys.time()
end - start

#scenario 2 took 59.1 minutes 

#scenario 4 took 35.6 minutes

folder_location <- "scenario4/"

list_files <- list.files(paste0("data2/",folder_location, "fit_l"), full.names = TRUE)
list_files <- c(list_files, list.files(paste0("data3/",folder_location, "fit_l"), full.names = TRUE))

list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, paste0("data3/",folder_location, "fit_lasso.rds"))

list_files <- list.files(paste0("data2/",folder_location, "fit_b"), full.names = TRUE)
list_files <- c(list_files, list.files(paste0("data3/",folder_location, "fit_b"), full.names = TRUE))
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, paste0("data3/",folder_location, "fit_bayes.rds"))

list_files <- list.files(paste0("data2/",folder_location, "fit_r"), full.names = TRUE)
list_files <- c(list_files, list.files(paste0("data3/",folder_location, "fit_r"), full.names = TRUE))
list_dat   <- lapply(list_files, \(f) data.frame(readRDS(f)))
arr        <- abind::abind(list_dat, along = 3)
saveRDS(arr, file = paste0("data3/",folder_location, "fit_ridge.rds"))
