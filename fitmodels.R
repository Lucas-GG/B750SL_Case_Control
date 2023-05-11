library(parallel)
options(digits = 3)
options(scipen = 1000000)
options(mc.cores = parallel::detectCores())
options(help_type = "html")
set.seed(0203)

#sapply(list.files("R/", ".R", full.names = TRUE), source)
#X4, X6, X8, X9 and X10

# TPR true positive rate
# FPR false positive rate
# CI covarage (agerage? )
betas <- saveRDS(sim1i_bs, "data/sim1i_bs")
dim(sim1i_bs)

df    <- readRDS("data/dat1")
df  <- df[order(df$rep, df$id, - df$y), ]


source("models.R")


#===============================================================================
### lasso
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    mclapply(split(df, df$rep), pclogit
    , nboot = 200)
end <- Sys.time()
end - start

# 10.3 secs with 100 boot with 5cv (1) 20:1
#about 2.3 mins with 100 boot with 10cv 1:2,1
#about 1.18 mins with 100 boot with 5cv 1:2,1
#about 1.21 mins with 100 boot with 5cv 1:4,1
#about 1.25 mins with 100 boot with 5cv 1:10,1
#about 1.25 mins with 100 boot with 5cv 1:15,1
#about 1.25 mins with 100 boot with 5cv 1:18,1
#about 2.33 mins with 200 boot with 5cv (2):1
#about 2.33 mins with 200 boot with 5cv (2):1

#===============================================================================
#Bayesian clogit
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    mclapply(split(df, df$rep)[1:5], bclogit, 3, 500)
end <- Sys.time()
end - start


#===============================================================================
#lasso clogit alternative algorithm
#===============================================================================
start <- Sys.time()
betas_pclogit <-
    mclapply(split(df, df$rep)[1:5], lclogit
        , 1, 100)
end <- Sys.time()
end - start