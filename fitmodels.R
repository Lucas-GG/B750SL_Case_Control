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

df    <- readRDS("data/dat1m")
betas <- saveRDS(sim1i_bs, "data/sim1i_bs")

dim(sim1i_bs)


source("models.R")

betas_pclogit <- pclogit(df, 20)



bclogit(df, 1, 100)