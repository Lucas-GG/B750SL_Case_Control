options(digits = 3)
options(scipen = 1000000)
options(mc.cores = parallel::detectCores())
options(help_type = "html")
set.seed(0203)

#sapply(list.files("R/", ".R", full.names = TRUE), source)

df <- readRDS("data/dat1m")
source("models.R")

pclogit(df, 20)
bclogit(df, 1, 200)