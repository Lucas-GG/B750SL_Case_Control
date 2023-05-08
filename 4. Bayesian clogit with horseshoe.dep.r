options(digits = 3)
options(scipen = 1000000)
options(mc.cores = parallel::detectCores())

set.seed(0203)

#toy data
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- as.numeric((.5 *  x1 + rnorm(100)) > 0)
id <- c(1:50, 1:50)
df <- data.frame(y, id, x1, x2)
#df$X <- cbind(x1, x2)



#===============================================================================
###II. Fitting Bayesian clogit with horsehose prior
#===============================================================================
library(posterior)
library(rstanarm)

df <- df[order(df$id), ]

bcl <- stan_clogit(y ~ . - id
     , strata = id
     , data = df
     , QR = TRUE
     , chains = 1, iter = 500 # for speed only
     , prior = hs()
     )

betas <- summary(bcl,
  pars = "beta",
  probs = c(0.025, 0.975),
)

as.data.frame(betas)