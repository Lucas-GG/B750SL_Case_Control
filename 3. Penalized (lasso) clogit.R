options(digits = 3)
options(scipen = 1000000)
options(help_type = "html")
options(mc.cores = parallel::detectCores())
set.seed(0203)

#toy data
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- (.5 *  x1 + rnorm(100)) > 0
id <- c(1:50, 1:50)
df <- data.frame(y, id)
df$X <- cbind(x1, x2)

#===============================================================================
#. - LASSO penalized conditional logistic
#===============================================================================
library(clogitL1)
library(parallel)
nboot     <- 1000 #a large number since the distribution will not be normal

###.- point estimate
cll1     <- with(df, clogitL1(X, y, id, alpha = 1))
clcv     <- cv.clogitL1(cll1)
beta_hat <- summary(clcv)$beta_minCV


###.-inference (95% CI)
estmtr    <- \(ids) with(df[df$id %in% ids, ], {
              cl0 <- clogitL1(X, y, id, alpha = 1)
              clcv <- cv.clogitL1(cl0)
              summary(clcv)$beta_minCV
              })
ids      <- replicate(nboot
  , sample(id, length(id), replace = TRUE)
  , simplify = FALSE)

boot_est <- mclapply(ids, estmtr)
boot_est <- simplify2array(boot_est)
beta_se    <- apply(boot_est, 1, sd)
beta_lw    <- apply(boot_est, 1, quantile, .025)
beta_up    <- apply(boot_est, 1, quantile, .975)

cbind(beta_hat, beta_se, beta_lw, beta_up)

hist(boot_est[1, ])
hist(boot_est[2, ])