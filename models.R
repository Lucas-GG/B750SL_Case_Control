
#toy data
#x1 <- rnorm(100)
#x2 <- rnorm(100)
#y <- as.numeric((.5 *  x1 + rnorm(100)) > 0)
#id <- c(1:50, 1:50)
#df <- data.frame(y, id, x1, x2)
#df$X <- cbind(x1, x2)

#===============================================================================
#. - Penalized conditional logistic
#===============================================================================
library(clogitL1)
library(parallel)
#' Fits Conditional logistic regression with elastic net penalties
#' http://www.jstatsoft.org/v58/i12/

pclogit <- function(df, nboot = 20
    , alpha = 1,  mc_cores = 1, num_folds = 5) {
    #a large number since the distribution will not be normal
    x  <-  df[, grep("X", colnames(df))]
    y  <-  df$y
    id <-  df$id


    ###.- point estimate
    cll1     <- clogitL1(x, y, id, alpha = alpha)
    clcv     <- cv.clogitL1(cll1, numFolds = num_folds)
    beta_hat <- summary(clcv)$beta_minCV
    # whole dataset used to compute estimates at optimum


    ###.-inference (95% CI)
    estmtr    <- \(ids) {
                cl0 <- clogitL1(x[id %in% ids, ]
                , y[id %in% ids], id[id %in% ids], alpha = alpha)
                #cross-validation whithin boot = time!!!
                clcv <- cv.clogitL1(cl0, numFolds = num_folds)
                summary(clcv)$beta_minCV
                }
    ids      <- replicate(nboot
        , sample(id, length(id), replace = TRUE)
        , simplify = FALSE)

    #boot_est   <- mclapply(ids, estmtr, mc.cores = mc_cores)
    #boot_est   <- simplify2array(boot_est)
    boot_est   <- sapply(ids, estmtr)
    beta_se    <- apply(boot_est, 1, sd)
    beta_lw    <- apply(boot_est, 1, quantile, .025)
    beta_up    <- apply(boot_est, 1, quantile, .975)

    cbind(beta_hat, beta_se, beta_lw, beta_up)
    }


#===============================================================================
###II. Bayesian clogit with horseshoe prior
#===============================================================================
#' Fits Bayesian conditional logistic regression with horseshoe prior
library(rstanarm)

bclogit  <- function(df, chains = 1, iter = 500) {
  df <- df[order(df$id), ]
  x  <-  df[, grep("X", colnames(df))]
  y  <-  df$y
  id <-  df$id

  bcl <- stan_clogit(y ~ . - id
      , strata = id
      , data = data.frame(y, x, id)
      , QR = TRUE
      , chains = chains, iter = iter
      , prior = hs()
      )

  betas <- summary(bcl,
    pars = "beta",
    probs = c(0.025, 0.975),
    )

  as.data.frame(betas)[, c(1, 4, 5)]
}



#===============================================================================
###III. Lasso clogit
#===============================================================================
library(clogitLasso)
#' Alternative algorithm
#' http://www.jstatsoft.org/v58/i12/

lclogit <- function(df, nboot = 20
    ,  mc_cores = 1, num_folds = 5) {
    #a large number since the distribution will not be normal
    x  <-  as.matrix(df[, grep("X", colnames(df))])
    y  <-  df$y
    id <-  df$id

    ###.- point estimate
    cll1     <- clogitLasso(x, y, id)
    clcv     <- cv.clogitLasso(cll1, K = num_folds)
    beta_hat <- clcv$beta
    # whole dataset used to compute estimates at optimum


    ###.-inference (95% CI)
    estmtr    <- \(ids) {
                cl0 <- clogitLasso(x[id %in% ids, ]
                , y[id %in% ids], id[id %in% ids])
                #cross-validation whithin boot = time!!!
                clcv <- cv.clogitLasso(cl0, K = num_folds)
                clcv$beta
                }
    ids      <- replicate(nboot
        , sample(id, length(id), replace = TRUE)
        , simplify = FALSE)

    boot_est   <- mclapply(ids, estmtr, mc.cores = mc_cores)
    boot_est   <- simplify2array(boot_est)
    beta_se    <- apply(boot_est, 1, sd)
    beta_lw    <- apply(boot_est, 1, quantile, .025)
    beta_up    <- apply(boot_est, 1, quantile, .975)

    cbind(beta_hat, beta_se, beta_lw, beta_up)
    }