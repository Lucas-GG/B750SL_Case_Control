
pars <- readRDS("data/sim1i_bs")
true_betas <- pars[3:7][1, ]

lasso    <- readRDS("data/fit_lasso.rds")
bayes    <- readRDS("data/fit_bayes.rds")
ridge    <- readRDS("data/fit_ridge.rds")

true_pred  <- c(4,  6,  8,  9, 10)
false_pred <- (1:50) [!1:50 %in% true_pred]

#===============================================================================
### coverage true predictors
#===============================================================================
true_betas

true_cvg_lasso <-
    apply(
        apply(lasso[true_pred, , ], 3, \(m) {
            true_betas >= m[, "beta_lw"] & true_betas <= m[, "beta_up"]
            })
        , 1, mean)

true_cvg_bayes <-
    apply(
        apply(bayes[true_pred, , ], 3, \(m) {
            true_betas >= m[, "X2.5."] & true_betas <= m[, "X97.5."]
            })
        , 1, mean)

true_cvg_ridge <-
    apply(
        apply(ridge[true_pred, , ], 3, \(m) {
            true_betas >= m[, "beta_lw"] & true_betas <= m[, "beta_up"]
            })
        , 1, mean)

rbind(
    true_cvg_lasso,
    true_cvg_bayes,
    true_cvg_ridge)

#===============================================================================
### coverage noise predictors
#===============================================================================
zero <- rep(0, length(false_pred))

false_cvg_lasso <-
    summary(
        apply(
            apply(lasso[false_pred, , ], 3, \(m) {
                zero >= m[, "beta_lw"] & zero <= m[, "beta_up"]
                })
            , 1, mean)
    )

false_cvg_bayes <-
    summary(
        apply(
            apply(bayes[false_pred, , ], 3, \(m) {
                zero >= m[, "X2.5."] & zero <= m[, "X97.5."]
                })
            , 1, mean)
    )


false_cvg_ridge <-
    summary(
        apply(
            apply(ridge[false_pred, , ], 3, \(m) {
                zero >= m[, "beta_lw"] & zero <= m[, "beta_up"]
                })
            , 1, mean)
    )

rbind(
    false_cvg_lasso,
    false_cvg_bayes,
    false_cvg_ridge)
