
pars <- readRDS("data/sim1i_bs")
true_betas <- pars[3:7][1, ]

lasso    <- readRDS("data/fit_lasso.rds")
bayes    <- readRDS("data/fit_bayes.rds")
ridge    <- readRDS("data/fit_ridge.rds")

true_pred  <- c(4,  6,  8,  9, 10)
false_pred <- (1:50) [!1:50 %in% true_pred]

dimnames(bayes)[[2]] <- c("beta_hat", "beta_lw", "beta_up")

#===============================================================================
## avg # of true and false effects
#===============================================================================
# proportion of times true preictors are detected (entire CI on either side)
tpr <- function(array_betas) {
    apply(
        apply(array_betas[true_pred, , ], 3, \(m) {
            (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
            (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
            })
        , 1, mean)
}
rbind(
round(tpr(lasso), 3),
round(tpr(bayes), 3),
round(tpr(ridge), 3)
)

fpr <- function(array_betas) {
    summary(
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
                (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
                })
            , 1, mean)
    )
}
rbind(
round(fpr(lasso), 3),
round(fpr(bayes), 3),
round(fpr(ridge), 3)
)


#===============================================================================
### coverage noise predictors
#===============================================================================

true_avg <- function(array_betas) {
    mean(
        apply(
            apply(array_betas[true_pred, , ], 3, \(m) {
                (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
                (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
                })
            , 2, sum)
    )
}
rbind(
round(true_avg(lasso), 3),
round(true_avg(bayes), 3),
round(true_avg(ridge), 3)
)

false_avg <- function(array_betas) {
    mean(
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
                (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
                })
            , 2, sum)
    )
}

rbind(
round(false_avg(lasso), 3),
round(false_avg(bayes), 3),
round(false_avg(ridge), 3)
)

#===============================================================================
### coverage true predictors
#===============================================================================

true_cvg <- function(array_betas) {
    apply(
        apply(array_betas[true_pred, , ], 3, \(m) {
            m[, "beta_lw"] <= true_betas & true_betas <= m[, "beta_up"]
            })
        , 1, mean)
}

rbind(
    round(true_cvg(lasso), 3),
    round(true_cvg(bayes), 3),
    round(true_cvg(ridge), 3)
    )


#===============================================================================
### coverage noise predictors
#===============================================================================
zero <- rep(0, length(false_pred))

false_cvg <- function(array_betas) {
    summary(
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                m[, "beta_lw"] <= zero & zero <= m[, "beta_up"]
                })
            , 1, mean)
    )
}

rbind(
    round(false_cvg(lasso), 3),
    round(false_cvg(bayes), 3),
    round(false_cvg(ridge), 3))
