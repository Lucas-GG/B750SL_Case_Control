library(tidyverse)

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

tibble(
    var = paste0("X", true_pred),
    lasso = tpr(lasso),
    bayes = tpr(bayes),
    ridge = tpr(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "TPR") %>%
    ggplot(aes(x = var, y = TPR, group = method, colour = method)) +
 geom_hline(yintercept = .95, linetype = 2, color = "gray25") +
 geom_line() + geom_point() + theme_minimal()

ggsave(paste0("output/", "TPR.png"), bg = "white")





fpr <- function(array_betas) {
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
                (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
                })
            , 1, mean)
}

rbind(
summary(fpr(lasso)),
summary(fpr(lasso)),
summary(fpr(lasso))
)

tibble(
    lasso = fpr(lasso),
    bayes = fpr(bayes),
    ridge = fpr(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "FPR") %>%
    ggplot(aes(x = method, y = FPR, colour = method)) +
 geom_hline(yintercept = .05, linetype = 2, color = "gray25") +
 geom_boxplot() + theme_minimal() + theme(legend.position = "none")

ggsave(paste0("output/", "FPR.png"), bg = "white")

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

tibble(
    lasso = true_avg(lasso),
    bayes = true_avg(bayes),
    ridge = true_avg(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "avg_num_true") %>%
    ggplot(aes(x = method, y = avg_num_true,  fill = method)) +
 geom_col() + theme_minimal() + theme(legend.position = "none")

ggsave(paste0("output/", "avg_num_true.png"), bg = "white")


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

tibble(
    lasso = false_avg(lasso),
    bayes = false_avg(bayes),
    ridge = false_avg(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "avg_num_false") %>%
    ggplot(aes(x = method, y = avg_num_false,  fill = method)) +
 geom_col() + theme_minimal() + theme(legend.position = "none")
ggsave(paste0("output/", "avg_num_false.png"), bg = "white")

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

tibble(
    var = paste0("X", true_pred),
    lasso = true_cvg(lasso),
    bayes = true_cvg(bayes),
    ridge = true_cvg(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "coverage_true") %>%
    ggplot(aes(x = var, y = coverage_true, group = method, colour = method)) +
 geom_hline(yintercept = .95, linetype = 2, color = "gray25") +
 geom_line() + geom_point() + theme_minimal()

ggsave(paste0("output/", "cvg_true.png"), bg = "white")


#===============================================================================
### coverage noise predictors
#===============================================================================
zero <- rep(0, length(false_pred))

false_cvg <- function(array_betas) {
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                m[, "beta_lw"] <= zero & zero <= m[, "beta_up"]
                })
            , 1, mean)
}

rbind(
    round(summary(false_cvg(lasso)), 3),
    round(summary(false_cvg(bayes)), 3),
    round(summary(false_cvg(ridge)), 3))

tibble(
    lasso = false_cvg(lasso),
    bayes = false_cvg(bayes),
    ridge = false_cvg(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "coverage_false") %>%
    ggplot(aes(x = method, y = coverage_false, colour = method)) +
 geom_hline(yintercept = .95, linetype = 2, color = "gray25") +
 geom_boxplot() + theme_minimal() + theme(legend.position = "none")

ggsave(paste0("output/", "cvg_false.png"), bg = "white")
