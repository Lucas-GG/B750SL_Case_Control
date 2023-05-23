library(tidyverse)
library(gridExtra)
pars <- readRDS("data/sim1i_bs")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s <- 4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
true_betas <- pars[3:7][s, ]; true_betas
folder_location <- paste0("data3/scenario", s, "/")
output_folder <- paste0("output/scenario", s, "/")

lasso    <- readRDS(paste0(folder_location, "fit_lasso.rds"))
bayes    <- readRDS(paste0(folder_location, "fit_bayes.rds"))
ridge    <- readRDS(paste0(folder_location, "fit_ridge.rds"))

true_pred  <- c(4,  6,  8,  9, 10)
false_pred <- (1:50) [!1:50 %in% true_pred]

dimnames(bayes)[[2]] <- c("beta_hat", "beta_lw", "beta_up")

dim(lasso)
lasso[true_pred, , 1:2]
true_betas


#===============================================================================
##.1.- FPR & TPR
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
fpr <- function(array_betas) {
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
                (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
                })
            , 1, mean)
}

rbind(
    round(mean(tpr(lasso)), 3),
    round(mean(tpr(bayes)), 3),
    round(mean(tpr(ridge)), 3)
)

rbind(
summary(fpr(lasso)),
summary(fpr(lasso)),
summary(fpr(lasso))
)

g11 <-
tibble(
    var = paste0("X", true_pred),
    lasso = tpr(lasso),
    bayes = tpr(bayes),
    ridge = tpr(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "TPR") %>%
    ggplot(aes(x = var, y = TPR, group = method, colour = method)) +
 geom_hline(yintercept = .8, linetype = 2, color = "gray25") +
 geom_line() + geom_point() + theme_minimal()

g12 <-
tibble(
    lasso = fpr(lasso),
    bayes = fpr(bayes),
    ridge = fpr(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "FPR") %>%
    ggplot(aes(x = method, y = FPR, colour = method)) +
 geom_hline(yintercept = .2, linetype = 2, color = "gray25") +
 geom_boxplot() + theme_minimal() + theme(legend.position = "none")

grid.arrange(g11, g12, nrow = 1)
png(paste0(output_folder, "TPR_FPR.png")
    , width = 480 * 6, heigh = 480 * 2.5, res = 300)
grid.arrange(g11, g12, nrow = 1)
dev.off()


#===============================================================================
##.2.- Number of predictors
#===============================================================================

true_num <- function(array_betas) {
        apply(
            apply(array_betas[true_pred, , ], 3, \(m) {
                (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
                (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
                })
            , 2, sum)
}

false_num <- function(array_betas) {
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                (0 > m[, "beta_lw"] & 0 > m[, "beta_up"]) |
                (0 < m[, "beta_lw"] & 0 < m[, "beta_up"])
                })
            , 2, sum)
}

rbind(
    summary(true_num(lasso)),
    summary(true_num(bayes)),
    summary(true_num(ridge))
)

rbind(
    summary(false_num(lasso)),
    summary(false_num(bayes)),
    summary(false_num(ridge))
)

g21 <-
tibble(
    lasso = tail(c(NA * 1:150, true_num(lasso)), 150),
    bayes = true_num(bayes),
    ridge = true_num(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "number_true") %>%
    ggplot(aes(x = method, y = number_true, colour = method)) +
 geom_boxplot() + theme_minimal() + theme(legend.position = "none") +
   stat_summary(fun = mean, colour = "darkred", geom = "point",
               shape = 18, size = 3, show.legend = FALSE)


g22 <-
tibble(
    lasso = tail(c(NA * 1:150, false_num(lasso)), 150),
    bayes = false_num(bayes),
    ridge = false_num(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "number_false") %>%
    ggplot(aes(x = method, y = number_false, colour = method)) +
 geom_boxplot() + theme_minimal() + theme(legend.position = "none") +
   stat_summary(fun = mean, colour = "darkred", geom = "point",
               shape = 18, size = 3, show.legend = FALSE)


grid.arrange(g21, g22, nrow = 1)
png(paste0(output_folder, "num_true_false.png")
    , width = 480 * 6, heigh = 480 * 2.5, res = 300)
grid.arrange(g21, g22, nrow = 1)
dev.off()


#===============================================================================
##.3.- Coverage
#===============================================================================

true_cvg <- function(array_betas) {
    apply(
        apply(array_betas[true_pred, , ], 3, \(m) {
            m[, "beta_lw"] <= true_betas & true_betas <= m[, "beta_up"]
            })
        , 1, mean)
}

zero <- rep(0, length(false_pred))

false_cvg <- function(array_betas) {
        apply(
            apply(array_betas[false_pred, , ], 3, \(m) {
                m[, "beta_lw"] <= zero & zero <= m[, "beta_up"]
                })
            , 1, mean)
}

rbind(
    round(true_cvg(lasso), 3),
    round(true_cvg(bayes), 3),
    round(true_cvg(ridge), 3)
    )

rbind(
    round(summary(false_cvg(lasso)), 3),
    round(summary(false_cvg(bayes)), 3),
    round(summary(false_cvg(ridge)), 3))


g31 <-
tibble(
    var = paste0("X", true_pred),
    lasso = true_cvg(lasso),
    bayes = true_cvg(bayes),
    ridge = true_cvg(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "coverage_true") %>%
    ggplot(aes(x = var, y = coverage_true, group = method, colour = method)) +
 geom_hline(yintercept = .8, linetype = 2, color = "gray25") +
 geom_line() + geom_point() + theme_minimal()

g32 <-
tibble(
    lasso = false_cvg(lasso),
    bayes = false_cvg(bayes),
    ridge = false_cvg(ridge)) %>%
    pivot_longer(cols = c("lasso", "bayes", "ridge")
    , names_to = "method", values_to = "coverage_false") %>%
    ggplot(aes(x = method, y = coverage_false, colour = method)) +
 geom_hline(yintercept = .8, linetype = 2, color = "gray25") +
 geom_boxplot() + theme_minimal() + theme(legend.position = "none")

grid.arrange(g31, g32, nrow = 1)
png(paste0(output_folder, "Coverage.png")
    , width = 480 * 6, heigh = 480 * 2.5, res = 300)
grid.arrange(g31, g32, nrow = 1)
dev.off()
