library(tidyverse)
library(bindata)


# sim 1

# scenario i & ii

set.seed(1219)

n    <- 25000
nvar <- 50
n_pairs <- 25 #per z type
z <- as.numeric(cut(1:n, quantile(1:n, seq(0, 1, .1)), include.lowest = TRUE))
table(z)

# all 4 scenarios data generation parameters
sim1i_bs <- data.frame(
  i = c(1:4),
  a = c(-1, -1, -2, -2),
  b4 = c(-0.4, -0.4, -0.5, -0.75),
  b6 = c(.4, .4, .5, 1.25),
  b8 = c(.4, .4, .5, 1),
  b9 = c(-.4, -.4, .5, .75),
  b10 = c(.25, .25, .5, 1),
  b4x6 = c(-.4, -.4, -1.75, 1.5),
  b4x8 = c(.4, .4, 1.5, 1.5),
  b6x8 = c(.4, .4, -1.75, 1.5),
  gamma = c(0.05, 0.05, 0.3, 0.3)
) %>%
  mutate(bs = pmap(list(b4, b6, b8, b9, b10, b4x6, b4x8, b6x8),
                   function(x4, x6, x8, x9, x10, x46, x48, x68) {
                     c(0, 0, 0, x4, 0, x6, 0, x8, x9, x10
                     , rep(0, 40), x46, x48, x68)
                     })
  )


saveRDS(sim1i_bs, "data/sim1i_bs")




#===============================================================================
# SCENARIO 1
#===============================================================================
#' simulate the correlated bernoulli vars followed this procedure
#' https://stackoverflow.com/questions/59595292/simulating-correlated-bernoulli-data

s1 <- function(r) {
  # variables associated with y
  # in scenario i they are independent from each other i.e. correlation = 0
  m1 <- matrix(0, 5, 5)
  diag(m1) <- 1
  m1
  r1 <- rmvbin(n = n, margprob = c(.4, .6, .5, .3, .15), bincorr = m1)
  round(cor(r1), 1)

  #variables independent of Y given X4, X6, X8, X9, X10
  ##TODO clarify how to generate these as independent of Y given the above Xs
  #' LGG: It seems that "given" is meaningless in scenario 1 & 2
  #' since none of the other variables are associated with X4, X6, X8, X9, X10
  #' they are both marginally and conditionally intendent of Y
  #' this is different for X1,X2,X3 in scenarios 3 & 4
  #' which will show a marginal (spurious association)
  #' due to their association with X4, X6, X8

  m2 <- matrix(0, 5, 5)
  diag(m2) <- 1
  r2 <- rmvbin(n = n, margprob = c(.4, .4, .4, .25, .2), bincorr = m2)
  round(cor(r2), 1)

  # generate remaining 40 variables
  # independent of each other

  #x11-x15 - separating here bc in later simulations they have diff structure
  m3 <- matrix(0, 5, 5)
  diag(m3) <- 1
  r3 <- rmvbin(n = n, margprob = c(rep(.1, 5)),
              bincorr = m3)
  round(cor(r3), 1)

  # x16-50 independent from each other 
  #with marginal probability of success as in web table 1
  m4 <- matrix(0, 35, 35)
  diag(m4) <- 1
  r4 <- rmvbin(n = n
        , margprob = c(rep(.1, 5), rep(.2, 10), rep(.25, 10), rep(.3, 10))
        , bincorr = m4)
  summary(round(cor(r4), 1))

  # generate NULL matrix placeholder
  xs <- matrix(rep(NA, nvar * n), ncol = nvar)

  # insert the generated variables
  xs[, c(4, 6, 8, 9, 10)] <- r1
  xs[, c(1, 2, 3, 5, 7)] <- r2
  xs[, c(11:15)] <- r3
  xs[, c(16:nvar)] <- r4

  # check that probabilities look right given setup
  summary(xs)
  # yep!

  # add known interaction terms for easy generation of Ys
  #xs_inter <- cbind(xs, xs[, 4] * xs[, 6], xs[, 4] * xs[, 8], xs[, 6] * xs[, 8])

  #outcome <- rep(sim1i_bs$a[1], n) +
  #            colSums(t(xs_inter) * (sim1i_bs$bs[[1]])) +
  #            (sim1i_bs$gamma[1]) * z



  # can we create a matrix with the predictors that mater?
  X <- cbind(1, xs[, 4], xs[, 6], xs[, 8], xs[, 9], xs[, 10]
            , xs[, 4] * xs[, 6], xs[, 4] * xs[, 8], xs[, 6] * xs[, 8], z)


  outcome  <- X %*% t(sim1i_bs[2:11][1, ])
  pr1      <- plogis(outcome)
  runis    <- runif(n, 0, 1)
  y1       <- as.numeric(runis < pr1)

  #now make dataframe for analysis

  dat1 <- data.frame(xs) %>%
    mutate(z = z,
           y = y1) %>%
    select(y, z, everything())

  #generate matched case-control
  dat1m <-
    dat1 %>%
    group_by(strata = paste0(z, y)) %>%
    mutate(id = z * 10000 + 1:n()) %>%
    group_by(z) %>%
    mutate(mcc = id %in% sample(unique(id[y == 1]), n_pairs)) %>%
    filter(mcc) %>%
    mutate(rep = r, mcc = NULL,  strata = NULL) %>%
    ungroup

  dat1m
}

dat1 <- map_df(1:50, s1)
summary(dat1)

saveRDS(dat1, "data/dat1")



#===============================================================================
# SCENARIO 2
#===============================================================================
#simulate the correlated bernoulli vars
# followed this procedure https://stackoverflow.com/questions/59595292/simulating-correlated-bernoulli-data

# variables associated with y
# in scenario ii they are associated 
m1 <- matrix(c(1, -.6, -.6, -.6, 1, .65, -.6, .65, 1), 3,3)
r1 <- rmvbin(n=n, margprob = c(.4, .6, .5), sigma = m1)
round(cor(r1),2)

m1b <- matrix(c(1, .6, .6, 1), 2,2)
r1b <- rmvbin(n=n, margprob = c(.3, .15), sigma = m1b)
round(cor(r1b),2)

#variables independent of Y given X4, X6, X8, X9, X10
##TODO clarify how to generate these as independent of Y given the above Xs
m2a <-  matrix(c(1, .65, .6, .65, 1, .65, .6, .65, 1), 3,3)
r2a<- rmvbin(n = n, margprob = c(.4, .4, .4), bincorr = m2a)
round(cor(r2a), 2)

m2b <-  matrix(c(1, .6, .6, 1), 2,2)
r2b<- rmvbin(n = n, margprob = c(.25, .2), sigma = m2b)
round(cor(r2b), 2)

# generate remaining 40 variables
# independent of each other

#x11-x15 - separating here bc in later simulations they have diff structure
m3 <- matrix(c(1,-.549,.807,-.440,.421,-.549,1,-.566,.318,-.368,.807,-.566,1,-.403,.536,-.44,.318,-.403,1,-.309,.421,-.368,.536,-.309,1), 5, 5)
r3 <- rmvbin(n = n, margprob = c(.24,.21,.36,.31,.23),
             sigma = m3)
round(cor(r3), 2)

# x16-50 independent from each other with marginal probability of success as in web table 1
m4 <- matrix(0, 35, 35)
diag(m4) <- 1
r4<- rmvbin(n = n, margprob = c(rep(.1, 5), rep(.2, 10), rep(.25, 10), rep(.3, 10)),
            sigma = m4)
summary(round(cor(r4), 2))

# generate NULL matrix placeholder
xs2 <- matrix(rep(NA, nvar*n), ncol = nvar)

# insert the generated variables
xs2[,c(4,6,8)] <- r1
xs2[,c(9,10)] <- r1b
xs2[,c(1,2,3)] <- r2a
xs2[,c(5,7)] <- r2b
xs2[,c(11:15)] <- r3
xs2[,c(16:nvar)] <- r4

# check that probabilities look right given setup
summary(xs2)
# yep!

# add known interaction terms for easy generation of Ys
xs_inter2 <- cbind(xs2, xs2[,4]*xs2[,6], xs2[,4]*xs2[,8], xs2[,6]*xs2[,8])

outcome2 <- rep(sim1i_bs$a[2], n) + colSums(t(xs_inter2)*(sim1i_bs$bs[[2]])) + (sim1i_bs$gamma[2])*z

pr2 <- exp(outcome2)/(1+exp(outcome2))
runis2 = runif(n, 0, 1)
y2= ifelse(runis2 < pr2, 1, 0)

summary(y2)

#now make dataframe for analysis

dat2 <- data.frame(xs2) %>%
  mutate(z = z,
         y = y2) %>%
  select(y, z, everything())

saveRDS(dat2, file = "data/dat2")

#################
# SCENARIO 3
#################

#simulate the correlated bernoulli vars
# followed this procedure https://stackoverflow.com/questions/59595292/simulating-correlated-bernoulli-data

#auxilliary variables
# corr
mu <- matrix(c(1, .7, .7, .7, 1, .8, .7, .8, 1), 3,3)
ru <- rmvbin(n=n, margprob = c(.3, .26, .35), bincorr = mu)
round(cor(ru),2)

w = rbinom(n, 1, .75)


# variables associated with y
# in scenario i they are independent from each other i.e. correlation = 0
# m1 <- matrix(c(1, -.6, -.6, -.6, 1, .65, -.6, .65, 1), 3,3)
# r1 <- rmvbin(n=n, margprob = c(.4, .6, .5), bincorr = m1)
# round(cor(r1),2)

x4 <-  w*map_int(z, ~rbinom(1,1,.x/15)) + (1-w)*ru[1]
x6 <-  w*map_int(z, ~rbinom(1,1,.x/10)) + (1-w)*ru[1]
x8 <-  w*map_int(z, ~rbinom(1,1,.x/17.5)) + (1-w)*ru[1]

m1b <- matrix(0, 2,2)
diag(m1b) <- 1
r1b <- rmvbin(n=n, margprob = c(.3, .15), sigma = m1b)
round(cor(r1b),2)

#variables independent of Y given X4, X6, X8, X9, X10
##TODO clarify how to generate these as independent of Y given the above Xs
u1 <- rbinom(n, 1, .4)
u2 <- rbinom(n, 1, .4)
u3 <- rbinom(n, 1, .4)

p1 <- 0.55*x4 + 0.45*u1
p2 <- 0.55*x6+ 0.45*u2
p3 <- 0.55*x8 + 0.45*u3

x1 <- rbinom(n, 1, p1)
x2 <- rbinom(n, 1, p2)
x3 <- rbinom(n, 1, p3)

m2b <- matrix(0, 2,2)
diag(m2b) <- 1
r2b <- rmvbin(n = n, margprob = c(.25, .2), sigma = m2b)
round(cor(r2b), 2)

# generate remaining 40 variables
# independent of each other

#x11-x15 - separating here bc in later simulations they have diff structure
m3 <- matrix(c(1,-.549,.807,-.440,.421,-.549,1,-.566,.318,-.368,.807,-.566,1,-.403,.536,-.44,.318,-.403,1,-.309,.421,-.368,.536,-.309,1), 5, 5)
r3 <- rmvbin(n = n, margprob = c(.24,.21,.36,.31,.23),
             sigma = m3)
round(cor(r3), 2)

# x16-50 independent from each other with marginal probability of success as in web table 1
m4 <- matrix(0, 35, 35)
diag(m4) <- 1
r4<- rmvbin(n = n, margprob = c(rep(.15, 5), rep(.2, 10), rep(.25, 10), rep(.2, 10)),
            sigma = m4)
summary(round(cor(r4), 2))

# generate NULL matrix placeholder
xs3 <- matrix(rep(NA, nvar*n), ncol = nvar)

# insert the generated variables
xs3[,c(4,6,8)] <- cbind(x4,x6,x8)
xs3[,c(9,10)] <- r1b
xs3[,c(1,2,3)] <- cbind(x1,x2,x3)
xs3[,c(5,7)] <- r2b
xs3[,c(11:15)] <- r3
xs3[,c(16:nvar)] <- r4

# check that probabilities look right given setup
summary(xs3)
# yep!

# add known interaction terms for easy generation of Ys
xs_inter3 <- cbind(xs3, xs3[,4]*xs3[,6], xs3[,4]*xs3[,8], xs3[,6]*xs3[,8])

outcome3 <- rep(sim1i_bs$a[3], n) + colSums(t(xs_inter3)*(sim1i_bs$bs[[3]])) + (sim1i_bs$gamma[3])*z

pr3 <- exp(outcome3)/(1+exp(outcome3))
runis3= runif(n, 0, 1)
y3= ifelse(runis3 < pr3, 1, 0)

summary(y3)

#now make dataframe for analysis

dat3 <- data.frame(xs3) %>%
  mutate(z = z,
         y = y3) %>%
  select(y, z, everything())

saveRDS(dat3, file = "data/dat3")
#===============================================================================
# SCENARIO 4
#===============================================================================
#simulate the correlated bernoulli vars
# followed this procedure https://stackoverflow.com/questions/59595292/simulating-correlated-bernoulli-data

# variables associated with y
# in scenario iv they are associated 
m1 <- matrix(c(1, .45,.5, .45, 1, .6, .5, .6, 1), 3,3)
r1 <- rmvbin(n=n, margprob = c(.3, .56, .36), bincorr = m1)
round(cor(r1),2)


m1b <- matrix(c(1,.6,.6,1), 2,2)
r1b <- rmvbin(n=n, margprob = c(.3, .15), sigma = m1b)
round(cor(r1b),2)

#variables independent of Y given X4, X6, X8, X9, X10
##TODO clarify how to generate these as independent of Y given the above Xs
u1 <- rbinom(n, 1, .4)
u2 <- rbinom(n, 1, .4)
u3 <- rbinom(n, 1, .4)

p1 <- 0.25*r1[,1] + 0.75*u1
p2 <- 0.25*r1[,2]+ 0.75*u2
p3 <- 0.25*r1[,3] + 0.75*u3

x1 <- rbinom(n, 1, p1)
x2 <- rbinom(n, 1, p2)
x3 <- rbinom(n, 1, p3)

m2b <- matrix(0, 2,2)
diag(m2b) <- 1
r2b<- rmvbin(n = n, margprob = c(.25, .2), sigma = m2b)
round(cor(r2b), 2)

# generate remaining 40 variables
# independent of each other

#x11-x15 - separating here bc in later simulations they have diff structure
m3 <- matrix(c(1,-.549,.807,-.440,.421,-.549,1,-.566,.318,-.368,.807,-.566,1,-.403,.536,-.44,.318,-.403,1,-.309,.421,-.368,.536,-.309,1), 5, 5)
r3 <- rmvbin(n = n, margprob = c(.24,.21,.36,.31,.23),
             sigma = m3)
round(cor(r3), 2)

# x16-50 independent from each other with marginal probability of success as in web table 1
m4 <- matrix(0, 35, 35)
diag(m4) <- 1
r4<- rmvbin(n = n, margprob = c(rep(.15, 5), rep(.2, 10), rep(.25, 10), rep(.2, 10)),
            sigma= m4)
summary(round(cor(r4), 2))

# generate NULL matrix placeholder
xs4 <- matrix(rep(NA, nvar*n), ncol = nvar)

# insert the generated variables
xs4[,c(4,6,8)] <- r1
xs4[,c(9,10)] <- r1b
xs4[,c(1,2,3)] <- cbind(x1,x2,x3)
xs4[,c(5,7)] <- r2b
xs4[,c(11:15)] <- r3
xs4[,c(16:nvar)] <- r4

# check that probabilities look right given setup
summary(xs4)
# yep!

# add known interaction terms for easy generation of Ys
xs_inter4 <- cbind(xs4, xs4[,4]*xs4[,6], xs4[,4]*xs4[,8], xs4[,6]*xs4[,8])

outcome4 <- rep(sim1i_bs$a[4], n) + colSums(t(xs_inter4)*(sim1i_bs$bs[[4]])) + (sim1i_bs$gamma[4])*z

pr4 <- exp(outcome4)/(1+exp(outcome4))
runis4= runif(n, 0, 1)
y4= ifelse(runis4 < pr4, 1, 0)

summary(y4)

#now make dataframe for analysis

dat4 <- data.frame(xs4) %>%
  mutate(z = z,
         y = y4) %>%
  select(y, z, everything())

saveRDS(dat4, "data/dat4")

# # generate matrix of bernoulli variables
# 
# # here there is no correlation structure inside
# # TODO add the 4 proper generated x matrices specific to each scenario
# xs <- matrix(rbinom(n*nvar, 1, .5), ncol = nvar)
# 
# # add known interaction terms
# xs_inter <- cbind(xs, xs[,4]*xs[,6], xs[,4]*xs[,8], xs[,6]*xs[,8])
# # generate matching variable Z which follows uniform from 1:10
# z <- runif(n, 1, 10)
# runis = runif(n, 0, 1)
# 
# res <- sim1i_bs %>%
#   mutate(
#     outcome = pmap(list(a,bs,gamma), function(xa, xbs, xgamma){
#       rep(xa, n) + colSums(t(xs_inter)*xbs) + xgamma*z}),
#     pr1 = map(outcome, ~exp(.x)/(1+exp(.x))),
#     y1 = map(pr1, ~ifelse(runis < .x, 1, 0))
#   )