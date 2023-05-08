library(tidyverse)
library(bindata)


# sim 1

# scenario i & ii

set.seed(1219)

n = 250
nvar = 50

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
  b4x8 = c(.4,.4,1.5, 1.5),
  b6x8 = c(.4,.4, -1.75,1.5),
  gamma = c(0.05, 0.05, 0.3, 0.3)
) %>%
  mutate(bs = pmap(list(b4, b6, b8, b9, b10,b4x6, b4x8, b6x8),
                   function(x4, x6, x8, x9, x10, x46, x48, x68){
                     c(0, 0, 0, x4, 0, x6, 0, x8, x9, x10, rep(0, 40),x46, x48, x68)
                   })
  )


# SCENARIO 1

#simulate the correlated bernoulli vars
# followed this procedure https://stackoverflow.com/questions/59595292/simulating-correlated-bernoulli-data

# variables associated with y
# in scenario i they are independent from each other i.e. correlation = 0
m1 <- matrix(0, 5,5)
diag(m1) <- 1
r1 <- rmvbin(n=n, margprob = c(.4, .6, .5, .3, .15), bincorr = m1)
round(cor(r_1),2)

#variables independent of Y given X4, X6, X8, X9, X10
##TODO clarify how to generate these as independent of Y given the above Xs
m2 <- matrix(0, 5, 5)
diag(m2) <- 1
r2 <- rmvbin(n = n, margprob = c(.4, .4, .4, .25, .2), bincorr = m2)
round(cor(r2), 2)

# generate remaining 40 variables
# independent of each other

#x11-x15 - separating here bc in later simulations they have diff structure
m3 <- matrix(0, 5, 5)
diag(m3) <- 1
r3 <- rmvbin(n = n, margprob = c(rep(.1, 5)),
             bincorr = m3)
round(cor(r3), 2)

# x16-50 independent from each other with marginal probability of success as in web table 1
m4 <- matrix(0, 35, 35)
diag(m4) <- 1
r4<- rmvbin(n = n, margprob = c(rep(.1, 5), rep(.2, 10), rep(.25, 10), rep(.3, 10)),
             bincorr = m4)
summary(round(cor(r4), 2))

# generate NULL matrix placeholder
xs <- matrix(rep(NA, nvar*n), ncol = nvar)

# insert the generated variables
xs[,c(4,6,8,9,10)] <- r1
xs[,c(1,2,3,5,7)] <- r2
xs[,c(11:15)] <- r3
xs[,c(16:nvar)] <- r4

# check that probabilities look right given setup
summary(xs)
# yep!

# add known interaction terms for easy generation of Ys
xs_inter <- cbind(xs, xs[,4]*xs[,6], xs[,4]*xs[,8], xs[,6]*xs[,8])

outcome <- rep(sim1i_bs$a[1], n) + colSums(t(xs_inter)*(sim1i_bs$bs[[1]])) + (sim1i_bs$gamma[1])*z

pr1 <- exp(outcome)/(1+exp(outcome))
runis = runif(n, 0, 1)
y1 = ifelse(runis < pr1, 1, 0)

summary(y1)

#now make dataframe for analysis

dat1 <- data.frame(xs) %>%
  mutate(z = z,
         y = y1) %>%
  select(y, z, everything())


# SCENARIO 2

#simulate the correlated bernoulli vars
# followed this procedure https://stackoverflow.com/questions/59595292/simulating-correlated-bernoulli-data

# variables associated with y
# in scenario i they are independent from each other i.e. correlation = 0
m1 <- matrix(0, 5,5)
diag(m1) <- 1
r1 <- rmvbin(n=n, margprob = c(.4, .6, .5, .3, .15), bincorr = m1)
round(cor(r_1),2)

#variables independent of Y given X4, X6, X8, X9, X10
##TODO clarify how to generate these as independent of Y given the above Xs
m2 <- matrix(0, 5, 5)
diag(m2) <- 1
r2 <- rmvbin(n = n, margprob = c(.4, .4, .4, .25, .2), bincorr = m2)
round(cor(r2), 2)

# generate remaining 40 variables
# independent of each other

#x11-x15 - separating here bc in later simulations they have diff structure
m3 <- matrix(0, 5, 5)
diag(m3) <- 1
r3 <- rmvbin(n = n, margprob = c(rep(.1, 5)),
             bincorr = m3)
round(cor(r3), 2)

# x16-50 independent from each other with marginal probability of success as in web table 1
m4 <- matrix(0, 35, 35)
diag(m4) <- 1
r4<- rmvbin(n = n, margprob = c(rep(.1, 5), rep(.2, 10), rep(.25, 10), rep(.3, 10)),
            bincorr = m4)
summary(round(cor(r4), 2))

# generate NULL matrix placeholder
xs <- matrix(rep(NA, nvar*n), ncol = nvar)

# insert the generated variables
xs[,c(4,6,8,9,10)] <- r1
xs[,c(1,2,3,5,7)] <- r2
xs[,c(11:15)] <- r3
xs[,c(16:nvar)] <- r4

# check that probabilities look right given setup
summary(xs)
# yep!

# add known interaction terms for easy generation of Ys
xs_inter <- cbind(xs, xs[,4]*xs[,6], xs[,4]*xs[,8], xs[,6]*xs[,8])

outcome <- rep(sim1i_bs$a[1], n) + colSums(t(xs_inter)*(sim1i_bs$bs[[1]])) + (sim1i_bs$gamma[1])*z

pr1 <- exp(outcome)/(1+exp(outcome))
runis = runif(n, 0, 1)
y1 = ifelse(runis < pr1, 1, 0)

summary(y1)

#now make dataframe for analysis

dat1 <- data.frame(xs1) %>%
  mutate(z = z,
         y = y1) %>%
  select(y, z, everything())




# generate matrix of bernoulli variables

# here there is no correlation structure inside
# TODO add the 4 proper generated x matrices specific to each scenario
xs <- matrix(rbinom(n*nvar, 1, .5), ncol = nvar)

# add known interaction terms
xs_inter <- cbind(xs, xs[,4]*xs[,6], xs[,4]*xs[,8], xs[,6]*xs[,8])
# generate matching variable Z which follows uniform from 1:10
z <- runif(n, 1, 10)
runis = runif(n, 0, 1)

res <- sim1i_bs %>%
  mutate(
    outcome = pmap(list(a,bs,gamma), function(xa, xbs, xgamma){
      rep(xa, n) + colSums(t(xs_inter)*xbs) + xgamma*z}),
    pr1 = map(outcome, ~exp(.x)/(1+exp(.x))),
    y1 = map(pr1, ~ifelse(runis < .x, 1, 0))
  )

