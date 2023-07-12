### Example 4.1:  Shark Attacks --- Poisson Loglinear Regression ###

library(rjags)


### Set up data, initializations, and model

d <- read.table("ex4.1data.txt", header=TRUE)

inits <- list(list(beta1=0, beta2=0),
              list(beta1=1, beta2=1),
              list(beta1=-1, beta2=-1))

m <- jags.model("ex4.1model.bug", d, inits, n.chains=3)


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("beta1","beta2","lambda","y[14]","beta2.gt.0"),
                  n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("beta1","beta2","lambda","y[14]","beta2.gt.0"),
                  n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)


### Run 100000 more iterations

x <- coda.samples(m, c("beta1","beta2","lambda","y[14]","beta2.gt.0"),
                  n.iter=100000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 60000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x[,c("beta1","beta2","y[14]","beta2.gt.0")], 60000),
     trace=FALSE, ask=TRUE)
