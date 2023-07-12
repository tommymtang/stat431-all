### Example 4.2:  Dyestuff Data --- Normal Random-Effects ###

library(rjags)


### Set up data, initializations, and model

d <- list(y=read.table("ex4.2data.txt"))

inits <- list(list(mu=1500, tausqW=0.001, sigmaB=100), 
              list(mu=3000, tausqW=1, sigmaB=1), 
              list(mu=0, tausqW=0.000001, sigmaB=10000))

m <- jags.model("ex4.2model.bug", d, inits, n.chains=3)

  # alpha[i] values are automatically initialized


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("alpha","mu","sigmasqW","sigmasqB","rho"), n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("alpha","mu","sigmasqW","sigmasqB","rho"), n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Run 100000 more iterations

x <- coda.samples(m, c("alpha","mu","sigmasqW","sigmasqB","rho"), n.iter=100000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 60000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 60000), trace=FALSE, ask=TRUE)


### Plot sigmasqB versus sigmasqW (log scale)

plot(as.matrix(window(x, 60000)[,c("sigmasqW","sigmasqB")]), log="xy",
     pch=".", cex=2)
