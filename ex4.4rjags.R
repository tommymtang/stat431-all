### Example 4.4:  Baby Rat Growth --- Hierarchical Normal (Bivariate) ###

library(rjags)


### Set up data, initializations, and model

ages <- c(8, 15, 22, 29, 36)

d <- list(Y = read.table("ex4.4data.txt"),
          X = ages,
          Xbar = mean(ages),
          Omega0 = rbind(c(100, 0),
                         c(0, 0.1)),
          mu0 = c(0,0),
          Sigma0.inv = rbind(c(1.0E-6, 0),
                             c(0, 1.0E-6)))

inits <- list(list(tausq.y=1, beta=c(0,0),
                   Omega.inv=diag(2)),
              list(tausq.y=100, beta=c(100,100),
                   Omega.inv=100*diag(2)),
              list(tausq.y=0.01, beta=c(-100,-100),
                   Omega.inv=0.01*diag(2)))

m <- jags.model("ex4.4model.bug", d, inits, n.chains=3)


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("beta","sigma.y","Omega","rho"), n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("beta","sigma.y","Omega","rho"), n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 4000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 4000), trace=FALSE, ask=TRUE)


### Posterior correlations between beta1 and beta2?

plot(as.matrix(window(x, 4000)[,c("beta[1]","beta[2]")]), pch=".")

   # apparently so!
