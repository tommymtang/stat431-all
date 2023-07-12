### Example 4.3:  Baby Rat Growth --- Hierarchical Normal (Univariate) ###

library(rjags)


### Set up data, initializations, and model

ages <- c(8, 15, 22, 29, 36)

d <- list(Y = read.table("ex4.3data.txt"),
          X = ages,
          Xbar = mean(ages))

inits <- list(list(tausq.y=1, beta1=0, beta2=0,
                   sigma.alpha1=1, sigma.alpha2=1),
              list(tausq.y=100, beta1=100, beta2=100,
                   sigma.alpha1=0.1, sigma.alpha2=0.1),
              list(tausq.y=0.01, beta1=-100, beta2=-100,
                   sigma.alpha1=10, sigma.alpha2=10))

m <- jags.model("ex4.3model.bug", d, inits, n.chains=3)


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("beta1","beta2","sigma.y","sigma.alpha1","sigma.alpha2"),
                  n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("beta1","beta2","sigma.y","sigma.alpha1","sigma.alpha2"),
                  n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 4000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 4000), trace=FALSE, ask=TRUE)


### Posterior correlations between beta1 and beta2?

plot(as.matrix(window(x, 4000)[,c("beta1","beta2")]), pch=".")

   # none apparent, probably because X values are mean-centered
