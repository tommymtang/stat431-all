### Example 5.2:  Shark Attacks --- Posterior Predictive Checking ###

library(rjags)


### Set up data, initializations, and model

d <- read.table("ex5.2data.txt", header=TRUE)

inits <- list(list(beta1=0, beta2=0),
              list(beta1=1, beta2=1),
              list(beta1=-1, beta2=-1))

m1 <- jags.model("ex5.2model1.bug", d, inits, n.chains=3)

m2 <- jags.model("ex5.2model2.bug", d, inits, n.chains=3)


### Routine burn-in

update(m1, 10000)

update(m2, 10000)


### Run 100000 more iterations

x1 <- coda.samples(m1, c("chisq","chisqrep","pb.ind"), n.iter=100000)

x2 <- coda.samples(m2, c("chisq","chisqrep","pb.ind"), n.iter=100000)


### Check stats

summary(x1)

summary(x2)

  # Verify: Time-series SE less than 1/20 of SD

plot(x1, trace=FALSE, ask=TRUE)

plot(x2, trace=FALSE, ask=TRUE)


### A plot suggested by Gelman, Meng, and Stern (1996)

plot(as.matrix(x1)[,"chisq"], as.matrix(x1)[,"chisqrep"], pch=".", cex=2)
abline(0, 1, col="blue")

plot(as.matrix(x2)[,"chisq"], as.matrix(x2)[,"chisqrep"], pch=".", cex=2)
abline(0, 1, col="blue")
