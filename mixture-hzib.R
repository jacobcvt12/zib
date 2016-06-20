library(VGAM)
library(R2jags)

set.seed(1)
n <- 300
pts <- rpois(n, 100)

# latent classes
k <- 2
p <- c(1/3, 1-1/3)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)
alpha <- rep(1, k)

# overall beta parameters
a <- c(2, 4)
b <- c(5, 1)

# shared probability of structural zero
#phi <- 0.1

theta <- rbeta(n, a[z], b[z])
#pass <- rzibinom(n, pts, theta, pstr0=phi)
pass <- rbinom(n, pts, theta)
plot(density(theta))

model <- function() {
    for (i in 1:n) {
        pass[i] ~ dbinom(theta[i], pts[i])
        #pass[i] ~ dbinom(zi.theta[i], pts[i])
        #zi.theta[i] <- theta[i] * zi[i] + 0.00001
        theta[i] ~ dbeta(a[z[i]], b[z[i]])
        #zi[i] ~ dbern(phi.inverse)
        z[i] ~ dcat(pi[])
    }
    
    # priors
    for (cluster in 1:k) {
        a[cluster] ~ dunif(0, 1000)
        b[cluster] ~ dunif(0, 1000)
    }

    #phi <- 1 - phi.inverse
    #phi.inverse ~ dbeta(1, 1)
    pi ~ ddirch(alpha[])
}

model.data <- c("pass", "pts", "n", "alpha", "k")
model.params <- c("a", "b", "pi")

fit <- jags(model.data, NULL, model.params, model, n.iter=100000)
fit