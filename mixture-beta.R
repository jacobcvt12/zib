library(R2jags)
library(ggplot2)
library(shinystan)

set.seed(1)
n <- 100

# latent classes
k <- 3
p <- c(1/4, 1/2, 1/4)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)
alpha <- rep(1, k)

# overall beta parameters
a <- c(1, 50, 99)
b <- c(99, 50, 1)

# generate theta
theta <- rbeta(n, a[z], b[z])
#ggplot(data.frame(theta), aes(theta)) +
#       geom_density()

model <- function() {
    for (i in 1:n) {
        theta[i] ~ dbeta(a[z[i]], b[z[i]])
        z[i] ~ dcat(pi[])
    }
    
    # priors
    for (cluster in 1:k) {
        a[cluster] ~ dunif(0, 1000)
        b[cluster] ~ dunif(0, 1000)
    }

    pi ~ ddirch(alpha[])
}

model.data <- c("theta", "alpha", "k", "n")
model.params <- c("a", "b", "pi", "z")

fit <- jags(model.data, NULL, model.params, model, n.iter=500000)
saveRDS(fit, file="mixture-beta.RDS")
#launch_shinystan(as.shinystan(as.mcmc(fit)))
