library(R2jags)
library(ggplot2)

set.seed(1)
n <- 100

# latent classes
k <- 2
p <- c(1/3, 1-1/3)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)
alpha <- rep(1, k)

# overall beta parameters
a <- c(2, 40)
b <- c(50, 10)

# generate theta
theta <- rbeta(n, a[z], b[z])
ggplot(data.frame(theta), aes(theta)) +
    geom_density()

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
model.params <- c("a", "b", "pi")

fit <- jags(model.data, NULL, model.params, model, n.iter=20000)
fit
