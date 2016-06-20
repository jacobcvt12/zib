library(R2jags)

set.seed(1)
n <- 200

# latent classes
k <- 2
p <- c(1/3, 1-1/3)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)
alpha <- rep(1, k)

# overall beta parameters
a <- c(1, 4)
b <- c(5, 1)

# generate theta
theta <- rbeta(n, a[z], b[z])
plot(density(theta))

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

fit <- jags(model.data, NULL, model.params, model, n.iter=100000)
fit