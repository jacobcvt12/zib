# generate binomial draws using hierarchical 3-mixture beta
set.seed(42)
n <- 100

# latent classes
k <- 3
p <- rep(1/k, k)
z <- sample(seq_len(k), n, replace=TRUE, prob=p)

# overall beta parameters
a <- c(1, 5, 9)
b <- c(9, 5, 1)

# calculate true overall means and *variance* of the mixture
means <- a / (a + b)
variances <- means * (1 - means) / (a + b + 1)
mean.overall <- means %*% p
var.overall <- p %*% (means^2 + variances) - mean.overall^2

# generate theta
theta <- rbeta(n, a[z], b[z])

# generate binomials
X <- rbinom(n, 100, theta)

# save data to use for models
saveRDS(X, "data/binomial.RDS")
