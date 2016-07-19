library(R2jags)
library(coda)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())

# read in results

# beta binomial
fit.bb <- readRDS("output/beta-binomial.RDS")
bb.mcmc <- do.call(rbind, as.mcmc(fit.bb)) # combine chains
bb.mcmc <- bb.mcmc[ , 2:ncol(bb.mcmc)]
colnames(bb.mcmc)[1] <- "between.var"

# beta mixture binomial (true K=3)
fit.bmb <- readRDS("output/betamix-binomial.RDS")
bmb.mcmc <- do.call(rbind, as.mcmc(fit.bmb)) # combine chains
bmb.mcmc[, 1] <- bmb.mcmc[, ncol(bmb.mcmc)]
bmb.mcmc <- bmb.mcmc[ , 1:(ncol(bmb.mcmc)-1)]
colnames(bmb.mcmc)[1] <- "between.var"

# Dirichlet process binomial
fit.dpb <- readRDS("output/dp-binomial.RDS")
dpb.mcmc <- do.call(rbind, as.mcmc(fit.dpb)) # combine chains
dpb.mcmc[, 1] <- dpb.mcmc[, ncol(dpb.mcmc)]
dpb.mcmc <- dpb.mcmc[ , 1:(ncol(dpb.mcmc)-1)]
colnames(dpb.mcmc)[1] <- "between.var"

# function to calculate reliability
reliability <- function(mcmc) {
  # calculate variance of each posterior p_i
  binomial.error <- apply(mcmc[ ,2:ncol(mcmc)], 2, var)
  
  # get the posterior samples of the between variance
  between.variance <- mcmc[, "between.var"]
  
  # get list of reliability curves
  reliability.list <- lapply(between.variance, 
                             function(x) x / (x + binomial.error))
  # put lists into matrix
  reliability.matrix <- do.call(rbind, reliability.list)
  
  # now calculate the credible intervals for each point 
  # as well as posterior median
  reliability.intervals <- apply(reliability.matrix, 2, 
                                 quantile, c(0.025, 0.5, 0.975))
  
  # order by the medians
  rel.order <- order(reliability.intervals[2, ])
  reliability.intervals <- reliability.intervals[, rel.order]
  
  return(reliability.intervals)
}

# graphic of between variance
bb.df <- data.frame(bb.mcmc)
bmb.df <- data.frame(bmb.mcmc)
dpb.df <- data.frame(dpb.mcmc)
bb.df$model <- "Beta Binomial"
bmb.df$model <- "Beta-Mixture Binomial"
dpb.df$model <- "DP Binomial"
cols <- c(1, ncol(bb.df))

var.data <- bind_rows(bb.df[, cols], 
                      bmb.df[, cols], 
                      dpb.df[, cols]) %>% 
    mutate(model=factor(model,
                        levels=c("Beta Binomial",
                                 "Beta-Mixture Binomial",
                                 "DP Binomial")))

var.data.hpd <- var.data %>% 
    group_by(model) %>% 
    summarise(hpdl=HPDinterval(as.mcmc(between.var))[1],
              hpdu=HPDinterval(as.mcmc(between.var))[2])

pdf("doc/variance.pdf")
ggplot(var.data, aes(x=between.var)) +
    geom_density() +
    geom_vline(data=var.data.hpd, aes(xintercept=hpdl),
               colour="blue", linetype=2) +
    geom_vline(data=var.data.hpd, aes(xintercept=hpdu),
               colour="blue", linetype=2) +
    geom_vline(xintercept=0.04259, colour="orange", 
               linetype=2) +
    facet_wrap(~model, nrow=3) +
    scale_y_continuous(breaks=NULL) +
    xlab("Between Variance") +
    ggtitle("Comparison of Between Variance Approximations")
dev.off()

# graphic of reliability
rel.bb <- data.frame(t(reliability(bb.mcmc)))
rel.bb$id <- 1:nrow(rel.bb)
rel.bb$model <- "Beta Binomial"

rel.bmb <- data.frame(t(reliability(bmb.mcmc)))
rel.bmb$id <- 1:nrow(rel.bmb)
rel.bmb$model <- "Beta-Mixture Binomial"

rel.dpb <- data.frame(t(reliability(dpb.mcmc)))
rel.dpb$id <- 1:nrow(rel.dpb)
rel.dpb$model <- "DP Binomial"

rel.data <- bind_rows(rel.bb, rel.bmb, rel.dpb) %>% 
    mutate(model=factor(model,
                        levels=c("Beta Binomial",
                                 "Beta-Mixture Binomial",
                                 "DP Binomial")))

pdf("doc/reliability.pdf")
ggplot(rel.data, aes(x=id, y=X50.)) +
  geom_line() +
  geom_ribbon(aes(ymin=X2.5., ymax=X97.5.),
              fill="blue",
              alpha=0.05) +
  facet_wrap(~model) +
  scale_x_continuous(breaks=NULL) +
  xlab("") +
  ylab("Reliability") +
  ggtitle("Reliability Estimated by Site")
dev.off()

