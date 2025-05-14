# Script to perform a simulation to estimate the precision of the primary
# outcome for the project:
# "Computational Reproducibility in Sports Science"
# date: 05/2025
# author: Simon Nolte

# load packages
library(lme4)       # model fit
library(DescTools)  # binomial CI
library(dplyr)      # data wrangling
library(ggplot2)    # visualization

# default for study level reproducibility is beta(2, 2)
sim <- function(n = 50, a = 2, b = 2, pooled = "partial") {
  d <- data.frame(
    id = seq_len(n)
  )
  
  # use a negative binomial distribution with n = 1.5 and p = 0.25 to simulate 
  # the number of primary claims per study
  d$pcn <- rnbinom(n, 1.5, 0.25)
  
  # use a beta distribution to simulate the proportion per study
  d$prop <- rbeta(n, a, b)
  
  # drop studies with zero primary claims
  d <- d[d$pcn != 0, ]
  
  # create one row per primary claim
  d_long <- d[rep(1:nrow(d), d$pcn), ]
  
  # simulate success for each primary claim
  d_long$number <- ave(d_long$id, d_long$id, FUN = seq_along)
  d_long$value <- mapply(function(p) rbinom(1, 1, p), d_long$prop)
  
  if (pooled == "partial") {
    # partial pooling
    mod <- glmer(value ~ 1 + (1 | id), family = binomial, data = d_long)
    prop <- plogis(fixef(mod))
    ci_logit <- confint(mod, parm = "(Intercept)", method = "Wald")
    ci_prop <- plogis(ci_logit)
    data.frame(
      x = prop[[1]],
      ci_lower = ci_prop[,1],
      ci_upper = ci_prop[,2]
    )
  } else {
    if (pooled == "full") {
      # completely pooled
      o <- BinomCI(sum(d_long$value), nrow(d_long), conf.level = 0.95, method = "wald")
    } else if (pooled == "no") {
      # not pooled
      mean_props <- aggregate(value ~ id, data = d_long, FUN = mean)
      o <- BinomCI(mean(mean_props$value) * nrow(mean_props), nrow(mean_props), conf.level = 0.95, method = "wald")
    } else {
      stop("Choose 'partial', 'full' or 'no' for pooled argument.")
    }
    data.frame(
      x = o[[1,1]],
      ci_lower = o[[1,2]],
      ci_upper = o[[1,3]]
    )
  }
}

# set random seed
set.seed(4711)
# run 1000 simulations (uncomment to run!)
#sim_results <- purrr::map_dfr(1:1000, ~ sim(pooled = "partial"))
# save and load simulation results (uncomment to run!)
#save(sim_results, file = "data/sim_results.Rda")
load("data/sim_results.Rda")
# mean values for parameter estimate and CI bounds
mean(sim_results$x)
mean(sim_results$ci_lower)
mean(sim_results$ci_up)

# calculate CI width
sim_results$width <- sim_results$ci_upper - sim_results$ci_lower
mean(sim_results$width)
# plot CI width
p_sim <- ggplot(sim_results, aes(x = width)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "grey80") +
  scale_x_continuous(name = "95% confidence interval width") +
  scale_y_continuous(name = "Count", expand = c(0.01,0.01)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave("plots/sim_results.png", plot = p_sim, dpi = 300, width = 5, height = 3.5, bg = "white")  
