library(lme4)
library(DescTools)
library(dplyr)

# default is beta(10, 10)
sim <- function(n = 50, a = 10, b = 10, pooled = "partial") {
  d <- data.frame(
    id = seq_len(n)
  )
  
  # use a negative binomial distribution with n = 3 and p = 0.3 to simulate number
  # of pc per study
  d$pcn <- rnbinom(n, 3, 0.3)
  
  # use a beta distribution with to simulate the proportion per study
  d$prop <- rbeta(n, a, b)
  
  # drop studies with zero pcs 
  d <- d[d$pcn != 0, ]
  
  # create one row per pc
  d_long <- d[rep(1:nrow(d), d$pcn), ]
  
  # simulate success for each pc
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


# run 100 simulations
sim_results <- purrr::map_dfr(1:100, ~ sim(pooled = "partial"))
mean(sim_results$x)
mean(sim_results$ci_lower)
mean(sim_results$ci_upper)
# result from 100 simulations:
# 0.500 [0.436, 0.563]

