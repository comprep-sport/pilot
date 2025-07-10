# Script to for further analysis of the type of values extracted for piloting
# the project:
# "Computational Reproducibility in Sports Science"
# date: 07/2025
# author: Simon Nolte

# We have had internal discussion on which values should be primarily used for
# the recalculation (i.e., the determination procedure for the number to
# reproduce). Part of this discussion are the considerations, how often and
# how precise different values are reported. We use the PC determination
# piloting data to inform some of these consideration.

# read data
d <- read.csv("data/pilot_pc.csv")

# number of PCs
n <- nrow(d)

# mark missing p-values
d$p_value[d$p_value == ""] <- NA
d$has_pvalue <- ifelse(is.na(d$p_value), FALSE, TRUE)
d[,12:18][is.na(d[,12:18])] <- FALSE

# number of PC with a p-value
pn <- sum(d$has_pvalue)
pn
#> 88
# proportion
pn / n
#> 0.58

# number of p-values with inequality
ine_p <- sum(grepl("<", d$p_value))
ine_p
#> 43
# proportion of p-values with inequality among all p-values
ine_p / pn
#> 0.49

# types of p-values with inequality
table(d$p_value[grepl("<", d$p_value)])

# number of significant p-values among exact values:
ps <- as.numeric(d$p_value[!grepl("<", d$p_value)])
n_sig <- sum(ps < 0.05, na.rm = TRUE)
n_sig
#> 41

# since all p-values with inequalities were significant, we can add them for the
# total number of significant p-values
n_sig_total <- n_sig + ine_p 
n_sig_total
#> 84

# proportion of significant values
n_sig_total / sum(d$has_pvalue)
#> 0.95

# proportion of other reported values
# test statistics
sum(d$has_statistic) / n
#> 0.13

# standardized ES
sum(d$has_es_st) / n
#> 0.38

# confidence intervals (of standardized/unstandardized effect sizes)
sum(d$has_ci) / n
#> 0.36

# proportion of PC that only provide a p-value
sum(d$has_pvalue & !(d$has_ci | d$has_es | d$has_es_st | d$has_sd | d$has_se | d$has_statistic)) / n
#> 0.03
