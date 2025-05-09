library(MASS)

inc <- read.csv2("pilot_inclusion.csv")
pc <- read.csv2("pilot_pc.csv")

# number of articles screened
nrow(inc)
#> 38

# number of articles included
sum(inc$inclusion)
#> 31

# total number of PCs
nrow(pc)

# number of articles with at least one pc
unique(pc$id)

# count number of pcs
t <- table(factor(pc$id, levels = inc$id[inc$inclusion]))
# extract counts
count <- as.numeric(t)
count

# get descriptive statistics
quantile(count)
#> 4 [2, 7.5 IQR]

# plot histogram
hist(count, breaks = 15)
hist(rnbinom(100000, size = 1.5, mu = 5), breaks = 40)

# fit neg bin
# use raw count data plus zero counts
count

fitdistr(count, densfun = "negative binomial")$estimate
