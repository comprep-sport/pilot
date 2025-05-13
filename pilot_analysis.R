# Script to analyze the pilot data of the project
# "Computational Reproducibility in Sports Science"
# date: 05/2025
# author: Simon Nolte

# load packages
library(MASS)    # function fitting
library(ggplot2) # visualization
library(dplyr)   # data wrangling

# read pilot data
inc <- read.csv2("pilot_inclusion.csv")
pc <- read.csv2("pilot_pc.csv")

# number of articles screened
nrow(inc)
#> 38

# number of articles included
sum(inc$inclusion)
#> 31

# exclusion reasons
table(inc$reason)
#> no original research: 4
#> qualitative research: 3

# total number of PCs
nrow(pc)
#> 152

# number of articles with at least one pc
length(unique(pc$id))
#> 25

# count number of primary claims
t <- table(factor(pc$id, levels = inc$id[inc$inclusion]))
# extract counts
count <- as.numeric(t)
count

# get descriptive statistics
quantile(count)
#> 4 [2, 7.5 IQR]

# plot histogram of primary claims per study
ct <- data.frame(count = count)
p <- ggplot(ct, aes(x = count)) +
  geom_histogram(binwidth = 1, color = "black", fill = "grey80") +
  scale_x_continuous(name = "Number of Primary Claims per Study") +
  scale_y_continuous(name = "Count", expand = c(0.01,0.01)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave("pc_count.png", p, dpi = 300, width = 5, height = 3.5, bg = "white")

# fit a negative binomial distribution on the raw count data
fit <- fitdistr(count, densfun = "negative binomial")$estimate
fit
#> size = 1.56
#> mu = 4.9
# convert mu to more common scale p:
fit[["size"]] / (fit[["size"]]+fit[["mu"]])
#> prob = 0.24

# convert to relative counts
pmf <- data.frame(count = rnbinom(100000, size = 1.5, prob = 0.25)) |>
  count(count) |>
  mutate(prob = n / sum(n))

# overlay the count data by the fitted negBinomial
p2 <- ggplot(ct, aes(x = count)) +
  geom_histogram(binwidth = 1, color = "black", fill = "grey80") +
  geom_line(data = pmf, aes(y = prob * nrow(ct)), 
            color = "darkred", size = 1.2) +
  scale_x_continuous(
    name = "Number of Primary Claims per Study",
    limits = c(-1,20)
  ) +
  scale_y_continuous(name = "Count", expand = c(0.01,0.01)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave("pc_count_overlay.png", p2, dpi = 300, width = 5, height = 3.5, bg = "white")