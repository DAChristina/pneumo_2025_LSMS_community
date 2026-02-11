# test power 2

# analytic_sample_size_clustered.R
# Requires: base R + stats
# Functions to compute required sample sizes/cluster sizes / clusters for cluster designs.

moe <- function(p, n, alpha = 0.05){
  z <- qnorm(1 - alpha/2)
  se <- sqrt(p*(1-p)/n)
  return(z * se)
}

# example:
p <- 0.3; n <- 450
moe(p,n)         # ~0.042 -> 4.2%
moe(0.5, 450)    # worst-case ~0.044 -> 4.4%


design_effect <- function(m, ICC) 1 + (m - 1) * ICC

# example:
m <- 5; ICC <- 0.02
DE <- design_effect(m, ICC)
n_eff <- 450 / DE
moe(p, n_eff)    # margin after clustering adjustment

required_n <- function(p, d, alpha = 0.05){
  z <- qnorm(1 - alpha/2)
  n <- (z^2 * p * (1-p)) / (d^2)
  ceiling(n)
}
# example: p=0.25, d=0.04
required_n(0.25, 0.04)   # returns 484 -> so 450 is slightly conservative
