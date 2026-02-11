# power simulation

# a clustered quasi-experimental comparison (not cohen's h commonly used for individual-randomized designs)
# WHO cluster trial guidelines

# baseline carriage prevalence = 0.30
# RR (vaccinated vs unvaccinated) = 0.70  (example)
# ICC = 0.02
# children per island = 500
# islands = 4 (2 vaccinated, 2 unvaccinated)

library(lme4)
library(dplyr)

# PARAMETERS
baseline <- 0.30*0.5       # VT in carriage prevalence in unvaccinated islands
RR <- 0.50             # effect of PCV13, RR = 1 = no effect; RR = 0.1 = 90& reduction
p_vax <- baseline * RR # carriage in vaccinated islands
ICC <- 0.022
clusters <- 4          # islands
n_per_cluster <- 450   # children per island
nsim <- 2000           # simulation iterations

# CALCULATE RANDOM EFFECT SD FROM ICC
sigma_cluster <- sqrt(ICC * (pi^2 / 3) / (1 - ICC))

simulate_study <- function() {
  
  # cluster IDs
  island <- rep(1:clusters, each = n_per_cluster)
  
  # vaccination policy by island (2 vs 2)
  policy <- rep(c(0, 0, 1, 1), each = n_per_cluster)
  
  # true logit probabilities
  eta <- qlogis(ifelse(policy == 1, p_vax, baseline))
  
  # random effect
  u <- rnorm(clusters, 0, sigma_cluster)
  eta_cluster <- eta + u[island]
  
  # generate outcomes
  y <- rbinom(length(eta_cluster), 1, plogis(eta_cluster))
  
  # fit modified Poisson (RR)
  model <- glm(y ~ policy, family = poisson(link="log"))
  
  robust <- sqrt(diag(sandwich::vcovHC(model, type = "HC0")))
  z <- coef(model) / robust
  pval <- 2 * pnorm(-abs(z))
  
  return(pval[2] < 0.05)  # TRUE if significant
}

# RUN SIMULATION
set.seed(1)
power <- mean(replicate(nsim, simulate_study()))

power

# Power for the island-policy comparison was estimated using a cluster-adjusted two-proportion power calculation.
# The design effect was calculated as
# DE = 1 + (m − 1) × ICC,
# with m = 500 children per island and ICC = 0.02.
# # With a baseline carriage of 30%, four islands, and 500 children per island, the effective sample size was approximately 980. Under these assumptions, the detectable effect size with 80% power corresponds to an RR of approximately 0.45. Smaller effects (e.g., RR 0.70–0.90) have substantially lower power due to the limited number of clusters.

