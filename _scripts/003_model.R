# What's the Half-Life of Economic Growth?
# Script 3: Fit Exponential Decay Model

# Jack Bailey
# The University of Manchester
# jack.bailey@manchester.ac.uk



# 1. Housekeeping ---------------------------------------------------------

# The following code requires that you have first opened the associated
# project file. If not, click the button on the top right and open it
# ("Open Project...").

# Note also that the Bayesian methods I use below rely on having installed
# the probabilistic programming language Stan. If you have yet to install
# Stan, please see https://mc-stan.org/users/interfaces/


# Set random seed

set.seed(666)


# Load packages

library(jbmisc)     # https://github.com/jackobailey/jbmisc
library(rstan)
library(here)


# Load compiled individual-level data from the CMS

dta <- readRDS(here("_output", "dta.rds"))


# Load "to matrix"

to_mtrx <- readRDS(here("_output", "to_mtrx.rds"))


# Load predicted GDP data

gdp <- readRDS(here("_output", "pred_gdp.rds"))



# 1. Fit model ------------------------------------------------------------

# We're going to fit a model that captures the economic vote, but also lets
# it decay over time using an exponential decay process.

# First, Stan likes data to be in list format, so we'll sort that out.

stan_dta <- 
  list(
    N = nrow(dta),
    T = max(gdp$time),
    M = max(dta$month),
    C = max(dta$time),
    w8 = dta$w8,
    vote = as_dummy(dta$vote, "Incumbent"),
    month = dta$month,
    time = dta$time,
    from = dta$from,
    to = to_mtrx,
    gdp = gdp$level_est
  )


# Next, we'll compile the model.

model <- stan_model(file = here("_models", "m001_exponential.stan"))


# And now we'll fit the model to the data

m1 <- 
  sampling(
    object = model,
    data = stan_dta,
    seed = 666,
    iter = 2e3,
    init = 0,
    refresh = 1,
    chains = 2,
    cores = 2
  )


# Then save the model to disk to recall later


# Now that the model has finished fitting, let's check that there aren't
# any divergent transitions or other issues that might suggest that the
# inferences we make from the model are suspect. No errors here, so looks
# like we're good to go.

check_hmc_diagnostics(m1$fit)



# 5. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "002_cms.txt"))


# One last thing...

thanks()