# What's the Half-Life of Economic Growth?
# Script 1: Estimate Daily GDP

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

library(tidyverse)
library(lubridate)
library(tidybayes)
library(magrittr)
library(jbmisc)     # https://github.com/jackobailey/jbmisc
library(brms)
library(here)


# Load data

gdp <- read_csv(here("_data", "gdp.csv"))



# 2. Transform data -------------------------------------------------------

# The GDP data are very simple. Let's take a look.

head(gdp)


# As you can see, there are only two variables: date and gdp. The former
# corresponds to the month that the data are from and the latter the level
# of GDP at that point in time. Though the date variable implies that any
# growth occurred at the start of the month, this is not the case. Instead,
# it occurred over the course of the month. Let's take care of that first
# by increasing each date so that it is the *last* -- not the *first* --
# day of the month.

gdp <-
  gdp %>% 
  mutate(
    date =
      date %>% 
      ceiling_date() %m-%
      days(1)
  )


# The model we're going to use needs a continuous measure of time. To
# make this possible, we'll create a continuous variable that tracks
# the time passed in days where 0 is 31st December 1996 -- the first
# observation in our data.

gdp <-
  gdp %>% 
  mutate(
    time = 
      interval(
        start = "1996-12-31",
        end = date
      )/days(1)
  )



# 3. Fit model ------------------------------------------------------------

# Now that we have the data sorted, we can fit our model. Despite using
# a generalised additive model, which is relatively uncommon in political
# science research, the model is straightforward enough. It simply fits a
# smoothed curve to the data.

gdp_model <-
  brm(
    formula = gdp ~ 1 + s(time, k = 100),
    family = gaussian(),
    prior = 
      prior(lognormal(3.6, 0.5), class = "Intercept") +
      prior(normal(0, 5), class = "sds") +
      prior(normal(0, 5), class = "sigma"),
    data = gdp,
    iter = 2000,
    refresh = 5,
    chains = 4,
    cores = 4,
    seed = 666,
    control = list(max_treedepth = 15),
    file = here("_output", "gdp_model")
  )


# Now that the model has fit, let's check how well it fits the GDP data.
# As you can see, it's a pretty good fit.

plot(conditional_effects(gdp_model, effects = "time"), points = T)


# The convergence statistics imply that there are no issues or errors
# that we need to contend with.

rstan::check_hmc_diagnostics(gdp_model$fit)


# Next, we want to compute the level of GDP for each day that the data
# cover. To do this, we'll get posterior predictions from our model by
# using the add_fitted_draws() function from the tidybayes package.

gdp_change <-
  add_fitted_draws(
    model = gdp_model,
    newdata = tibble(time = seq(0, max(gdp$time), by = 1))
  ) %>% 
  ungroup()


# Next, we'll summarise the data so that we have a point estimate for each
# time point.

gdp_change <-
  gdp_change %>% 
  filter(time != 0) %>% 
  group_by(time) %>% 
  summarise(
    level_est = median(.value),
    .groups = "drop"
  )


# Now, we'll add the dates back in to make things easy to match in the next
# script where we model individual-level voting behaviour.

gdp_change <-
  gdp_change %>% 
  mutate(
    date =
      time %>% 
      as.Date(origin = "1996-12-31")
  ) %>% 
  relocate(date, .after = 1) %>% 
  select(-time)
  

# Finally, we'll save the data to disk so that we can load it quickly when
# modelling individual-level voting behaviour.

saveRDS(gdp_change, here("_output", "pred_gdp.rds"))



# 4. Thank you for replicating! -------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
