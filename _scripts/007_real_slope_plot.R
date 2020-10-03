# What's the Half-Life of Economic Growth?
# Script 7: Real Slope Plot

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


# Load packages

library(tidyverse)
library(magrittr)
library(jbmisc)     # https://github.com/jackobailey/jbmisc
library(brms)
library(here)


# Load model

m1 <- readRDS(here("_output", "m1.rds"))


# Estimate median half-life

half_life <-
  log(2) %>% 
  divide_by(
    posterior_samples(
      m1,
      pars = "b_lambda"
    ) %>% 
      pluck(1)
  ) %>% 
  median() %>% 
  round(2)



# 2. Create Triptych Plot -------------------------------------------------

# We're going to create a triptych plot that shows how the slope parameter
# in our fitted model changes as a function of time. First, we must compute
# the predicted probability of voting for the incumbent party at different
# values of GDP and time. We'll do this by using the conditional_effects()
# function from the brms package.

probs <-
  conditional_effects(
    m1,
    effects = "gdp",
    conditions =
      tibble(
        time = c(0, half_life, 5),
        cond__ = paste("Time Interval (Years) =", c(0, half_life, 5))
      ),
    re_formula = NA
  )


# Next, we'll use the values to create our plot.

real_slope_plot <-
  probs$gdp %>% 
  mutate(
    cond__ =
      cond__ %>% 
      ordered(labels = paste("Time Interval (Years) = ", c(0, half_life, 5)))
  ) %>% 
  ggplot(
    aes(
      x = gdp,
      y = estimate__,
      ymin = estimate__ - 1.96*se__,
      ymax = estimate__ + 1.96*se__
    )
  ) +
  facet_wrap(~cond__, scales = "free_x") +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    colour = bailey_colours("grey6")
  ) +
  geom_ribbon(alpha = 0.1) +
  geom_line(lineend = "round") +
  scale_x_continuous(
    breaks = seq(-5, 15, by = 5),
    labels = c("-5%", "0%", "+5%", "+10%", "+15%")) +
  scale_y_continuous(
    breaks = seq(0, .5, by = .1),
    labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .5)) +
  labs(
    x = "GDP Change",
    y = "Prob. Voting for Incumbent"
  ) +
  theme_bailey()



# 3. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
