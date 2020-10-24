# What's the Half-Life of Economic Growth?
# Script 8: Real Slope Plot

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
library(tidybayes)
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
# values of GDP and time.

probs <- 
  add_fitted_draws(
    model = m1,
    newdata =
      tibble(
        gdp = rep(seq(min(m1$data$gdp), max(m1$data$gdp), length.out = 100), 3),
        time = rep(c(0, half_life, 5), each = length(gdp)/3),
        year = 0,
        office = 0,
        leader = "Tony Blair"
      ),
    re_formula = NA
  ) %>% 
  ungroup()


# Next, we'll use the values to create our plot.

real_slope_plot <-
  probs %>% 
  mutate(
    label = 
      paste0("Economic Time Frame (Years) = ", time) %>% 
      ordered(labels = paste("Economic Time Frame (Years) = ", c(0, half_life, 5)))
  ) %>% 
  ggplot(
    aes(
      x = gdp,
      y = .value
    )
  ) +
  facet_wrap(~label, scales = "free_x") +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    colour = bailey_colours("grey6")
  ) +
  stat_lineribbon(
    .width = c(.95, .8, .5),
    colour = NA,
    fill = bailey_colours("grey"),
    show.legend = F,
    alpha = .15
  ) +
  stat_lineribbon(
    .width = .5,
    colour = bailey_colours("black"),
    fill = NA,
    show.legend = F,
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(-5, 15, by = 5),
    labels = c("-5%", "0%", "+5%", "+10%", "+15%")) +
  scale_y_continuous(
    breaks = seq(0, .6, by = .2),
    labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .6)) +
  labs(
    x = "GDP Change",
    y = "Prob. Voting for Incumbent"
  ) +
  theme_bailey()



# 3. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "007_real_slope_plot.txt"))


# One last thing...

thanks()

