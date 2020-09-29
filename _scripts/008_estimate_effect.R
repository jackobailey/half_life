# What's the Half-Life of Economic Growth?
# Script 8: Moving Economic Voting Effects

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
library(lubridate)
library(jbmisc)     # https://github.com/jackobailey/jbmisc
library(brms)
library(here)


# Load predicted GDP data from "001_gdp.R"

gdp <- readRDS(here("_output", "pred_gdp.rds"))


# Load model

m1 <- readRDS(here("_output", "m1.rds"))



# 2. Compute GDP change data ----------------------------------------------

# We're going to make a plot with two panels that shows the estimated effect
# of the economic vote at two points in time, at the peak in GDP just before
# the Global Financial Crisis and at its floor. To do so, we need to use the
# daily GDP data that we estimated to work out the change in GDP between each
# reference point and each day over the past five years. These two dates are
# 2008-01-02 and 2009-07-01.

ceiling <-
  gdp %>% 
  filter(
    date <= "2008-01-02",
    date >= as_date("2008-01-02") - years(5)
  ) %>% 
  mutate(
    gdp = level_est[n()] - level_est,
    time = n() - row_number()
  )

floor <- 
  gdp %>% 
  filter(
    date <= "2009-07-01",
    date >= as_date("2009-07-01") - years(5)
  ) %>% 
  mutate(
    gdp = level_est[n()] - level_est,
    time = n() - row_number()
  )


# So that we can use these to make predictions from our models, we also have
# to include an indicator that says who the Prime Minister was and a tracking
# variable that counts how long they'd been in office. We'll use the actual
# values here for the sake of accuracy.

ceiling <- 
  ceiling %>% 
  mutate(
    leader =
      case_when(
        date < "2007-06-27" ~ "Tony Blair",
        date >= "2007-06-27" & date < "2010-05-11" ~ "Gordon Brown",
        date >= "2010-05-11" ~ "David Cameron"
      ) %>% 
      factor(levels = c("Tony Blair", "Gordon Brown", "David Cameron")),
    track = interval(as_date("2004-04-08"), as_date(date))/years(1)
  )

floor <- 
  floor %>% 
  mutate(
    leader =
      case_when(
        date < "2007-06-27" ~ "Tony Blair",
        date >= "2007-06-27" & date < "2010-05-11" ~ "Gordon Brown",
        date >= "2010-05-11" ~ "David Cameron"
      ) %>% 
      factor(levels = c("Tony Blair", "Gordon Brown", "David Cameron")),
    track = interval(as_date("2004-04-08"), as_date(date))/years(1)
  )



# 3. Estimate economic voting effect --------------------------------------

# To estimate the economic voting effect in probability terms we need to do
# two things. First, we need to have our model produce estimates of voting
# for the incumbent using our ceiling and floor data. Second, we have to
# have it produce similar estimates in the counterfactual world where GDP
# growth is always 0 and take the former from the latter. We'll do that
# with the ceiling data first and then move on to the floor data.

ceiling_zero <- 
  add_fitted_draws(
    model = m1,
    newdata =
      ceiling %>% 
      mutate(
        gdp = 0
      ),
    re_formula = NA
  )

ceiling_est <- 
  add_fitted_draws(
    model = m1,
    newdata = ceiling,
    re_formula = NA
  ) %>% 
  ungroup() %>% 
  mutate(
    .value = .value - ceiling_zero$.value
  )



# Next we'll do the same for the floor data.

floor_zero <- 
  add_fitted_draws(
    model = m1,
    newdata =
      floor %>% 
      mutate(
        gdp = 0
      ),
    re_formula = NA
  )

floor_est <- 
  add_fitted_draws(
    model = m1,
    newdata = floor,
    re_formula = NA
  ) %>% 
  ungroup() %>% 
  mutate(
    .value = .value - floor_zero$.value
  )



# 4. Create plot ----------------------------------------------------------

# Now it's time to create our plots. Again, we'll start with the ceiling
# estimates first. This is similar to the decay plot we made, but rather
# than have the time past between the current date and the reference date
# it uses the actual dates and instead of plotting the economic vote it
# plots the probability of voting for the incumbent.

ceiling_est %>% 
  ggplot(
    aes(
      x = time,
      y = .value
    )
  ) +
  stat_lineribbon(
    .width = c(.95, .8, .5),
    colour = NA,
    fill = bailey_colours("grey"),
    show.legend = F,
    alpha = .1
  ) +
  stat_lineribbon(
    .width = .5,
    colour = bailey_colours("black"),
    fill = NA,
    show.legend = F,
    size = 0.5
  ) +
  scale_x_continuous(
    limits = c(0, 75)
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  coord_cartesian(
    ylim = c(0, 0.045)
  ) +
  labs(
    y = "Economic Voting Effect",
    x = "Time Interval (Years)"
  ) +
  theme_bailey() +
  theme(
    legend.title = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.y.right = element_text(margin = margin(r = 0)),
    axis.line.y.right = element_blank()
  )
