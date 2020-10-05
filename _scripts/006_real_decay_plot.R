# What's the Half-Life of Economic Growth?
# Script 6: Real Decay Plot

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



# 2. Create plot ----------------------------------------------------------

# We're going to create a single-panel plot that captures how the economic
# voting effect diminishes as the GDP reference point stretches further and
# further into the past. First, we'll extract beta0 and lambda from m1, and
# then we'll compute the half-life for later use.

pars <-
  m1 %>% 
  posterior_samples(pars = c("beta0_Intercept", "lambda_Intercept")) %>% 
  rename(
    beta0 = b_beta0_Intercept,
    lambda = b_lambda_Intercept
  ) %>% 
  mutate(halflife = log(2)/lambda) %>% 
  tibble()


# Next, we'll sort out our plot data.

plot_dta <-
  add_fitted_draws(
    model = m1,
    newdata = 
      tibble(
        year = NA,
        office = NA,
        time = seq(0, 5, by = .1),
        gdp = NA,
        leader = NA),
    re_formula = NA,
    nlpar = "betaT"
  )


# Finally, we'll create our plot.

real_decay_plot <- 
  plot_dta %>% 
  ggplot(
    aes(
      x = time,
      y = .value
    )
  ) +
  geom_segment(
    data = pars,
    aes(
      x = 0,
      xend = median(halflife),
      y = median(beta0)/2,
      yend = median(beta0)/2
    ),
    colour = bailey_colours("red"),
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_segment(
    data = pars,
    aes(
      x = median(halflife),
      xend = median(halflife),
      y = 0,
      yend = median(beta0)/2
    ),
    colour = bailey_colours("red"),
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_segment(
    data = pars,
    aes(
      x = median(halflife),
      xend = median(halflife),
      y = -.0023,
      yend = -.0029
    ),
    colour = bailey_colours("black"),
    lineend = "round",
    size = .45
  ) +
  annotate(
    "text",
    x = median(pars$halflife),
    y = -.004,
    label = paste("t½ =", round(median(pars$halflife), 2)),
    colour = bailey_colours("black"),
    family = "Cabin",
    size = 2
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
  scale_y_continuous(
    breaks = c(0, median(pars$beta0)/2, median(pars$beta0), median(pars$beta0)*2),
    labels = c(0, round(median(pars$beta0) * c(.5, 1, 2), 2)),
    sec.axis = 
      dup_axis(
        trans = ~.,
        labels = c("0", "½β", "β", "2β")
      )
  ) +
  scale_x_continuous(
    limits = c(0, 5),
    breaks = c(0, 5)
  ) +
  coord_cartesian(
    ylim = c(0, 0.045),
    clip = "off"
  ) +
  labs(
    y = "Economic Voting Effect (Log Odds)",
    x = "Time Interval (Years)"
  ) +
  theme_bailey() +
  theme(
    legend.title = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.y.right = element_text(margin = margin(r = 0)),
    axis.line.y.right = element_blank()
    )



# 3. Create PNG version ---------------------------------------------------

# As this plot is clearly the most important in the entire paper, and as
# this project is hosted on GitHub, it makes sense to render it as an image
# file so that I can call it remotely from other services and always get the
# most up-to-date version.

# First, we'll add a title to the plot

real_decay_plot <-
  real_decay_plot +
  labs(title = "Voter Myopia is Real")


# Then, we'll render the image

png(
  filename = here("_output", "real_decay_plot.png"),
  width = 4,
  height = 2.5,
  units = "in",
  res = 300
)
real_decay_plot
dev.off()


# 4. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
