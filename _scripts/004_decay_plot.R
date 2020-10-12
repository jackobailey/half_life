# What's the Half-Life of Economic Growth?
# Script 4: Decay Constant Plot

# Jack Bailey
# The University of Manchester
# jack.bailey@manchester.ac.uk



# 1. Housekeeping ---------------------------------------------------------

# The following code requires that you have first opened the associated
# project file. If not, click the button on the top right and open it
# ("Open Project...").


# Load packages

library(tidyverse)
library(ggpubr)
library(jbmisc)
library(here)



# 2. Create Triptych Plot -------------------------------------------------

# We're going to create a triptych plot that shows how different values on
# for the decay constant lambda affects the rate at which a given quantity
# decays over time. We'll simulate these so that we know all the important
# parameter values. First, we'll create a function that simulates half-life
# data for us so that we can repeat it as we please.

halflife <- function(time = 0:10, beta0 = 1, lambda = 1){
  
  beta0*exp(-lambda*time)
  
}


# Now we'll set the three values for our decay constants

decay <- c(0.25, 0.5, 0.75)


# Next, we'll use the function to simulate some data.

dta <-
  tibble(
    time = rep(seq(0, 5, by = .1), length(decay)),
    lambda = rep(decay, each = 51),
    label = paste0("λ = ", lambda) %>% ordered()
  ) %>% 
  group_by(lambda) %>% 
  mutate(value = halflife(time = time, lambda = lambda))


# Then, we'll create an annotation data set for the lines and text we want
# to plot on each facet.

labs_h <-
  tibble(
    x = 0,
    xend = log(2)/decay,
    y = .5,
    yend = .5,
    label = paste0("λ = ", decay)
  )

labs_v <-
  tibble(
    x = log(2)/decay,
    xend = log(2)/decay,
    y = 0,
    yend = .5,
    label = paste0("λ = ", decay)
  )


# Now, we'll plot the data

decay_plot <- 
  dta %>% 
  ggplot(
    aes(
      y = value,
      x = time
    )
  ) +
  facet_wrap(~ label, nrow = 1) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0β", "½β", "1β")) +
  scale_x_continuous(breaks = c(0, 5)) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  labs(
    y = "Parameter Value",
    x = "Economic Time Frame (Years)"
  ) +
  geom_segment(
    data = labs_h,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    colour = bailey_colours("white"),
    lineend = "round"
  ) +
  geom_segment(
    data = labs_h,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    colour = bailey_colours("red"),
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_segment(
    data = labs_v,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    colour = bailey_colours("red"),
    linetype = "dotted",
    lineend = "round"
  ) +
  geom_segment(
    data = labs_v,
    aes(
      x = x,
      xend = xend,
      y = -.05,
      yend = -.08
    ),
    colour = bailey_colours("black"),
    lineend = "round"
  ) +
  geom_text(
    data = labs_v,
    aes(
      x = x,
      y = y-.12,
      label = paste("t½ =", round(xend, 1))
    ),
    colour = bailey_colours("black"),
    family = "Cabin",
    size = 2.3
  ) +
  geom_line(lineend = "round") +
  theme_bailey() +
  theme(
    axis.title = element_blank(),
    axis.ticks.y = element_line(lineend = "round"),
    plot.margin = unit(c(.01,0.02,0,0), "null")
    )



# 4. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
