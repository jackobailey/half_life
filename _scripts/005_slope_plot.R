# What's the Half-Life of Economic Growth?
# Script 5: Slope Plot

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

# We're going to create a triptych plot that shows how the slope parameter
# beta changes as a function of time given a fixed decay constant. Again,
# we'll simulate these so that we know all of the parameter values. First,
# we'll create a function that returns the slope of beta for given values
# of beta0, lambda, and time.

betaT <- function(time = 0, beta0 = 1, lambda = 1){
  
  beta0*exp(-lambda*time)
  
}


# Next, we'll simulate some data

dta <-
  tibble(
    gdp = rep(seq(-5, 5, by = 0.1), 3),
    time = rep(0:2, each = length(gdp)/3),
    label = rep(paste("Economic Time Frame (Years) =", 0:2), each = length(gdp)/3),
    vote = inv_logit(0 + betaT(time = time, beta0 = 0.2, lambda = 1)*gdp)
  )


# Now, we'll plot the data

slope_plot <- 
  dta %>% 
  ggplot(
    aes(
      y = vote,
      x = gdp
    )
  ) +
  facet_wrap(~ label, nrow = 2, ncol = 3) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(-5, 5, by = 2.5),
    labels = c("-5%", "-2.5%", "0%", "+2.5%", "+5%")
  ) +
  labs(
    y = "Prob. Voting for Incumbent",
    x = "GDP Change"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    colour = bailey_colours("grey6")
    ) +
  geom_line(lineend = "round") +
  theme_bailey() +
  theme(
    axis.ticks.y = element_line(lineend = "round"),
    plot.margin = unit(c(.01,0.02,0,0), "null")
  )



# 4. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
