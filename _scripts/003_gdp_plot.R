# What's the Half-Life of Economic Growth?
# Script 3: GDP Plot

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
library(lubridate)
library(jbmisc)     # https://github.com/jackobailey/jbmisc
library(ggpubr)
library(brms)
library(here)


# Load GDP data

gdp <- read_csv(here("_data", "gdp.csv"))


# Load fitted GDP model

gdp_model <- readRDS(here("_output", "gdp_model.rds"))



# 2. Create Triptych Plot -------------------------------------------------

# We're going to create a triptych plot that shows the original monthly GDP
# data from the ONS, the predicted output from our model, and the estimates
# that we are allocating to each case in the data. First, we'll create the
# first panel which shows the draw data. We'll plot only the data from 2000
# to 2020 for the sake of convenience.

panel1 <-
  gdp %>% 
  filter(date >= "2000-01-01", date <= "2020-01-01") %>% 
  ggplot(
    aes(
      x = date,
      y = gdp
    )
  ) +
  geom_point(
    size = 0.25,
    colour = bailey_colours("grey6")
  ) +
  scale_y_continuous(limits = c(70, 110), breaks = seq(70, 110, by = 10)) +
  coord_cartesian(xlim = c(as_date("2000-01-01"), as_date("2020-01-01"))) +
  labs(
    title = "Raw Monthly GDP",
    y = "GDP (July 2016 = 100)"
  ) +
  theme_bailey() +
  theme(
    plot.title = element_text(size = rel(1), hjust = .5),
    axis.ticks.y = element_line(lineend = "round"),
    plot.margin=unit(c(.01,0.035,0,0), "null"),
    axis.title.x = element_blank()
    )


# Second, we'll plot the output of our model in a similar style.

conds <- conditional_effects(gdp_model, resolution = 1000)

conds <-
  conds$time %>% 
  mutate(
    time = as_date(time, origin = "1996-12-31")
  ) %>% 
  filter(time >= "2000-01-01", time <= "2020-01-01")

panel2 <- 
  panel1 +
  geom_ribbon(
    data = conds,
    aes(
          x = time,
          y = estimate__,
          ymin = lower__,
          ymax = upper__
        ),
    colour = NA,
    fill = bailey_colours("grey6")) +
  geom_line(
    data = conds,
    aes(
      x = time,
      y = estimate__
    ),
    colour = bailey_colours("black"),
    lineend = "round") +
  labs(
    title = "Fitted Model Output"
  ) +
  theme_bailey() +
  theme(
    plot.title = element_text(size = rel(1), hjust = .5),
    axis.title = element_blank(),
    plot.margin = unit(c(.01,0.035,0,0.035), "null"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank()
    )


# Third, we'll create an explanatory panel that shows how we compute the
# GDP data that we allocate to each case in our data.

panel3 <-
  panel2 +
  labs(
    title = "Random GDP Change"
  ) +
  annotate(
    "point",
    x = conds[414, "time"],
    y = conds[414, "estimate__"],
    colour = bailey_colours("black"),
    size = 1.5
  ) +
  annotate(
    "point",
    x = conds[576, "time"],
    y = conds[576, "estimate__"],
    colour = bailey_colours("black"),
    size = 1.5
  ) +
  annotate(
    "point",
    x = conds[414, "time"],
    y = conds[414, "estimate__"],
    colour = bailey_colours("red"),
    size = 0.5
  ) +
  annotate(
    "point",
    x = conds[576, "time"],
    y = conds[576, "estimate__"],
    colour = bailey_colours("red"),
    size = 0.5
  ) +
  annotate(
    "segment",
    x = conds[414, "time"],
    xend = conds[576, "time"],
    y = conds[414, "estimate__"],
    yend = conds[414, "estimate__"],
    colour = bailey_colours("red"),
    lineend = "round",
    size = 0.4,
    linetype = "dotted"
  ) +
  annotate(
    "segment",
    x = conds[576, "time"],
    xend = conds[576, "time"],
    y = conds[414, "estimate__"],
    yend = conds[576, "estimate__"],
    colour = bailey_colours("red"),
    lineend = "round",
    size = 0.4,
    linetype = "dotted"
  ) +
  geom_segment(
    x = conds[414, "time"],
    xend = conds[576, "time"],
    y = conds[414, "estimate__"] - 1.5,
    yend = conds[414, "estimate__"] - 1.5,
    lineend = "round",
    size = 0.3,
    color = bailey_colours("red"),
    arrow = arrow(ends = "last", type = "closed", length = unit(1.5, "pt"))) +
  geom_segment(
    x = conds[576, "time"] + 350,
    xend = conds[576, "time"] + 350,
    y = conds[414, "estimate__"],
    yend = conds[576, "estimate__"],
    lineend = "round",
    size = 0.3,
    color = bailey_colours("red"),
    arrow = arrow(ends = "last", type = "closed", length = unit(1.5, "pt"))) +
  annotate(
    "text",
    x = conds[495, "time"],
    y = conds[414, "estimate__"] - 3.5,
    label = "italic(t)",
    parse = TRUE,
    size = 2.5,
    colour = bailey_colours("red"),
    family = "Cabin"
  ) +
  annotate(
    "text",
    x = conds[576, "time"] + 1400,
    y = conds[495, "estimate__"],
    label = "GDP\nChange",
    size = 2.5,
    colour = bailey_colours("red"),
    family = "Cabin"
  ) + 
  annotate(
    "text",
    label = "Survey\nDate",
    x = conds[576, "time"] - 1750,
    y = conds[576, "estimate__"] + 3.5,
    size = 2.5,
    color = "black",
    family = "Cabin") +
  geom_curve(data =
               tibble(
                 x = conds[576, "time"] - 1000,
                 xend = conds[576, "time"] - 100,
                 y = conds[576, "estimate__"] + 3.5,
                 yend = conds[576, "estimate__"] + 1
               ),
             aes(x = x, xend = xend, y = y, yend = yend),
             angle = 90,
             curvature = -0.25,
             lineend = "round",
             size = 0.4,
             arrow = grid::arrow(length = grid::unit(1.5, "pt"),
                                 type = "closed"),
             inherit.aes = FALSE,
             show.legend = FALSE) +
  annotate(
    "text",
    label = "Random\nDate",
    x = conds[414, "time"] - 750,
    y = conds[414, "estimate__"] - 6,
    size = 2.5,
    color = "black",
    family = "Cabin") +
  geom_curve(data =
               tibble(
                 x = conds[414, "time"] - 750,
                 xend = conds[414, "time"] - 200,
                 y = conds[414, "estimate__"] - 2.75,
                 yend = conds[414, "estimate__"] - 0.5
               ),
             aes(x = x, xend = xend, y = y, yend = yend),
             angle = 90,
             curvature = -0.25,
             lineend = "round",
             size = 0.4,
             arrow = grid::arrow(length = grid::unit(1.5, "pt"),
                                 type = "closed"),
             inherit.aes = FALSE,
             show.legend = FALSE) +
  theme(
    plot.title = element_text(size = rel(1), hjust = .5),
    axis.title = element_blank(),
    plot.margin=unit(c(.01,0.02,0,0.035), "null")
  )


# Now that we have our three panels, we can join them together into a single
# plot using ggarrange().

gdp_plot <- ggarrange(
  panel1, panel2, panel3,
  widths = c(1, .85, .85),
  nrow = 1)



# 4. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
