# What's the Half-Life of Economic Growth?
# Supplement 2: Standard Economic Vote Table

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

library(kableExtra)
library(tidyverse)
library(jbmisc) # https://github.com/jackobailey/jbmisc
library(brms)
library(here)


# Load conventional economic voting model

m2 <- readRDS(here("_output", "m2.rds"))



# 2.  Create table --------------------------------------------------------

# Get each draw from the posterior distribution

draws <- posterior_samples(m2)


# Get regression coefficients

draws <-
  draws %>% 
  select(
    `\\textsf{Intercept}` = b_Intercept,
    `\\textsf{Year-on-Year GDP Change}` = b_gdp,
    `\\textsf{Years Passed}` = b_year,
    `\\textsf{Time in Office}` = b_office,
    `\\textsf{Gordon Brown (vs. Tony Blair)}` = b_leaderGordonBrown,
    `\\textsf{David Cameron (vs. Tony Blair)}` = b_leaderDavidCameron,
    `\\textsf{Gordon Brown $\\times$ Time}` = `b_office:leaderGordonBrown`,
    `\\textsf{David Cameron $\\times$ Time}` = `b_office:leaderDavidCameron`
  )


# Pivot table to long-format then compute summary statistics

table <- 
  draws %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(
    name =
      name %>% 
      factor(
        levels =
          c(
            "\\textsf{Intercept}",
            "\\textsf{Year-on-Year GDP Change}",
            "\\textsf{Years Passed}",
            "\\textsf{Time in Office}",
            "\\textsf{Gordon Brown (vs. Tony Blair)}",
            "\\textsf{David Cameron (vs. Tony Blair)}",
            "\\textsf{Gordon Brown $\\times$ Time}",
            "\\textsf{David Cameron $\\times$ Time}"
          )
      )
  ) %>% 
  group_by(name) %>% 
  summarise(
    `\\multicolumn{1}{r}{\\textsf{Median}}` = format(round(median(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{Error}}` = format(round(sd(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = format(round(quantile(value, probs = 0.025), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = format(round(quantile(value, probs = 0.975), 2), nsmall = 2),
    .groups = "drop"
  ) %>% 
  rename(
    " " = name
  )


# Create latex table

table <-
  table %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l ", rep("D{.}{.}{-1} ", 4)),
    linesep = "",
    caption = "Parameter estimates from my conventional economic voting model. Here, year-on-year GDP change data come from the ONS' monthly time series of UK GDP and individual-level voting intention data come from the BES Continuous Monitoring Survey, 2004--2014.",
    label = "tablea1"
  ) %>% 
  kable_styling(
    position = "center"
  )


# Add number of individuals and groups

table <- 
  table %>% 
  str_replace(
    "\\\\bottomrule\\\n",
    paste0(
      "\\\\midrule\\\n",
      "\\\\textsf{N (Individuals)} & ",
      "\\\\multicolumn{4}{r}{$",
      format(nrow(m2$data), big.mark = ","),
      "$}\\\\\\\\\n",
      "\\\\textsf{N (Survey)} & ",
      "\\\\multicolumn{4}{r}{$",
      length(unique(m2$data$survey)),
      "$}\\\\\\\\\n",
      "\\\\bottomrule\\\n"
    )
  )


# Print table
# Save table to disk

sink(file = here("_paper", "_assets", "tablea1.tex"))
cat(table)
sink()



# 3. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
