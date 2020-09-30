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

library(kableExtra)
library(tidyverse)
library(jbmisc) # https://github.com/jackobailey/jbmisc
library(brms)
library(here)


# Load half-life model

m1 <- readRDS(here("_output", "m1.rds"))



# 2.  Create table --------------------------------------------------------

# Get each draw from the posterior distribution

draws <- posterior_samples(m1)


# Get regression coefficients

draws <-
  draws %>% 
  select(
    `\\textsf{Intercept}` = b_pars_Intercept,
    `\\textsf{GDP (t = 0)}` = b_beta0_Intercept,
    `\\textsf{Decay Constant}` = b_lambda_Intercept,
    `\\textsf{Time Tracker}` = b_pars_track,
    `\\textsf{Gordon Brown (vs. Tony Blair)}` = b_pars_leaderGordonBrown,
    `\\textsf{David Cameron (vs. Tony Blair)}` = b_pars_leaderDavidCameron,
    `\\textsf{Gordon Brown $\\times$ Time}` = `b_pars_track:leaderGordonBrown`,
    `\\textsf{David Cameron $\\times$ Time}` = `b_pars_track:leaderDavidCameron`
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
            "\\textsf{GDP (t = 0)}",
            "\\textsf{Decay Constant}",
            "\\textsf{Time Tracker}",
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
    caption = "Posterior parameter estimates from the half-life model predicting incumbent voting intention, measured in log odds. Data here come from the BES Continuous Monitoring Survey, 2004--2014."
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
      format(nrow(m1$data), big.mark = ","),
      "$}\\\\\\\\\n",
      "\\\\textsf{N (Survey)} & ",
      "\\\\multicolumn{4}{r}{$",
      length(unique(m1$data$survey)),
      "$}\\\\\\\\\n",
      "\\\\bottomrule\\\n"
    )
  )


# Print table

cat(table)



# 3. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
