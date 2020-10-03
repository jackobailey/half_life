# What's the Half-Life of Economic Growth?
# Supplement 1: Standard Economic Vote

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
library(haven)
library(purrr)
library(rstan)
library(brms)
library(here)


# Load data from the British Election Study Continuous Monitoring Surveys

cms <- read_dta(here("_data", "cms.zip"))


# Load raw GDP data

gdp <- read_csv(here("_data", "gdp.csv"))



# 2. Transform data -------------------------------------------------------

# First, let's select the variables that we need from the BES CMS data. We
# are going to use the weights, the survey ID, the date that each respondent
# took their survey, and how each respondent said that they would vote.

cms <- 
  cms %>% 
  select(
    w8 = W8,
    survey = yrmon,
    date1 = respdate,
    date2 = date,
    vote = Q6
  )


# Next, we'll combine the two date variables into a single variable and
# remove the originals to keep things neat and tidy.

cms <-
  cms %>% 
  mutate(
    date = 
      ifelse(is.na(cms$date1) == T,
             cms$date2,
             cms$date1) %>% 
      as.Date(origin = "1970-01-01")
  ) %>% 
  select(-date1, -date2)


# We'll convert the voting intention item from specific parties t0  "Incumbent"
# or "Other" based on who was in power at what time. I've kept in those who
# said "Don't know", etc., as it's probable that some voters who support the
# incumbent will pick these options when things get bad so that they don't have
# to challenge their own party identification. Likewise, some opposition
# supporters will pick them when things are good. Keeping them in not only
# preserves statistical power, it also takes this into account.

cms <-
  cms %>% 
  mutate(
    vote =
      vote %>% 
      as_factor() %>% 
      mark_na(c("8", "9")) %>% 
      fct_recode(
        "Con" = "Conservative",
        "Lab" = "Labour",
        "Lib" = "Liberal Democrat",
        "Oth" = "Other Party"
      )
  ) %>% 
  mutate(
    inc = 
      case_when(
        # Recode those who support an incumbent party as "Incumbent"
        date < "2010-05-06" & vote == "Lab" ~ "Incumbent",
        date >= "2010-05-06" & vote %in% c("Con", "Lib") ~ "Incumbent",
        
        # Recode those who don't support an incumbent party as "Other"
        date < "2010-05-06" & !(vote == "Lab") ~ "Other",
        date >= "2010-05-06" & !(vote %in% c("Con", "Lib")) ~ "Other"
      ) %>% 
      factor(levels = c("Other", "Incumbent"))
  )


# Next, we'll create a factor variable that tracks who the current Prime
# Minister was at the time of the survey so that we can account for any
# leader-specific effects that might affect whether respondents support
# the incumbent or not.

cms <-
  cms %>% 
  mutate(
    leader =
      case_when(
        date < "2007-06-27" ~ "Tony Blair",
        date >= "2007-06-27" & date < "2010-05-11" ~ "Gordon Brown",
        date >= "2010-05-11" ~ "David Cameron"
      ) %>% 
      factor(levels = c("Tony Blair", "Gordon Brown", "David Cameron"))
  )


# We'll also create a new variable "office" that counts how long each
# Prime Minister has been in power.

cms <- 
  cms %>% 
  mutate(
    office =
      case_when(
        leader == "Tony Blair" ~ interval("1997-05-02", date)/years(1),
        leader == "Gordon Brown" ~ interval("2007-06-27", date)/years(1),
        leader == "David Cameron" ~ interval("2010-05-11", date)/years(1)
      )
  )


# We'll also create a new variable "year" that tracks the time that has
# passed since the first date of the survey in years.

cms <- 
  cms %>% 
  mutate(
    year = interval(min(as_date(cms$date), na.rm = T), as_date(date))/years(1)
  )


# Finally, we'll remove any missing data list-wise.

cms <- 
  cms %>% 
  na.omit()



# 3. Link raw GDP data ----------------------------------------------------

# We're going to link each respondent to a monthly estimate of year-on-year
# GDP change. To do so, we first need to compute year-on-year change in the
# raw data.

gdp <- 
  gdp %>% 
  mutate(d_gdp = ((gdp - lag(gdp, 12))/lag(gdp, 12)) * 100)


# We'll now link the data together. Note that I create a "link_date" that I
# use to link the data together. This is to reflect the fact that voters
# know only the year-on-year change in GDP from the *previous* month.

cms <- 
  cms %>% 
  mutate(
    link_date = 
      date %>%
      floor_date("month")
    %m-% months(1)
  ) %>% 
  inner_join(
    gdp,
    by = c("link_date" = "date")
  ) %>% 
  select(
    -link_date,
    -gdp
  ) %>% 
  rename(
    gdp = d_gdp
  )



# 4. Fit model ------------------------------------------------------------

# Fit model

m2 <-
  brm(
    formula = 
      bf(inc | weights(w8) ~ 1 + gdp + year + office*leader + (1 | survey),
         decomp = "QR"),
    family = bernoulli(link = "logit"),
    prior =
      prior(normal(0, 1.5), class = "Intercept") +
      prior(normal(0, 0.5), class = "b") +
      prior(exponential(5), class = "sd"),
    data = cms,
    iter = 2000,
    refresh = 5,
    chains = 4,
    cores = 4,
    seed = 666,
    file = here("_output", "m2")
  )


# Now that the model has finished fitting, let's check that there aren't
# any divergent transitions or other issues that might suggest that the
# inferences we make from the model are suspect. No errors here, so looks
# like we're good to go.

check_hmc_diagnostics(m2$fit)



# 5. Thanks for replicating! ----------------------------------------------

# Any questions, feel free to get in touch at jack.bailey@manchester.ac.uk.
