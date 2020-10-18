# What's the Half-Life of Economic Growth?
# Script 2: Estimate the Half-Life of Economic Growth

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


# Load predicted GDP data from "001_gdp.R"

gdp <- readRDS(here("_output", "pred_gdp.rds"))



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


# We'll convert the voting intention item from specific parties to "Incumbent"
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



# 3. Link predicted GDP data ----------------------------------------------

# Because we have daily estimates of change in the rate of GDP growth, it's
# relatively simple to link together our two data sources. But we don't want
# to just link in the data for the day that each respondent took the survey.
# Instead, we want to link them to a random day between the first predicted
# GDP data and the day that the CMS data were collected.

# First, we'll create a random "link_date" for each case in the data by
# sampling a single date at random between the date that the respondent
# took the survey and five years before.

cms <-
  cms %>% 
  mutate(
    link_date =
      map_dbl(.x = date,
              .f = function(x){
                from <- x %m-% years(5)
                to <- x
                rdm <- seq(from, to, by = "day")
                smp <- sample(rdm, size = 1)
                smp
              }
      ) %>% 
      as.Date(origin = "1970-01-01")
  )


# Next, we'll compute the time interval between when they took the survey
# and the link_date that we just allocated them to.

cms <- 
  cms %>% 
  mutate(
    time = -(interval(date, link_date)/years(1))
  )


# Now, we'll merge in the predicted GDP data for the day of the survey
# from "001_gdp.R". This is a little tricky, because left_join() doesn't
# behave well when linking on dates, so we'll convert the dates in cms
# and gdp to character vectors to make it work.

cms <- 
  cms %>% 
  mutate(date = as.character(date))

gdp <- 
  gdp %>% 
  mutate(date = as.character(date))

cms <-
  cms %>% 
  left_join(gdp, by = "date") %>% 
  rename(
    level0_est = level_est
  )


# And now we'll merge in the predicted GDP data from "001_gdp.R" based on
# the random "link_date" we created.

cms <-
  cms %>% 
  mutate(link_date = as.character(link_date)) %>% 
  left_join(gdp, by = c("link_date" = "date"))


# Finally, we'll calculate the GDP growth between the date that the survey
# took place and the randomly-chosen link date.

cms <-
  cms %>%
  mutate(
    gdp = ((level0_est - level_est)/level_est)*100
  ) %>%
  select(
    -level0_est,
    -level_est
  )



# 4. Fit model ------------------------------------------------------------

# We're going to fit a really simple model, but with a twist. At its heart
# the model is just a conventional economic voting model. But unlike the
# economic voting models that we're used to, we also fit a non-linear model
# to the slope parameter "betaT" that captures the economic voting effect.
# This non-linear model allows the slope parameter to show exponential
# decay in the effect of GDP on incumbent voting in line with the amount of
# time that has passed between the current and the referent date.

# First, let's fit the model to the data. (Note that this model is complex
# and will likely take several hours to fit on even a high-end computer).

m1 <-
  brm(
    formula = 
      bf(
        inc | weights(w8) ~ pars + betaT*gdp,
        pars ~ 1 + year + office*leader + (1 | survey),
        nlf(betaT ~ beta0*exp(-lambda*time)),
        beta0 + lambda ~ 1,
        nl = TRUE,
        decomp = "QR"
      ),
    family = bernoulli(link = "logit"),
    prior =
      prior(normal(0, 1.5), coef = "Intercept", nlpar = "pars") +
      prior(normal(0, 0.5), class = "b", nlpar = "pars") +
      prior(exponential(5), class = "sd", nlpar = "pars") +
      prior(normal(0, 0.5), nlpar = "beta0") +
      prior(normal(0, 0.5), nlpar = "lambda"),
    data = cms,
    iter = 2000,
    refresh = 5,
    chains = 4,
    cores = 4,
    seed = 666,
    control = 
      list(adapt_delta = .99,
           max_treedepth = 15),
    file = here("_output", "m1")
  )


# Now that the model has finished fitting, let's check that there aren't
# any divergent transitions or other issues that might suggest that the
# inferences we make from the model are suspect. No errors here, so looks
# like we're good to go.

check_hmc_diagnostics(m1$fit)



# 5. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "002_cms.txt"))


# One last thing...

thanks()