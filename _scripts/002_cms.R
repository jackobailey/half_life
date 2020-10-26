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


# Now, we'll create a tracking variable that counts up in months for our
# rolling intercept parameters.

cms <- 
  cms %>% 
  mutate(
    month =
      date %>% 
      floor_date("months") %>% 
      interval(min(., na.rm = T), .) %>% 
      divide_by(months(1)) %>% 
      add(1)
  )


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



# Next, we'll create a variable that tells us what date the government under
# which the respondent currently lives first came to power (1 May 1997 for
# the Labour Party, 11 May 2010 for the Coalition).

cms <- 
  cms %>% 
  mutate(
    term =
      ifelse(date < "2010-05-11", "1997-05-01", "2010-05-11") %>% 
      as_date()
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
# took the survey and the current government's first day in power.

cms <-
  cms %>% 
  mutate(
    link_date =
      map2_dbl(
        .x = date,
        .y = term,
              .f = function(x, y){
                rdm <- seq(y, x, by = "day")
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

# First, we'll put the data in list format for Stan.

stan_dta <- 
  list(
    N = nrow(cms),
    M = max(cms$month),
    vote = as_dummy(cms$inc, "Incumbent"),
    month = cms$month,
    gdp = cms$gdp,
    time = cms$time,
    w8 = cms$w8
  )


# Second, we'll compile the model.

model <- stan_model(file = here("_models", "m001_exponential.stan"))


# Third, we'll fit it to the data

m1 <- 
  sampling(
    object = model,
    data = stan_dta,
    iter = 2e3,
    refresh = 5,
    seed = 666,
    chains = 4,
    cores = 4
  )


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