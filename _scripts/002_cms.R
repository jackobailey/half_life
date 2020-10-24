# What's the Half-Life of Economic Growth?
# Script 2: Compile CMS Data

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
library(here)


# Load data from the British Election Study Continuous Monitoring Surveys

dta <- read_dta(here("_data", "cms.zip"))



# 2. Transform data -------------------------------------------------------

# First, let's select the variables that we need from the BES CMS data. We
# are going to use the weights, the survey ID, the date that each respondent
# took their survey, and how each respondent said that they would vote.

dta <- 
  dta %>% 
  select(
    w8 = W8,
    survey = yrmon,
    date1 = respdate,
    date2 = date,
    vote = Q6
  )


# Next, we'll combine the two date variables into a single variable and
# remove the originals to keep things neat and tidy.

dta <-
  dta %>% 
  mutate(
    date = 
      ifelse(is.na(date1) == T,
             date2,
             date1) %>% 
      as.Date(origin = "1970-01-01")
  ) %>% 
  select(-date1, -date2)


# Next, we'll create a time tracking variable that counts up each month since
# the first one in the data. We'll also order the rows by time.

dta <- 
  dta %>% 
  mutate(
    month = 
      date %>% 
      floor_date("months") %>% 
      interval(min(., na.rm = T), .) %>% 
      divide_by(months(1)) %>% 
      add(1)
  ) %>% 
  arrange(month)


# We'll convert the voting intention item from specific parties to "Incumbent"
# or "Other" based on who was in power at what time. I've kept in those who
# said "Don't know", etc., as it's probable that some voters who support the
# incumbent will pick these options when things get bad so that they don't have
# to challenge their own party identification. Likewise, some opposition
# supporters will pick them when things are good. Keeping them in not only
# preserves statistical power, it also takes this into account.

dta <-
  dta %>% 
  mutate(
    vote =
      vote %>% 
      as_factor() %>% 
      mark_na(c("8", "9")) %>% 
      fct_recode(
        "Con" = "Conservative",
        "Lab" = "Labour",
        "Lib" = "Liberal Democrat",
        "Oth" = "Other Party",
        "Oth" = "Don't know",
        "Oth" = "None, won't vote"
      )
  ) %>% 
  mutate(
    vote = 
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


# We'll also create a new variable "from" that tracks the time that has
# passed since 1 January 1997 (the earliest date in the GDP data). This
# is useful for linking each case to the relevant GDP data in the Stan
# model later on. Note that this is indexed such that the first value
# is 1, not 0.

dta <- 
  dta %>% 
  mutate(
    from = 
      interval(
        start = "1996-12-31",
        end = date
      ) %>% 
      divide_by(days(1)) %>% 
      floor()
  )


# We also need to calculate for each case in the data how many monthly
# comparisons they will be allocated to. This varies depending on if they
# took their survey during the 1997-2010 Labour government or the 2010-2015
# Coalition government as we don't want to allocate voters to GDP change
# that took place under the previous government.

dta <- 
  dta %>% 
  mutate(
    term = ifelse(date <= "2010-05-06", "1997-05-01", "2010-05-06"),
    time =
      interval(
        start = term,
        end = date,
      ) %>% 
      divide_by(days(60)) %>% 
      floor()
  )


# Now, we'll remove any missing data or cases with zero comparisons with
# list-wise deletion.

dta <- 
  dta %>% 
  filter(time != 0) %>% 
  na.omit()


# Next, we'll then loop over the data with map and create a vector of each
# of the dates that each case will be linked to in the final Stan model.
# We'll call this "to".

max_time <- max(dta$time)

dta <- 
  dta %>% 
  mutate(
    to = 
      map2(
        .x = date,
        .y = time,
        .f = function(x, y){
          
          # Calculate comparison dates
          to <- 
            seq(1, y) %>%
            multiply_by(-60) %>%
            as_date(origin = x) %>% 
            as.numeric() %>% 
            subtract(9861) %>%  # 1996-12-31 in numeric
            floor()
          
          # Add in very large values to cause a problem if Stan calls them
          to <- c(to,rep(1e6, max_time - y))
        }
      )
  )


# Finally, we'll create a matrix of the "to" values that we have just worked
# out so that we can feed them into Stan later on. First, we'll create an
# empty matrix and populate it with Inf values so that Stan will notify us
# if something goes wrong.

to_mtrx <- do.call("rbind", dta$to)



# 3. Save data ------------------------------------------------------------

# First, we'll save the individual-level data so that we can recall it when
# we fit our models.

saveRDS(dta, here("_output", "dta.rds"))


# Second, we'll save the "to matrix" so that we can recall it later too.

saveRDS(to_mtrx, here("_output", "to_mtrx.rds"))



# 4. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "002_cms.txt"))


# One last thing...

thanks()
