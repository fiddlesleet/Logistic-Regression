library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(RcppRoll)
library(dplyr)
library(exploratory)
##########
# GET DATA
##########

df.flight_delays <- read_csv('https://ja.exploratory.io/data/kanaugust/3583654049003612/USFlightDelayData-September.csvx')
# inspect
df.flight_delays

##############
# REGRESSION
##############

model <- lm(DEP_DELAY ~ FL_DATE + ARR_DELAY + dep_hour + DISTANCE, data = df.flight_delays)
model$coefficients

df.flight_delays %>%
  mutate(FL_DATE = ymd(FL_DATE)) %>%
  filter(!is.na(ARR_DELAY)) %>%
  select(FL_DATE, CARRIER, ORIGIN_CITY_NAME, DEST_CITY_NAME, DEP_DELAY, ARR_DELAY, AIR_TIME, DEST, ORIGIN, everything()) %>%
  group_by(CARRIER) %>%
  build_lm(ARR_DELAY ~ DEP_DELAY, test_rate = 0.3) %>%
  prediction(data = "test") %>%
  evaluate_regression(predicted_value, ARR_DELAY)

# logistic regression
df.flight_delays %>%
  mutate(is_delayed = ARR_DELAY > 0)

model <- glm(is_delayed ~ DEP_DELAY + AIR_TIME, family = "binomial", link = "logit")
model

df.flight_delays %>%
select(-CANCELLATION_CODE) %>%
  separate(ORIGIN_CITY_NAME, into = c("city", "state"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
  filter(!is.na(ARR_DELAY)) %>%
  mutate(is_delayed = ARR_DELAY>0) %>%
  select(FL_DATE, CARRIER, ORIGIN, DEST, DEP_DELAY, ARR_DELAY, AIR_TIME, is_delayed, everything()) %>%
  group_by(CARRIER) %>%
  build_lr(is_delayed ~ DEP_DELAY + AIR_TIME, test_rate = 0.3) %>%
  prediction_binary(data = "test", threshold = "accuracy") %>%
  do_roc(predicted_probability, is_delayed)
