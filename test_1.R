library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(lubridate)
library(gtsummary)
library(pec)

setwd("C:\\Users\\giada.spaccapanico2\\Desktop\\repos\\crime-recidivism")

test_1 <- read.csv("data\\test_1.csv")
source("clean_test_data.R")
source("transform_test_data.R")

summary(test_1)
test_1_recoded <- clean_test_data(test_1)
test_1_recoded <- arrange(test_1_recoded, id)
test_1_recoded_m_drug_tested <- test_1_recoded %>%
  filter(gender == "M") %>% 
  filter(drugtests_meth_positive != "not tested") %>%
  droplevels()

test_1_s <- transform_test_data(test_1_recoded_m_drug_tested)
N <- nrow(test_1_recoded_m_drug_tested)
# survival probability
predictions_1 <- 1-predictSurvProb(coxmodel,
                                   newdata = test_1_s[seq(1,nrow(test_1_s),4),],
                                   times = c(time_span*2, time_span*3, time_span*4)
                                   )
# number of events for each time
predictions_expected_events <- round(t(
  matrix(
    predict(
      coxmodel,
      newdata=test_1_s,
      id=id,
      type="expected"),
    ncol=N)[2:4,]),2)
predictions_surv_prob <- round(exp(-predictions_2),2)[,2:4]
predictions_risk <-round(t(
  matrix(
    predict(
      coxmodel,
      newdata=test_1_s,
      id=id,
      type="risk"),
    ncol=N)[2:4,]),2)

cbind(round(predictions_1,2), predictions_expected_events, predictions_surv_prob, test_1_recoded_m_drug_tested[,c("recidivism_arrest_year1","recidivism_arrest_year2","recidivism_arrest_year3")])

threshold = 0.75

predictions_risky <- predictions_expected_events
predictions_risky[predictions_risky>threshold] <- 1
predictions_risky[predictions_risky<=threshold] <- 0

cbind(predictions_expected_events,test_1_recoded_m_drug_tested[,c("recidivism_arrest_year1","recidivism_arrest_year2","recidivism_arrest_year3")])
cbind(predictions_risk,test_1_recoded_m_drug_tested[,c("recidivism_arrest_year1","recidivism_arrest_year2","recidivism_arrest_year3")])
