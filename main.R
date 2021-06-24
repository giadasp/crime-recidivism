library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(lubridate)
library(My.stepwise)
library(gtsummary)
#interesting: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6015946/#r8
setwd("C:\\Users\\giada.spaccapanico2\\Desktop\\repos\\crime-recidivism")
source("clean_training_data.R")
source("transform_training_data.R")
source("utils.R")
source("var_class.R")
source("explore.R")
time_span = 1
training <- read.csv("data\\training.csv")
test_1 <- read.csv("data\\test_1.csv")
test_2 <- read.csv("data\\test_2.csv")

summary(training)

training_recoded <- clean_training_data(training)
training_recoded_m_drug_tested <- training_recoded %>%
  filter(gender == "M") %>% 
  filter(drugtests_meth_positive != "not tested") %>%
  droplevels()

summary(training_recoded_m_drug_tested)
explore(training_recoded)

N <- nrow(training_recoded_m)

#transform dataset
training_s <- transform_training_data(training_recoded_m_drug_tested)

training_s <- training_s %>% 
  select(!(residence_puma)) %>% 
  select(!(supervision_risk_score_first)) %>% 
  select(!(supervision_level_first)) %>% 
  select(!(recidivism_arrest_year1)) %>%
  select(!(recidivism_arrest_year2)) %>% 
  select(!(recidivism_arrest_year3)) %>% 
  select(!(recidivism_within_3years)) %>%
  select(!(avg_days_per_drugtest))  
  
covar_list <- names(training_s)[1:45]

#cox model
coxmodel <- coxph(
  Surv(time=tstart, time2=tstop, event=recidive) 
    ~ 
    #strata(gender) 
    # # race +
    + age_at_release 
    # residence_puma +
    + gang_affiliated # error: only male
    # supervision_risk_score_first +
    # supervision_level_first +
    + education_level 
    #+ dependents #p>0.4
    + prison_offense  # maybe we need to put other and violentnonsex together
    + prison_years 
    + prior_arrest_episodes_felony 
    + prior_arrest_episodes_misd 
    + prior_arrest_episodes_violent
    + prior_arrest_episodes_property 
    + prior_arrest_episodes_drug  # NS p=0.7
    + prior_arrest_episodes_ppviolationcharges 
    + prior_arrest_episodes_domesticviolencecharges 
    + prior_arrest_episodes_guncharges # NS p=0.8
    + prior_conviction_episodes_felony 
    + prior_conviction_episodes_misd  # NS p=0.1
    + prior_conviction_episodes_violent  # NS p=0.1
    + prior_conviction_episodes_prop  # NS p=0.4
    + prior_conviction_episodes_drug  # NS p=0.4
    + prior_conviction_episodes_ppviolationcharges 
    + prior_conviction_episodes_domesticviolencecharges  # NS p=0.3
    + prior_conviction_episodes_guncharges  # NS p=0.6
    + prior_revocations_parole 
    + prior_revocations_probation  # NS p=0.3
    + condition_mh_sa 
    + condition_cog_ed # NS p=0.4
    + condition_other  # NS p=0.3
    + violations_electronicmonitoring 
    + violations_instruction  # NS p=0.2
    + violations_failtoreport  # NS p=0.3
    + violations_movewithoutpermission 
    + delinquency_reports 
    + program_attendances 
    + program_unexcusedabsences 
    + residence_changes 
    # #+ avg_days_per_drugtest #it does not have any sense
    + drugtests_thc_positive
    + drugtests_cocaine_positive  #NS p=0.2
    + drugtests_meth_positive 
    + drugtests_other_positive 
    + percent_days_employed 
    #+ jobs_per_year 
    + employment_exempt
  ,
  model=TRUE ,
  data = training_s,
  x=TRUE,
  y=TRUE,
  id = id,
  control= coxph.control(iter.max=1000)
  )  


#model selection
sw_coxmodel<- My.stepwise.coxph(Time = NULL, T1 = "tstart", T2 = "tstop", Status = "recidive",
                  covar_list,  data=training_s, sle = 0.15, sls = 0.15,
                  vif.threshold = 999)

print(coxmodel %>%
        gtsummary::tbl_regression(exp = TRUE, conf.level=0.1))
ggforest(coxmodel, data = training_s)

tracebakc()#not working

cz <- cox.zph(coxmodel)
 print(cz)
 plot(cz[1], lwd=1)

#not working
# survival::plot.survfit(coxmodel)

# fit <- survfit(coxmodel, newdata = training_s )
# ggsurvplot(fit, conf.int = TRUE, palette = "Dark2", 
#            censor = FALSE, surv.median.line = "hv", data = training_s[,3:4])
survminer::ggadjustedcurves(coxmodel, data = training_s,
                            variable = "prison_offense",
                            method="marginal"
                            )
