clean_test_data <- function(x) {
  names(x) <-tolower(names(x))
  x$gang_affiliated[x$gang_affiliated == ""] <- NA
  
  dichotomous_to_factorize <- c(
    "condition_mh_sa",
    "condition_cog_ed",
    "condition_other",
    "employment_exempt",
    "gang_affiliated",
    "prior_arrest_episodes_domesticviolencecharges",
    "prior_arrest_episodes_guncharges",
    "prior_conviction_episodes_ppviolationcharges",
    "prior_conviction_episodes_violent",
    "prior_conviction_episodes_domesticviolencecharges",
    "prior_conviction_episodes_guncharges",
    "prior_revocations_parole",
    "prior_revocations_probation",
    "recidivism_within_3years",
    "recidivism_arrest_year1",
    "recidivism_arrest_year2",
    "recidivism_arrest_year3",
    "violations_electronicmonitoring",
    "violations_instruction",
    "violations_failtoreport",
    "violations_movewithoutpermission"
  )
  
  x_supervision_factor <- c(
    "violations_electronicmonitoring",
    "violations_instruction",
    "violations_failtoreport",
    "violations_movewithoutpermission",
    "employment_exempt"
  )
  
  x_supervision_numeric <- c(
    "delinquency_reports",
    "program_attendances",
    "program_unexcusedabsences",
    "residence_changes",
    "avg_days_per_drugtest",
    "drugtests_thc_positive",
    "drugtests_cocaine_positive",
    "drugtests_meth_positive",
    "drugtests_other_positive",
    "percent_days_employed",
    "jobs_per_year"
  )
  factorize_var <- function(df, var = "x") {
    return(mutate_at(df, vars(var), factor))
  }
  #
  #
  x <-
    rename(x, prior_arrest_episodes_domesticviolencecharges = prior_arrest_episodes_dvcharges) %>%
    rename(prior_conviction_episodes_violent = prior_conviction_episodes_viol) %>%
    # rename(prior_arrest_episodes_guncharges = prior_arrest_episodes_1) %>%
    # rename(prior_conviction_episodes_felony = prior_conviction_episodes) %>%
    # rename(prior_conviction_episodes_misd = prior_conviction_episodes_1) %>%
    # rename(prior_conviction_episodes_violent = prior_conviction_episodes_2) %>%
    # rename(prior_conviction_episodes_prop = prior_conviction_episodes_3) %>%
    # rename(prior_conviction_episodes_drug = prior_conviction_episodes_4) %>%
    # rename(violations_instruction = violations_instruction) %>%
    # rename(violations_electronicmonitoring = violations) %>%
    # rename(violations_movewithoutpermission = violations_1) %>%
    factorize_var(var = dichotomous_to_factorize) %>%
    mutate(id = as.character(id)) %>%
    mutate(gender = factor(gender, labels = c("F", "M"))) %>%
    mutate(race = factor(race, labels = c("BLACK", "WHITE"))) %>%
    mutate(age_at_release = factor(age_at_release)) %>%
    mutate(supervision_level_first = factor(
      supervision_level_first,
      levels = c("Standard", "High", "Specialized")
    )) %>%
    mutate(education_level = factor(
      education_level,
      levels = c(
        "Less than HS diploma",
        "High School Diploma",
        "At least some college"
      )
    )) %>%
    mutate(dependents = factor(
      dependents,
      levels = c("0", "1", "2", "3 or more")
    )) %>%
    mutate(prison_years = factor(
      prison_years,
      levels = c(
        "Less than 1 year",
        "1-2 years",
        "Greater than 2 to 3 years",
        "More than 3 years"
      )
    )
    ) 
  
  #dichomotizing the percentages of positive tests, set missings as level "not tested"
  
  x <- x %>%
    mutate_at(vars(x_tests), dichomotize_perc_col) %>%
    mutate_at(vars(x_tests), factor)
  
  x$prior_arrest_episodes_felony[x$prior_arrest_episodes_felony ==
                                   "10 or more"] <- 10
  x$prior_arrest_episodes_felony <-
    as.numeric(x$prior_arrest_episodes_felony)
  
  x$prior_arrest_episodes_misd[x$prior_arrest_episodes_misd ==
                                 "6 or more"] <- 6
  x$prior_arrest_episodes_misd <-
    as.numeric(x$prior_arrest_episodes_misd)
  
  x$prior_arrest_episodes_violent[x$prior_arrest_episodes_violent ==
                                    "3 or more"] <- 3
  x$prior_arrest_episodes_violent <-
    as.numeric(x$prior_arrest_episodes_violent)
  
  x$prior_arrest_episodes_property[x$prior_arrest_episodes_property ==
                                     "5 or more"] <- 5
  x$prior_arrest_episodes_property <-
    as.numeric(x$prior_arrest_episodes_property)
  
  x$prior_arrest_episodes_drug[x$prior_arrest_episodes_drug ==
                                 "5 or more"] <- 5
  x$prior_arrest_episodes_drug <-
    as.numeric(x$prior_arrest_episodes_drug)
  
  x$prior_arrest_episodes_ppviolationcharges[x$prior_arrest_episodes_ppviolationcharges ==
                                               "5 or more"] <- 5
  x$prior_arrest_episodes_ppviolationcharges <-
    as.numeric(x$prior_arrest_episodes_ppviolationcharges)
  
  x$prior_conviction_episodes_felony[x$prior_conviction_episodes_felony ==
                                       "3 or more"] <- 3
  x$prior_conviction_episodes_felony <-
    as.numeric(x$prior_conviction_episodes_felony)
  
  x$prior_conviction_episodes_misd[x$prior_conviction_episodes_misd ==
                                     "4 or more"] <- 4
  x$prior_conviction_episodes_misd <-
    as.numeric(x$prior_conviction_episodes_misd)
  
  x$prior_conviction_episodes_prop[x$prior_conviction_episodes_prop ==
                                     "3 or more"] <- 3
  x$prior_conviction_episodes_prop <-
    as.numeric(x$prior_conviction_episodes_prop)
  
  x$prior_conviction_episodes_drug[x$prior_conviction_episodes_drug ==
                                     "2 or more"] <- 2
  x$prior_conviction_episodes_drug <-
    as.numeric(x$prior_conviction_episodes_drug)
  
  x$delinquency_reports[x$delinquency_reports ==
                          "4 or more"] <- 4
  x$delinquency_reports <-
    as.numeric(x$delinquency_reports)
  
  x$program_attendances[x$program_attendances ==
                          "10 or more"] <- 10
  x$program_attendances <-
    as.numeric(x$program_attendances)
  
  x$program_unexcusedabsences[x$program_unexcusedabsences ==
                                "3 or more"] <- 3
  x$program_unexcusedabsences <-
    as.numeric(x$program_unexcusedabsences)
  
  x$residence_changes[x$residence_changes ==
                        "3 or more"] <- 3
  x$residence_changes <-
    as.numeric(x$residence_changes)
  print(summary(x))
  return(x)
}