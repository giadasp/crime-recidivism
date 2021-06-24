x_tests <- c(
  "drugtests_cocaine_positive",
  "drugtests_thc_positive",
  "drugtests_other_positive",
  "drugtests_meth_positive"
)

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
  # "avg_days_per_drugtest",
  # "drugtests_thc_positive",
  # "drugtests_cocaine_positive",
  # "drugtests_meth_positive",
  # "drugtests_other_positive",
  "percent_days_employed",
  "jobs_per_year"
)
