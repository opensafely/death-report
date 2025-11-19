###################################################
# This script creates year counts/rates of deaths
#  in ONS and PC records by practice
#
# Author: Martina Pesce / Andrea Schaffer
#   Bennett Institute for Applied Data Science
#   University of Oxford, 2025
###################################################

from ehrql import INTERVAL, create_measures, years, case, when, months, show
from ehrql.tables.tpp import patients, practice_registrations, ons_deaths, addresses

 
##########
#Numerator: dead during the period and registred on ONS/GP date

# Last deregistration date per patient
last_registration_end = (
    practice_registrations
    .sort_by(
        practice_registrations.start_date,
        practice_registrations.end_date
    )
    .last_for_patient()
    .end_date
)

GP_death_in_interval = (
    patients.date_of_death.is_during(INTERVAL) &
    (
        patients.date_of_death.is_on_or_before(last_registration_end) |
        last_registration_end.is_null()
    )
)

ONS_death_in_interval = (
    ons_deaths.date.is_during(INTERVAL) &
    (
        ons_deaths.date.is_on_or_before(last_registration_end) |
        last_registration_end.is_null()
    )
)

global_death_in_interval = GP_death_in_interval | ONS_death_in_interval


#Denominator: inclusion criteria
## Include people alive
was_alive_GP = patients.date_of_death.is_on_or_after(INTERVAL.start_date) | patients.date_of_death.is_null() 
was_alive_ONS = ons_deaths.date.is_on_or_after(INTERVAL.start_date) | ons_deaths.date.is_null() 

## Include people registered with a TPP practice
has_registration = (
    # Registered at the beginning of the period
   ( practice_registrations.for_patient_on(INTERVAL.start_date).exists_for_patient())
    |
    # Born in the same calendar year with a valid registration
    ((patients.date_of_birth.is_during(INTERVAL)) & (practice_registrations.where(practice_registrations.start_date.is_during(INTERVAL))).exists_for_patient())
)

## Exclude people >110 years due to risk of incorrectly recorded age
has_possible_age= ((patients.age_on(INTERVAL.start_date) < 110)  & (patients.age_on(INTERVAL.start_date) > 0) | (patients.date_of_birth.is_during(INTERVAL)))

## Exclude people with non-male or female sex due to disclosure risk
non_disclosive_sex= (patients.sex == "male") | (patients.sex == "female")


# define denominator
GP_denominator =  (was_alive_GP
                   & has_registration 
                   & has_possible_age 
                   & non_disclosive_sex)

ONS_denominator =  (was_alive_ONS
                   & has_registration
                   & has_possible_age 
                   & non_disclosive_sex)

global_denominator =  ( (was_alive_ONS | was_alive_GP)
                       & has_registration 
                       & has_possible_age 
                       & non_disclosive_sex) 

#Specify intervals
intervals = years(6).starting_on("2019-01-01")

## Practice
practice_gral = practice_registrations.for_patient_on(INTERVAL.start_date).practice_pseudo_id 

practice_babies = (practice_registrations
                   .where(patients.date_of_birth.is_during(INTERVAL))
                   .sort_by(practice_registrations.start_date)
                   .first_for_patient()
                   .practice_pseudo_id
                   )

practice = case(
    when(practice_gral.is_not_null()).then(practice_gral),
    when(practice_babies.is_not_null()).then(practice_babies),
    otherwise=None,
)


# Create meassures
measures = create_measures()

measures.configure_dummy_data(population_size=100000)

## GP
measures.define_measure(
    "GP_mortality_practice",
    numerator= GP_death_in_interval,
    denominator= GP_denominator,
    intervals=intervals,
    group_by={
        "practice": practice,
    },
)

## ONS
measures.define_measure(
    "ONS_mortality_practice",
    numerator= ONS_death_in_interval,
    denominator= ONS_denominator,
    intervals=intervals,
    group_by={
        "practice": practice,
    },
)

## Global
measures.define_measure(
    "global_mortality_practice",
    numerator= global_death_in_interval,
    denominator= global_denominator,
    intervals=intervals,
    group_by={
        "practice": practice,
    },
)