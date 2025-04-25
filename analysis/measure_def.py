###################################################
# This script creates year counts/rates of deaths
#  in ONS and PC records
#
# Author: Martina Pesce / Andrea Schaffer
#   Bennett Institute for Applied Data Science
#   University of Oxford, 2025
#####################################################################

from ehrql import INTERVAL, create_measures, years, case, when
from ehrql.tables.tpp import patients, practice_registrations, ons_deaths, addresses

 
##########
#Numerator: dead during the period and registred on ONS/GP data
GP_death_in_interval = patients.date_of_death.is_during(INTERVAL)
ONS_death_in_interval = ons_deaths.date.is_during(INTERVAL)
global_death_in_interval = ons_deaths.date.is_during(INTERVAL) | patients.date_of_death.is_during(INTERVAL)

#Denominator: inclusion criteria
## Include people alive
was_alive_GP = patients.date_of_death.is_after(INTERVAL.start_date) | patients.date_of_death.is_null() 
was_alive_ONS = ons_deaths.date.is_after(INTERVAL.start_date) | ons_deaths.date.is_null() 

## Include people registered with a TPP practice
has_registration = practice_registrations.for_patient_on(INTERVAL.start_date).exists_for_patient()

## Exclude people >110 years due to risk of incorrectly recorded age
has_possible_age= ((patients.age_on(INTERVAL.start_date) < 110) & (patients.age_on(INTERVAL.start_date) > 0)) | (patients.date_of_birth.year == INTERVAL.start_date.year)

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
intervals = years(20).starting_on("2005-01-01")

#Subgroups
## Age 
age = patients.age_on(INTERVAL.start_date)
age_band = case(
    when((age >= 0) & (age < 45)).then("0-44"),
    when((age >= 45) & (age < 65)).then("45-64"),
    when((age >= 65) & (age < 75)).then("65-74"),
    when((age >= 75) & (age < 85)).then("75-84"),
    when(age >= 85).then("85+"),
    )
## Practice
practice = practice_registrations.for_patient_on(INTERVAL.start_date).practice_pseudo_id
## Place of death
death_place = ons_deaths.place
## Practice region
region = practice_registrations.for_patient_on(INTERVAL.start_date).practice_nuts1_region_name
## Rurality
rural_urban = addresses.for_patient_on(INTERVAL.start_date).rural_urban_classification


# Create meassures
measures = create_measures()

measures.configure_dummy_data(population_size=100000)

###################
#GP date of death #
###################
measures.define_measure(
    "GP_mortality_overall",
    numerator= GP_death_in_interval,
    denominator= GP_denominator,
    intervals=intervals,
)

## Age band
measures.define_measure(
    "GP_mortality_age_band",
    numerator= GP_death_in_interval,
    denominator= GP_denominator,
    intervals=intervals,
    group_by={
        "age_band": age_band,
    },
)

## Practice (anonymous)
measures.define_measure(
    "GP_mortality_practice",
    numerator= GP_death_in_interval,
    denominator= GP_denominator,
    intervals=intervals,
    group_by={
        "practice": practice,
    },
)
## Place of death
measures.define_measure(
    "GP_mortality_death_place",
    numerator= GP_death_in_interval,
    denominator= GP_denominator,
    intervals=intervals,
    group_by={
        "death_place": death_place,
    },
)
## Practice region
measures.define_measure(
    "GP_mortality_region",
    numerator= GP_death_in_interval,
    denominator= GP_denominator,
    intervals=intervals,
    group_by={
        "region": region,
    },
)
## Rurality 
measures.define_measure(
    "GP_mortality_rural_urban",
    numerator= GP_death_in_interval,
    denominator= GP_denominator,
    intervals=intervals,
    group_by={
        "rural_urban": rural_urban,
    },
)


###################
# ONS date of death #
###################

##Overall
measures.define_measure(
    "ONS_mortality_overall",
    numerator= ONS_death_in_interval,
    denominator= ONS_denominator,
    intervals=intervals,
)

## Age band
measures.define_measure(
    "ONS_mortality_age_band",
    numerator= ONS_death_in_interval,
    denominator= ONS_denominator,
    intervals=intervals,
    group_by={
        "age_band": age_band,
    },
)

## Practice (anonymous)
measures.define_measure(
    "ONS_mortality_practice",
    numerator= ONS_death_in_interval,
    denominator= ONS_denominator,
    intervals=intervals,
    group_by={
        "practice": practice,
    },
)
## Place of death
measures.define_measure(
    "ONS_mortality_death_place",
    numerator= ONS_death_in_interval,
    denominator= ONS_denominator,
    intervals=intervals,
    group_by={
        "death_place": death_place,
    },
)
## Practice region
measures.define_measure(
    "ONS_mortality_region",
    numerator= ONS_death_in_interval,
    denominator= ONS_denominator,
    intervals=intervals,
    group_by={
        "region": region,
    },
)
## Rurality 
measures.define_measure(
    "ONS_mortality_rural_urban",
    numerator= ONS_death_in_interval,
    denominator= ONS_denominator,
    intervals=intervals,
    group_by={
        "rural_urban": rural_urban,
    },
)

###################
# Global date of death #
###################

##Overall
measures.define_measure(
    "global_mortality_overall",
    numerator= global_death_in_interval,
    denominator= global_denominator,
    intervals=intervals,
)

## Age band
measures.define_measure(
    "global_mortality_age_band",
    numerator= global_death_in_interval,
    denominator= global_denominator,
    intervals=intervals,
    group_by={
        "age_band": age_band,
    },
)

## Practice (anonymous)
measures.define_measure(
    "global_mortality_practice",
    numerator= global_death_in_interval,
    denominator= global_denominator,
    intervals=intervals,
    group_by={
        "practice": practice,
    },
)
## Place of death
measures.define_measure(
    "global_mortality_death_place",
    numerator= global_death_in_interval,
    denominator= global_denominator,
    intervals=intervals,
    group_by={
        "death_place": death_place,
    },
)
## Practice region
measures.define_measure(
    "global_mortality_region",
    numerator= global_death_in_interval,
    denominator= global_denominator,
    intervals=intervals,
    group_by={
        "region": region,
    },
)
## Rurality 
measures.define_measure(
    "global_mortality_rural_urban",
    numerator= global_death_in_interval,
    denominator= global_denominator,
    intervals=intervals,
    group_by={
        "rural_urban": rural_urban,
    },
)






# def measure_definition(source, sub_population):
#    group_by_block = ""
#    if sub_population != "overall":
#        group_by_block = f'''
#    group_by={{
#        "{sub_population}": {sub_population},
#    }},'''
#
#    measure_code = f'''measures.define_measure(
#    name="{source}_mortality_{sub_population}",
#    numerator="{source}_death_in_interval",
#    denominator="{source}_denominator",
#    intervals=intervals,
#    {group_by_block}
#    )'''
#    
#    return measure_code


#measure_definition(GP, age_band)

