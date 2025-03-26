from ehrql import  years, when, create_dataset, case
from ehrql.tables.tpp import patients, practice_registrations, ons_deaths, addresses

from sys import argv
from datetime import datetime

start_date = datetime.strptime(argv[1], '%Y%m%d').strftime('%Y-%m-%d') 
end_date = datetime.strptime(argv[2], '%Y%m%d').strftime('%Y-%m-%d') 


##########
study_year = (start_date, end_date)

# initialise dataset
dataset = create_dataset()
dataset.configure_dummy_data(population_size=1000)


# Inclusion/exclusion criteria:

## Include people alive
was_alive = patients.date_of_death.is_after(start_date) | patients.date_of_death.is_null()

## Include people registered with a TPP practice
has_registration = practice_registrations.for_patient_on(
    start_date
).exists_for_patient()

## Exclude people >110 years due to risk of incorrectly recorded age;
has_possible_age= patients.age_on(start_date) < 110

## Exclude people with non-male or female sex due to disclosure risk;
non_disclosive_sex= (patients.sex == "male") | (patients.sex == "female")


# define dataset poppulation
dataset.define_population(
  was_alive & 
  has_registration & 
  has_possible_age & 
  non_disclosive_sex
)

# Variables


## Death related variables

### Died during the year studied?
dataset.TPP_death = patients.date_of_death.is_during(study_year)
dataset.ONS_death = ons_deaths.date.is_during(study_year)

### Date of death
dataset.TPP_death_date = patients.date_of_death
dataset.ons_death_date = ons_deaths.date


## Sub-analysis variables

### Age band
age = patients.age_on(start_date)

dataset.age_band = case(
    when((age >= 0) & (age < 20)).then("0-19"),
    when((age >= 20) & (age < 40)).then("20-39"),
    when((age >= 40) & (age < 60)).then("40-59"),
    when((age >= 60) & (age < 80)).then("60-79"),
    when(age >= 80).then("80+"),
)

### Practice (anonymous)
dataset.practice = practice_registrations.for_patient_on(start_date).practice_pseudo_id

### Place of death
dataset.ons_death_place = ons_deaths.place

### Practice region
dataset.region = practice_registrations.for_patient_on(start_date).practice_nuts1_region_name

### Rurality 
dataset.rural_urban = addresses.for_patient_on(start_date).rural_urban_classification
