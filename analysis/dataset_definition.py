from ehrql import  when, create_dataset, case, minimum_of
from ehrql.tables.tpp import patients, practice_registrations, ons_deaths, addresses

# initialise dataset
dataset = create_dataset()
dataset.configure_dummy_data(population_size=1000)


#Dates 
start_date = "2005-01-01"
end_date = "2024-12-31"
earliest_DoD = minimum_of(
    patients.date_of_death,
    ons_deaths.date,
)

year_start_DoD = earliest_DoD.to_first_of_year()


# Inclusion/exclusion criteria:

## Include people alive
was_alive = patients.date_of_death.is_after(year_start_DoD) | patients.date_of_death.is_null() | ons_deaths.date.is_after(year_start_DoD)| ons_deaths.date.is_null()
patients.date_of_birth

## Include people registered with a TPP practice
has_registration = practice_registrations.for_patient_on(year_start_DoD).exists_for_patient()

## Exclude people >110 years due to risk of incorrectly recorded age;
has_possible_age= (patients.age_on(year_start_DoD) < 110) & (patients.age_on(year_start_DoD) > 0)

## Exclude people with non-male or female sex due to disclosure risk;
non_disclosive_sex= (patients.sex == "male") | (patients.sex == "female")

#Died during the period
died_during_study = patients.date_of_death.is_after(start_date) | ons_deaths.date.is_after(start_date)

# define dataset poppulation
dataset.define_population(
    was_alive & 
    has_registration & 
    has_possible_age & 
    non_disclosive_sex &
    died_during_study
    )

# Variables

## Death related variables

### Date of death
dataset.TPP_death_date = patients.date_of_death
dataset.ons_death_date = ons_deaths.date


## Sub-analysis variables

### Age band
age = patients.age_on(year_start_DoD)

dataset.age_band = case(
    when((age >= 0) & (age < 45)).then("0-44"),
    when((age >= 45) & (age < 65)).then("45-64"),
    when((age >= 65) & (age < 75)).then("65-74"),
    when((age >= 75) & (age < 85)).then("75-84"),
    when(age >= 85).then("85+"),
    )

### Practice (anonymous)
dataset.practice = practice_registrations.for_patient_on(year_start_DoD).practice_pseudo_id

### Place of death
dataset.ons_death_place = ons_deaths.place

### Practice region
dataset.region = practice_registrations.for_patient_on(year_start_DoD).practice_nuts1_region_name

### Rurality 
dataset.rural_urban = addresses.for_patient_on(year_start_DoD).rural_urban_classification

dataset.start_year_dod= year_start_DoD 