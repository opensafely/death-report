from ehrql import  when, create_dataset, case, minimum_of, codelist_from_csv
from ehrql.tables.tpp import patients, practice_registrations, ons_deaths, addresses, clinical_events

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
#was_alive = patients.date_of_death.is_after(year_start_DoD) | patients.date_of_death.is_null() | ons_deaths.date.is_after(year_start_DoD) | ons_deaths.date.is_null()
#patients.date_of_birth

## Include people registered with a TPP practice
has_registration = practice_registrations.for_patient_on(year_start_DoD).exists_for_patient()

## Exclude people >110 years due to risk of incorrectly recorded age;
has_possible_age= ((patients.age_on(year_start_DoD) < 110) & (patients.age_on(year_start_DoD) > 0)) | (patients.date_of_birth.year == year_start_DoD.year)

## Exclude people with non-male or female sex due to disclosure risk;
non_disclosive_sex= (patients.sex == "male") | (patients.sex == "female")

#Died during the period
died_during_study = patients.date_of_death.is_after(start_date) | ons_deaths.date.is_after(start_date)

# define dataset poppulation
dataset.define_population(
    #was_alive & 
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
dataset.date_of_birth = patients.date_of_birth
age = patients.age_on(year_start_DoD)

dataset.age_band = case(
    when((age < 45)).then("0-44"),
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



#IMD
imd = addresses.for_patient_on(year_start_DoD).imd_rounded

dataset.IMD_q10 = case(
        when((imd >= 0) & (imd < int(32844 * 1 / 10))).then("1 (most deprived)"),
        when(imd < int(32844 * 2 / 10)).then("2"),
        when(imd < int(32844 * 3 / 10)).then("3"),
        when(imd < int(32844 * 4 / 10)).then("4"),
        when(imd < int(32844 * 5 / 10)).then("5"),
        when(imd < int(32844 * 6 / 10)).then("6"),
        when(imd < int(32844 * 7 / 10)).then("7"),
        when(imd < int(32844 * 8 / 10)).then("8"),
        when(imd < int(32844 * 9 / 10)).then("9"),
        when(imd >= int(32844 * 9 / 10)).then("10 (least deprived)"),
        otherwise="unknown"
)


#Ethnicity
ethnicity5 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="code",
  category_column="Label_6", # it's 6 because there is an additional "6 - Not stated" but this is not represented in SNOMED, instead corresponding to no ethnicity code
)

dataset.ethnicity = clinical_events.where(
        clinical_events.snomedct_code.is_in(ethnicity5)
    ).sort_by(
        clinical_events.date
    ).last_for_patient().snomedct_code.to_category(ethnicity5)

# Sex
dataset.sex = patients.sex