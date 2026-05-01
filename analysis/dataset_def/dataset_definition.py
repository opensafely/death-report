###################################################
# This script calculates yearly death counts from 
# ONS and Primary Care records, and extracts 
# dates of death to analyze differences between 
# the ONS and PC datasets.
#
# Author: Martina Pesce / Andrea Schaffer
#   Bennett Institute for Applied Data Science
#   University of Oxford, 2025
###################################################

from ehrql import create_dataset, case, when, codelist_from_csv
from ehrql.tables.tpp import (
    patients,
    ons_deaths,
    clinical_events,
    practice_registrations,
    addresses,
)

dataset = create_dataset()


# -----------------------------------------------------------------------------
# Population
# -----------------------------------------------------------------------------

# Death definitions ------------------

# ONS death date recorded
has_ons_death_date = ons_deaths.date.is_not_null()

# TPP structured death date recorded
has_tpp_death_date = patients.date_of_death.is_not_null()

# TPP coded death recorded
tpp_coded_death_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-death_cod.csv",
    column="code",
)

tpp_coded_death_event = (
    clinical_events
    .where(clinical_events.snomedct_code.is_in(tpp_coded_death_codes))
    .sort_by(clinical_events.date)
    .first_for_patient()
)

has_tpp_coded_death = tpp_coded_death_event.date.is_not_null()

# Death recorded in any source
has_any_death = (
    has_ons_death_date
    | has_tpp_death_date
    | has_tpp_coded_death
)

# Reference death date:
# 1) ONS death date
# 2) TPP structured death date
# 3) TPP coded death date

ref_death_date = case(
    when(has_ons_death_date).then(ons_deaths.date),
    when(has_tpp_death_date).then(patients.date_of_death),
    when(has_tpp_coded_death).then(tpp_coded_death_event.date),
)


# Age at reference death date ---------------
age_at_death = patients.age_on(ref_death_date)

# Keep plausible ages only.
# Also retain infants aged <1 year, whose integer age may be recorded as 0.
has_possible_age = (
    ((age_at_death >= 0) & (age_at_death < 110))
    | (patients.date_of_birth.year == ref_death_date.year)
)

# Restrict to male/female categories for disclosure control -----------------------
has_non_disclosive_sex = (
    (patients.sex == "male")
    | (patients.sex == "female")
)

# Define population
dataset.define_population(
    has_any_death
    & has_possible_age
    & has_non_disclosive_sex
)

# -----------------------------------------------------------------------------
# Variables
# -----------------------------------------------------------------------------

# Death ------
dataset.ons_death_date = ons_deaths.date
dataset.tpp_death_date = patients.date_of_death
dataset.tpp_coded_death_date = tpp_coded_death_event.date
# dataset.ref_death_date = ref_death_date


## Last registration -----
last_registration = (
    practice_registrations
    .sort_by(
        practice_registrations.start_date,
        practice_registrations.end_date,
    )
    .last_for_patient()
)

dataset.last_registration_start_date = last_registration.start_date
dataset.last_registration_end_date = last_registration.end_date

# Demographic --------

### Age at ref date of death 
dataset.date_of_birth = patients.date_of_birth
dataset.age = patients.age_on(ref_death_date)

# Sex
dataset.sex = patients.sex

#Ethnicity
ethnicity5 = codelist_from_csv(
  "codelists/opensafely-ethnicity-snomed-0removed.csv",
  column="code",
  category_column="Label_6", # it's 6 because there is an additional "6 - Not stated" but this is not represented in SNOMED, instead corresponding to no ethnicity code
)

dataset.ethnicity = (
    clinical_events
    .where(clinical_events.snomedct_code.is_in(ethnicity5))
    .sort_by(clinical_events.date)
    .last_for_patient()
    .snomedct_code
    .to_category(ethnicity5)
)

#IMD
dataset.imd = addresses.for_patient_on(ref_death_date).imd_rounded

# Rurality 
dataset.rural_urban = addresses.for_patient_on(ref_death_date).rural_urban_classification

# Practice region
dataset.region = practice_registrations.for_patient_on(ref_death_date).practice_nuts1_region_name

### Practice (anonymous)
dataset.practice = practice_registrations.for_patient_on(ref_death_date).practice_pseudo_id

# -----------------------------------------------------------------------------
# Dummy data
# -----------------------------------------------------------------------------

dataset.configure_dummy_data(
    population_size=10000,
    timeout=180,
    additional_population_constraint=(
        (dataset.tpp_death_date.is_on_or_between("2008-01-01", "2026-05-01")
            | dataset.tpp_death_date.is_null()
        ) &
        (dataset.tpp_coded_death_date.is_on_or_between("2008-01-01", "2026-05-01")
            | dataset.tpp_coded_death_date.is_null()
        ) &
        (dataset.ons_death_date.is_on_or_between("2015-01-01", "2026-05-01")
            | dataset.ons_death_date.is_null()
        ) &
        (dataset.last_registration_start_date.is_on_or_between("2008-01-01", "2026-05-01")
            | dataset.last_registration_start_date.is_null()
        )
    ),
)

# # Died during study and while registered
# ## TPP death during study and while registered
# tpp_death_during_study = (
#     patients.date_of_death.is_on_or_after(start_date) &
#     patients.date_of_death.is_on_or_before(end_date) &    
#     (
#         patients.date_of_death.is_on_or_before(last_registration.end_date + days(30)) |
#         last_registration.end_date.is_null()
#     )
# )

# dataset.tpp_death_during_study = tpp_death_during_study

# # ONS death during study and while registered, allowing for missing end_date
# ons_death_during_study = (
#     ons_deaths.date.is_on_or_after(start_date) &
#     ons_deaths.date.is_on_or_before(end_date) &    
#     (
#         ons_deaths.date.is_on_or_before(last_registration.end_date  + days(30)) |
#         last_registration.end_date.is_null()
#     )
# )

# dataset.ons_death_during_study = ons_death_during_study

# # died during study any (ONS or TPP)

# dataset.any_death_during_study = ons_death_during_study | tpp_death_during_study


# ## Include people registered with a TPP practice
# dataset.has_registration = practice_registrations.for_patient_on(year_start_DoD).exists_for_patient() |  ((patients.date_of_birth.year == year_start_DoD.year) & practice_registrations.for_patient_on(earliest_DoD).exists_for_patient())
### Practice (anonymous)
# dataset.practice = practice_registrations.for_patient_on(earliest_DoD).practice_pseudo_id
# dataset.IMD_q10 = case(
#         when((imd >= 0) & (imd < int(32844 * 1 / 10))).then("1 (most deprived)"),
#         when(imd < int(32844 * 2 / 10)).then("2"),
#         when(imd < int(32844 * 3 / 10)).then("3"),
#         when(imd < int(32844 * 4 / 10)).then("4"),
#         when(imd < int(32844 * 5 / 10)).then("5"),
#         when(imd < int(32844 * 6 / 10)).then("6"),
#         when(imd < int(32844 * 7 / 10)).then("7"),
#         when(imd < int(32844 * 8 / 10)).then("8"),
#         when(imd < int(32844 * 9 / 10)).then("9"),
#         when(imd >= int(32844 * 9 / 10)).then("10 (least deprived)"),
#         otherwise="unknown"
# )
# dataset.age_band = case(
#     when((age < 45)).then("0-44"),
#     when((age >= 45) & (age < 65)).then("45-64"),
#     when((age >= 65) & (age < 75)).then("65-74"),
#     when((age >= 75) & (age < 85)).then("75-84"),
#     when(age >= 85).then("85+"),
#     )



