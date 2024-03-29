from cohortextractor import (
    codelist,
    codelist_from_csv,
)

covid_codes = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    system="icd10",
    column="icd10_code",
)

covid_primary_care_positive_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    system="ctv3",
    column="CTV3ID",
)

covid_primary_care_code = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    system="ctv3",
    column="CTV3ID",
)

covid_primary_care_sequalae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    system="ctv3",
    column="CTV3ID",
)

ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_6",
)
ethnicity_codes_16 = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_16",
)


solid_organ_transplantation_codes = codelist_from_csv(
    "codelists/opensafely-solid-organ-transplantation.csv",
    system="ctv3",
    column="CTV3ID",
)

lung_cancer_codes = codelist_from_csv(
    "codelists/opensafely-lung-cancer.csv", system="ctv3", column="CTV3ID",
)
haematological_cancer_codes = codelist_from_csv(
    "codelists/opensafely-haematological-cancer.csv", system="ctv3", column="CTV3ID",
)
bone_marrow_transplant_codes = codelist_from_csv(
    "codelists/opensafely-bone-marrow-transplant.csv", system="ctv3", column="CTV3ID",
)
cystic_fibrosis_codes = codelist_from_csv(
    "codelists/opensafely-cystic-fibrosis.csv", system="ctv3", column="CTV3ID",
)

sickle_cell_disease_codes = codelist_from_csv(
    "codelists/opensafely-sickle-cell-disease.csv", system="ctv3", column="CTV3ID",
)

permanent_immunosuppression_codes = codelist_from_csv(
    "codelists/opensafely-permanent-immunosuppression.csv",
    system="ctv3",
    column="CTV3ID",
)
temporary_immunosuppression_codes = codelist_from_csv(
    "codelists/opensafely-temporary-immunosuppression.csv",
    system="ctv3",
    column="CTV3ID",
)
chronic_cardiac_disease_codes = codelist_from_csv(
    "codelists/opensafely-chronic-cardiac-disease.csv", system="ctv3", column="CTV3ID",
)
intellectual_disability_including_downs_syndrome_codes = codelist_from_csv(
    "codelists/opensafely-intellectual-disability-including-downs-syndrome.csv",
    system="ctv3",
    column="CTV3ID",
)
dialysis_codes = codelist_from_csv(
    "codelists/opensafely-dialysis.csv", system="ctv3", column="CTV3ID",
)
other_respiratory_conditions_codes = codelist_from_csv(
    "codelists/opensafely-other-respiratory-conditions.csv",
    system="ctv3",
    column="CTV3ID",
)
heart_failure_codes = codelist_from_csv(
    "codelists/opensafely-heart-failure.csv", system="ctv3", column="CTV3ID",
)
other_heart_disease_codes = codelist_from_csv(
    "codelists/opensafely-other-heart-disease.csv", system="ctv3", column="CTV3ID",
)

chronic_cardiac_disease_codes = codelist_from_csv(
    "codelists/opensafely-chronic-cardiac-disease.csv", system="ctv3", column="CTV3ID",
)

chemotherapy_or_radiotherapy_codes = codelist_from_csv(
    "codelists/opensafely-chemotherapy-or-radiotherapy.csv",
    system="ctv3",
    column="CTV3ID",
)
cancer_excluding_lung_and_haematological_codes = codelist_from_csv(
    "codelists/opensafely-cancer-excluding-lung-and-haematological.csv",
    system="ctv3",
    column="CTV3ID",
)

current_copd_codes = codelist_from_csv(
    "codelists/opensafely-current-copd.csv", system="ctv3", column="CTV3ID"
)

dementia_codes = codelist_from_csv(
    "codelists/opensafely-dementia.csv", system="ctv3", column="CTV3ID"
)

dmards_codes = codelist_from_csv(
    "codelists/opensafely-dmards.csv", system="snomed", column="snomed_id",
)

dialysis_codes = codelist_from_csv(
    "codelists/opensafely-dialysis.csv", system="ctv3", column="CTV3ID",
)

chronic_liver_disease_codes = codelist_from_csv(
    "codelists/opensafely-chronic-liver-disease.csv", system="ctv3", column="CTV3ID",
)
other_neuro_codes = codelist_from_csv(
    "codelists/opensafely-other-neurological-conditions.csv",
    system="ctv3",
    column="CTV3ID",
)

psychosis_schizophrenia_bipolar_affective_disease_codes = codelist_from_csv(
    "codelists/opensafely-psychosis-schizophrenia-bipolar-affective-disease.csv",
    system="ctv3",
    column="CTV3Code",
)

asplenia_codes = codelist_from_csv(
    "codelists/opensafely-asplenia.csv", system="ctv3", column="CTV3ID"
)

carehome_primis_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv", 
    system="snomed", 
    column="code",
)
