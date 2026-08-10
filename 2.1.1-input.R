
# Choices -----------------------------------------------------------------

adm_age_group_choices <- c("50-54", "55-59", "60-64", "65-69", "70-74",
                           "75-79", "80-84", "85-89", "90-94", "95-99", "100+")

country_birth_choices <- c("Australia" = "AUS", "Other" = "OTHER")
language_choices <- c("English" = "eng", "Other" = "other")

organisation_type_choices <- c("Government", "Not-for-profit", "Private")
service_size_choices <- c("0-20", "21-40", "41-60", "61-80", "81-100", "100+")
acpr_ses_choices <- c("Q1", "Q2", "Q3", "Q4", "Q5")

remoteness_choices <- c(
  "Major city / metropolitan area" = "Major Cities",
  "Inner regional area" = "Inner Regional",
  "Outer regional / remote / very remote area" = "Outer Regional/Remote/Very Remote"
)

state_choices <- c(
  "New South Wales" = "NSW",
  "Victoria" = "VIC",
  "Queensland" = "QLD",
  "South Australia" = "SA",
  "Western Australia" = "WA",
  "Tasmania" = "TAS",
  "Australian Capital Territory" = "ACT",
  "Northern Territory" = "NT"
)



# Helper converters -------------------------------------------------------

age_to_group <- function(age) {
  dplyr::case_when(
    age >= 50  & age <= 54  ~ "50-54",
    age >= 55  & age <= 59  ~ "55-59",
    age >= 60  & age <= 64  ~ "60-64",
    age >= 65  & age <= 69  ~ "65-69",
    age >= 70  & age <= 74  ~ "70-74",
    age >= 75  & age <= 79  ~ "75-79",
    age >= 80  & age <= 84  ~ "80-84",
    age >= 85  & age <= 89  ~ "85-89",
    age >= 90  & age <= 94  ~ "90-94",
    age >= 95  & age <= 99  ~ "95-99",
    age >= 100              ~ "100+",
    .default = NA_character_
  )
}

service_to_group <- function(service) {
  dplyr::case_when(
    service >= 0   & service <= 20 ~ "0-20",
    service >= 21  & service <= 40 ~ "21-40",
    service >= 41  & service <= 60 ~ "41-60",
    service >= 61  & service <= 80 ~ "61-80",
    service >= 81  & service <= 99 ~ "81-100",
    service >= 100                 ~ "100+",
    .default = NA_character_
  )
}