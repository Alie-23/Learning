#### Canadian Cancer Application Using HMD Intermediate Age-specific Death Rates 1950-2022 ####

#libraries
library(readr)
library(tidyverse)
library(ggplot2)

can_int_dat_raw <- read_csv("CAN_m_interm_orig.csv",
col_types = cols(
  country = col_character(),
  year = col_double(),
  sex = col_character(),  
  agf = col_double(),
  cause = col_character(),
  m0 = col_character(), m1 = col_character(), m5 = col_character(), m10 = col_character(),
  m15 = col_character(), m20 = col_character(), m25 = col_character(), m30 = col_character(),
  m35 = col_character(), m40 = col_character(), m45 = col_character(), m50 = col_character(),
  m55 = col_character(), m60 = col_character(), m65 = col_character(), m70 = col_character(),
  m75 = col_character(), m80 = col_character(), m85p = col_character(), m85 = col_character(),
  m90p = col_character(), m90 = col_character(), m95p = col_character(), m95 = col_character(),
  m100p = col_character()
))

#add more rows with pivot longer to remove the raw age groups
can_int_dat <- can_int_dat_raw %>% 
  pivot_longer(
    cols = starts_with("m"),
    names_to = "age",
    values_to = "deaths"
  ) %>% 
  filter(!age %in% c("m85p", "m90p", "m95p", "m100p"))

#Removed ages above 85 because this data ends up not being available in the 2000's, so code below shows that d85p includes ages 85-100
can_int_dat <- can_int_dat %>% 
  mutate(
    age_group = case_when(
      age == "m0"  ~ "<1",
      age == "m1"  ~ "1-4",
      age == "m5"  ~ "5-9",
      age == "m10" ~ "10-14",
      age == "m15" ~ "15-19",
      age == "m20" ~ "20-24",
      age == "m25" ~ "25-29",
      age == "m30" ~ "30-34",
      age == "m35" ~ "35-39",
      age == "m40" ~ "40-44",
      age == "m45" ~ "45-49",
      age == "m50" ~ "50-54",
      age == "m55" ~ "55-59",
      age == "m60" ~ "60-64",
      age == "m65" ~ "65-69",
      age == "m70" ~ "70-74",
      age == "m75" ~ "75-79",
      age == "m80" ~ "80-84",
      age == "m85" ~ "85-89",
      age == "m90" ~ "90-94",
      age == "m95" ~ "95-99",
      TRUE ~ NA_character_
    ),
    age_start = case_when(
      age == "m0" ~ 0,
      TRUE ~ as.numeric(stringr::str_extract(age, "\\d+"))
    ),
    age_width = case_when(
      age == "d0" ~ 1,
      age == "d1" ~ 4,
      TRUE ~ 5
    ),
    deaths_numeric = as.numeric(deaths)
  ) %>%
  filter(
    !is.na(age_group),
    !cause %in% c(
      "I000","I001","I002","I003","I004","I005","I006",
      "I024","I025","I026","I027","I028","I029","I030","I031","I032",
      "I033","I034","I035","I036","I037","I038","I039","I040","I041",
      "I042","I043","I044","I045","I046","I047","I048","I049","I050",
      "I051","I052","I053","I054","I055","I056","I057","I058"
    )
  )

