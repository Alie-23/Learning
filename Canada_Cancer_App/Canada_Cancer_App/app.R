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

