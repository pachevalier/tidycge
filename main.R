library("readr")
library("dplyr")
library("tricky")
library("readxl")
library("tidyr")

read_csv(
  file = "data-raw/tidy_ncc.csv"
  ) %>% 
  glimpse()

read_csv(
  file = "data-raw/tidy_ncc.csv", 
  col_types = cols(numero = col_character())
  ) %>% 
  glimpse()


table_cge <- read_excel(path = "data-raw/2012 - 2017_Balances des comptes de l'État.xlsx") %>% 
  set_standard_names() 

table_cge %>% 
  glimpse()

table_cge 