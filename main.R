library("readr")
library("dplyr")
library("tricky")
library("readxl")

read_csv(
  file = "data-raw/tidy_ncc.csv"
  ) %>% 
  glimpse()

read_csv(
  file = "data-raw/tidy_ncc.csv", 
  col_types = cols(numero = col_character())
  ) %>% 
  glimpse()

read_csv(
  file = "data-raw/tidy_ncc.csv", 
  col_types = cols(numero = col_character())
  ) %>% 
  distinct(numero, libelle_long_complet_du_compte_cible) %>% 
  datatable()


table_cge <- read_excel(path = "data-raw/2012 - 2017_Balances des comptes de l'État.xlsx") %>% 
  set_standard_names() 

