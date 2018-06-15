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

library(ggplot2)

table_cge %>% 
  filter(compte == "6066620000") %>%
  group_by(year) %>%
  summarise(balance = sum(balance))  %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = year, y = balance)
    ) + 
  scale_y_continuous(labels = function(x) {format(x = x, scientific = FALSE, big.mark = " ")}) + 
  theme_fivethirtyeight()


table_cge %>% 
  filter(compte == "6066620000") %>%
  group_by(year) %>%
  summarise(balance = sum(balance))




table_cge %>% 
  filter(
    grepl(pattern = "^606", x = compte), 
    year == 2017
  ) %>%
  View()

table_cge %>% 
  filter(
    grepl(pattern = "^6", x = compte), 
    year == 2017
  ) %>%
  summarise(balance = format(sum(balance), scientific = FALSE, big.mark = " "))

table_cge %>% 
  filter(
    grepl(pattern = "^606", x = compte), 
    year == 2017
  ) %>%
  summarise(balance = format(sum(balance), scientific = FALSE, big.mark = " "))

table_cge %>% 
  filter(
    grepl(pattern = "^606", x = compte), 
    year == 2017
  ) %>%
  group_by(compte) %>%
  summarise(balance = sum(balance)) %>%
  left_join(
    y = select(table_ncc, numero, libelle_long_complet_du_compte_cible), 
    by = c("compte" = "numero")
    ) %>%
  select(compte,  libelle_long_complet_du_compte_cible, balance) %>%
  arrange(desc(balance)) %>%
  datatable() %>% 
  DT::formatRound(columns = ~ balance, digits = 0, mark = " ")



table_cge %>% 
  filter(
    grepl(pattern = "^606", x = compte), 
    year == 2017
  ) %>%
  summarise(balance = french_formatting(sum(balance)))


table_cge %>% 
  filter(
    grepl(pattern = "^606", x = compte), 
    year == 2017
    ) %>%
  group_by(libelle_ministere) %>%
  summarise(
    balance = sum(balance)
  ) %>% 
  arrange(desc(balance)) %>%
  datatable()




table_cge %>% 
  glimpse()

