library("readr")
library("dplyr")
library("tricky")
library("readxl")
library("tidyr")
library(ggplot2)
french_formatting <- function (x) 
{
  output <- stringr::str_trim(format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))
  return(output)
}


table_cge %>% 
  filter(compte == "6066620000") %>%
  group_by(libelle_ministere) %>%
  summarise(balance = sum(balance))

table_cge %>% 
  filter(compte == "6066620000") %>%
  group_by(libelle_ministere) %>%
  summarise(balance = sum(balance)) %>%
  left_join(
    y = table_plr, 
    by = c("libelle_ministere" = "ministere_au_1er_janvier_2017")
    ) %>%
  filter(etpt != 0, is.na(etpt) == FALSE) %>%
  mutate(balance_par_agent = balance / etpt) %>%
  arrange(desc(balance_par_agent)) %>%
  ggplot() + 
  geom_col(
    mapping = aes(
      x = reorder(libelle_ministere, balance_par_agent), 
      y = balance_par_agent
      )
    ) + 
  coord_flip() + 
  scale_x_discrete(name = "Ministère") + 
  scale_y_continuous(name = "Dépense de papier par agent")






table_plr <- read_excel(path = "data-raw/PLR2017-Exec-Min_T2_HT2_ETPT-BG_BA.xlsx") %>%
  set_standard_names() %>%
  mutate(
    etpt = parse_French_number(exec_etpt_2017_rap_2017)
    ) %>%
  group_by(ministere_au_1er_janvier_2017) %>%
  summarise(etpt = sum(etpt, na.rm = TRUE))


table_plr
 %>%
  glimpse()

table_plr %>%
  glimpse()

  
  View()


read.csv2("data-raw/PLR2017-Exec-Min_T2_HT2_ETPT-BG_BA.csv")



%>%
  group_by(libelle_ministere) %>%
  summarise(balance = french_formatting(sum(balance)))


table_ncc %>%
  filter()
  distinct(libelle_long_complet_du_compte_cible) 


  distinct() %>%
  View()


table_cge %>% 
  filter(grepl(pattern = "^606", x = compte)) %>%
  View()

table_cge %>%
  distinct(libelle_ministere)

table_cge %>% 
  filter(
    compte == "6066620000", 
    libelle_ministere == "Économie et finances"
    ) %>%
  group_by(year) %>%
  summarise(balance = sum(balance)) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = year, y = balance)
  ) + 
  scale_y_continuous(labels = function(x) {format(x = x, scientific = 
                                                    FALSE, big.mark = " ")})




%>%
  group_by(libelle_ministere) %>%
  summarise(balance = sum(balance)) %>%
  arrange(desc(balance)) %>%
  filter(is.na(libelle_ministere) == FALSE) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = reorder(libelle_ministere, balance), y = balance)
  ) + 
  scale_y_continuous(labels = function(x) {format(x, scientific = FALSE, big.mark = " ")}) + 
  coord_flip()







%>%
  group_by(libelle_ministere) %>%
  summarise(balance = sum(balance)) %>%
  arrange(desc(balance)) %>%
  filter(is.na(libelle_ministere) == FALSE) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = reorder(libelle_ministere, balance), y = balance)
  ) + 
  scale_y_continuous(labels = function(x) {format(x, scientific = FALSE, big.mark = " ")}) + 
  coord_flip()

