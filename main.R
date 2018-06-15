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



library("stringr")

table_cge %>% 
  filter(
    compte == "6066620000", 
    libelle_ministere == "Services du Premier ministre"
    ) %>%
  group_by(programme) %>%
  summarise(balance = sum(balance)) %>%
  left_join(
    y = table_plr %>%
      filter(ministere_au_1er_janvier_2017 == "Services du Premier ministre") %>%
      group_by(code_programme, programme) %>%
      summarise(etpt = sum(etpt, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(code_programme = str_pad(as.character(code_programme), width = 4, side = "left", pad = 0 )), 
    by = c("programme" = "code_programme")
  ) %>%
  filter(etpt != 0) %>%
  mutate(balance_par_agent = balance / etpt) %>%
  arrange(desc(balance_par_agent)) %>%
  ggplot() + 
  geom_col(
    mapping = aes(x = reorder(programme.y, balance_par_agent), y = balance_par_agent)
  ) + 
  theme_fivethirtyeight() + 
  scale_y_continuous(name = "Dépense de papier par agent") + 
  scale_x_discrete(name = "Programme") + 
  coord_flip()






  




  glimpse()

  left_join()
  glimpse()


table_plr %>% 
  glimpse()

