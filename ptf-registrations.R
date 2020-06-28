library(readr)
library(skimr)
library(dplyr)
library(stringr)

data <- readr::read_csv('data.csv')
skimr::skim(data)

data_cleaned <- data %>% filter(!stringr::str_detect(Page, "@powertofly\\.com")) %>% 
  mutate(Registrations = 
           case_when(stringr::str_detect(Page, "\\?show_confirmation") ~ 1, T ~ Registrations) )


write_csv(data_cleaned, "data_cleaned.csv")

cleaned_data_without_pages <- readr::read_csv('cleaned_data_without_pages.csv')
pageviews_campaign_sucess_table <- readr::read_csv('pageviews_campaign_sucess_table.csv')

cleaned_data_without_pages %>% pull(Registrations) %>% sum
pageviews_campaign_sucess_table %>% count(Campaign, `Source / Medium`, `Ad Content`, sort = T)

cleaned_data_without_pages %>% right_join(pageviews_campaign_sucess_table) %>% 
                               mutate(Registrations = case_when(is.na(Registrations) ~ 0, T ~ Registrations) ) %>% 
                               group_by(Campaign, `Source / Medium`, `Ad Content`, sort = T) %>%
                               summarise(`Total Registrations` = sum(Registrations)) %>% 
                               arrange(-`Total Registrations`)