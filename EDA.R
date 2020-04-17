library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)
library(forcats)

temp = list.files("data",pattern="*.csv")
myfiles = lapply(paste0("data/",temp),  read_csv)

GA_all_blog_by_week <- myfiles[[1]]
GA_data <- myfiles[[2]]
approved_companies <- myfiles[[3]]
cms_data <- myfiles[[4]]

names(GA_all_blog_by_week)
names(GA_data)
names(approved_companies)
names(cms_data)

str(cms_data)


CONTENT_ID_REGEXP = "-(\\d+)\\.?"
REMOVE_CHARACTERS = "[\\.-]"


approved_companies <- approved_companies %>% mutate_all(as.character)

cms_cleaned_data <- cms_data %>% select(`Content ID`, Sections) %>% mutate(Sections = as.character(Sections)) %>% 
                        filter(Sections!="") %>% 
                        mutate(Sections = gsub(", Inc\\.*","",Sections))  %>%
                        mutate(Sections = gsub("Inc\\.*","",Sections)) %>%
                        mutate(SplitSection = strsplit(gsub(", ",",",Sections),",") )

approved_companies <- approved_companies %>% mutate(name = gsub(", Inc\\.*","",name),",") %>%  
                        mutate(name = str_trim(gsub("Inc\\.*","",name)))
                        


unique_sections <- unique(c(unlist(cms_cleaned_data$SplitSection)))

sparse_sections <- cms_cleaned_data %>% separate(Sections, paste0('section-',
                      sep='',
                      seq(1, length(unique_sections), 1)), sep = ",")

section_wide_df <- sparse_sections %>% select(-SplitSection) %>%
                            gather(key = "section_name", value = "Section", -c(`Content ID`)) %>%
                            mutate(Section = str_trim(Section)) %>%
                            filter(!is.na(Section) & Section %in% approved_companies$name)


A <- GA_data %>% mutate(ContentId = gsub(REMOVE_CHARACTERS, '',str_extract(Page, CONTENT_ID_REGEXP ) )) %>% 
            filter(ContentId == 2641658620)


analytics_data_join <- GA_data %>% mutate(ContentId = gsub(REMOVE_CHARACTERS, '',str_extract(Page, CONTENT_ID_REGEXP ) )) %>% 
            mutate(`Bounce Rate` = as.numeric(sub("%","",`Bounce Rate`))/100) %>%
            mutate(`% Exit` = as.numeric(sub("%","",`% Exit`))/100) %>%
            filter(!is.na(ContentId)) %>% mutate(ContentId = as.numeric(ContentId), Pageviews = as.numeric(Pageviews)) %>% 
            inner_join(section_wide_df, by= c("ContentId"="Content ID")) %>% 
            select(-`Page Value`)
            #select(ContentId, Pageviews, Section, `Bounce Rate`, `Unique Pageviews`, `Avg. `)


company_pageviews <- analytics_data_join %>% group_by(Section) %>% 
          summarise(TotalUniquePageViews = sum(`Unique Pageviews`), AvgBR = mean(`Bounce Rate`)) %>%
          arrange(desc(TotalUniquePageViews)) 



BRPV <- analytics_data_join %>% group_by(Section) %>% 
  summarise(`Bounce Rate per Page view` = sum(Pageviews) * mean(`Bounce Rate`)) %>%
  arrange(desc(`Bounce Rate per Page view`)) 


analytics_data_join %>% group_by(ContentId) %>% 
  summarise(Percentage = sum(Pageviews)/sum(analytics_data_join$Pageviews)) %>%
  arrange(desc(Percentage)) %>% 
  head(50) %>%
  mutate(ContentId = as.factor(ContentId)) %>%
  ggplot(aes(x = fct_reorder(ContentId, Percentage), y = Percentage)) + 
  geom_bar(stat="identity") + 
  coord_flip()

company_pageviews %>% ggplot(aes(x = fct_reorder(Section, TotalPageViews), y = TotalPageViews)) + 
    geom_bar(stat="identity") + 
    coord_flip()


