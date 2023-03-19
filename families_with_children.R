library(RSocrata)
library(lubridate)
library(tidyverse)
library(janitor)

DHS_Census <- read.socrata("https://data.cityofnewyork.us/resource/3pjg-ncn9.json") %>% 
  pivot_longer(cols = -date_of_census, names_to = "measure", values_to = "count") %>% 
  mutate(table = "DHS daily census",
         count = as.numeric(count))

#write something here to update this daily

#write somthing that checks if there is new LL37 data each day

#-----------------------------------------------------------------------------------

#Local law 37 datasets

#write somethiung that will update all of these datsets when there is new data

data_links <- list(dhs = "https://data.cityofnewyork.us/resource/2mqz-v5im.json",
                dycd = "https://data.cityofnewyork.us/resource/2232-dj5q.json",
                hpd = "https://data.cityofnewyork.us/resource/mdht-5s6e.json",
                hra = "https://data.cityofnewyork.us/resource/e4ty-r26d.json"
           )


ll37_extract <- function(name, link) {
  
  table_name <- name
  # var <- sym(if_else(table_name == "dhs", "ll37_report_row_name", "category"))
  if (table_name == "dhs") {
    str_var <- "ll37_report_row_name"
  } else {
    str_var <- "category"
  }
  
  var <- sym(str_var)
  
  
  value <- case_when(table_name == "dhs" ~ "Number of unduplicated persons: DHS-administered facilities",
                     table_name == "dycd" ~ "number of unduplicated persons - DYCD-administered facilities",
                     table_name == "hra" ~ "Number of unduplicated persons: HRA domestic violence shelters **")
  
  if(table_name != "hra") {
    raw <- read.socrata(link) %>% 
      clean_names() %>% 
      mutate(table = paste0("total unique individuals in ", table_name, " facilities"))
  } else {
    raw <- read.socrata(link) %>% 
      clean_names() %>% 
      mutate(table = paste0("total unique individuals in ", table_name, " facilities")) %>% 
      rename(total_adults_in_families = adults_families)
  }

  if(table_name == "hpd") {
    raw %>%
      filter(facility_type == "HPD Facilities Combined (Census Total)" & facility_indicator == "Census Total") %>% 
      mutate(count = as.numeric(total_adults) + as.numeric(total_children),
             date = ym(data_period)) %>% 
      select(date, count, table)
  }
  else {
    raw %>% 
      filter({{var}} == value) %>% 
      mutate_at(c("total_single_adults", "total_adults_in_families", "total_children"), as.numeric) %>% 
      mutate(count = total_single_adults + total_adults_in_families + total_children,
             date = ym(data_period)) %>% 
      select(date, count, table)
  }
}

unique_by_agency <- map2_df(names(data_links), data_links, ~ll37_extract(.x, .y)) %>% 
  arrange(date)

unique_by_agency %>% write_csv("unique_by_agency.csv")




  



