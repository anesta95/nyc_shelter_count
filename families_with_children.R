library(RSocrata)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(janitor)


#write something here to update this daily

#write something that checks if there is new LL37 data each day
  # write code to update the csv with each of the 4 agency monthly updates
  # if using datawrapper api only update the chart once all four agency updates are in

#-----------------------------------------------------------------------------------

#Local law 37 datasets

#write something that will update all of these datsets when there is new data

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
             date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d")) %>% 
      select(date, count, table)
  }
  else {
    raw %>% 
      filter({{var}} == value) %>% 
      mutate_at(c("total_single_adults", "total_adults_in_families", "total_children"), as.numeric) %>% 
      mutate(count = total_single_adults + total_adults_in_families + total_children,
             date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d")) %>% 
      select(date, count, table)
  }
}

unique_by_agency_new <- map2_df(names(data_links), data_links, ~ll37_extract(.x, .y)) %>% 
  arrange(date)

# reading in main data file
unique_by_agency <- read_csv("./data/ll37_data_unique_by_agency.csv",
                             col_names = T,
                             col_types = "Ddc")

latest_new_data_date <- max(unique_by_agency_new$date,
                            na.rm = T)

latest_old_data_date <- max(unique_by_agency$date,
                            na.rm = T)

if (latest_new_data_date > latest_old_data_date) {
  # Write to disk if new data
  unique_by_agency_new %>% write_csv("./data/ll37_data_unique_by_agency.csv")
}






  



