library(RSocrata)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(janitor)
library(stringr)

#write something here to update this daily

#write something that checks if there is new LL37 data each day
  # write code to update the csv with each of the 4 agency monthly updates
  # if using datawrapper api only update the chart once all four agency updates are in

#-----------------------------------------------------------------------------------

#Local law 37 datasets

#write something that will update all of these datsets when there is new data


################
#OLD VERSION - OPEN DATA COMBINED FILES IN SUMMER 2023

# data_links <- list(dhs = "https://data.cityofnewyork.us/resource/2mqz-v5im.json",
#                 dycd = "https://data.cityofnewyork.us/resource/2232-dj5q.json",
#                 hpd = "https://data.cityofnewyork.us/resource/mdht-5s6e.json",
#                 hra = "https://data.cityofnewyork.us/resource/e4ty-r26d.json"
#            )
# 
# 
# ll37_extract <- function(name, link) {
#   
#   table_name <- name
#   # var <- sym(if_else(table_name == "dhs", "ll37_report_row_name", "category"))
#   if (table_name == "dhs") {
#     str_var <- "ll37_report_row_name"
#   } else {
#     str_var <- "category"
#   }
#   
#   var <- sym(str_var)
#   
#   

#   if(table_name != "hra") {
#     raw <- read.socrata(link) %>% 
#       clean_names() %>% 
#       mutate(table = paste0("total unique individuals in ", table_name, " facilities"))
#   } else {
#     raw <- read.socrata(link) %>% 
#       clean_names() %>% 
#       mutate(table = paste0("total unique individuals in ", table_name, " facilities")) %>% 
#       rename(total_adults_in_families = adults_families)
#   }
# 
#   if(table_name == "hpd") {
#     raw %>%
#       filter(facility_type == "HPD Facilities Combined (Census Total)" & facility_indicator == "Census Total") %>% 
#       mutate(count = as.numeric(total_adults) + as.numeric(total_children),
#              date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d")) %>% 
#       select(date, count, table)
#   }
#   else {
#     raw %>% 
#       filter({{var}} == value) %>% 
#       mutate_at(c("total_single_adults", "total_adults_in_families", "total_children"), as.numeric) %>% 
#       mutate(count = total_single_adults + total_adults_in_families + total_children,
#              date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d")) %>% 
#       select(date, count, table)
#   }
# }
################

#historical data - I think we only need to do this once?
# unique_by_agency_historical <- read.socrata("https://data.cityofnewyork.us/resource/bdft-9t6c.csv") %>% 
#   mutate(agency_abb = tolower(gsub("[()]", "", str_extract(agency, "\\([^)]+\\)"))),
#          count = case_when(agency_abb == "dhs" &
#                              category == "Number of unduplicated persons" &
#                              facility_or_program_type == "DHS-administered facilities" ~ total_single_adults + total_adults_on_families + total_children,
#                            agency_abb == "dycd" &
#                              category == "number of unduplicated persons - DYCD-administered facilities" ~ total_single_adults + total_adults_on_families + total_children,
#                            
#                            #do we remember why it's just domestic violence for HRA facilities and not emergency and transitional housing?
#                            agency_abb == "hra" &
#                              category == "Number of unduplicated persons" & facility_or_program_type == "HRA domestic violence shelters **" ~ total_single_adults + total_adults_on_families + total_children,
#                            agency_abb == "hpd" &
#                              category == "Census Total" ~ total_adults + total_children,
#                            T ~ NA
#          ),
#          date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d"),
#          table = "number of unduplicated individuals",
#          root = "ll37 historical") %>% 
#   filter(!is.na(count)) %>% 
#   select(agency_abb, date, count, table)
  
#ongoing question - should we add na.rm to these sums in case they miss data entry?

#new version

#MOCJ and DOHMH are not in the historical, they first appear here in May 2023

raw <- read.socrata("https://data.cityofnewyork.us/resource/jiwc-ncpi.csv")

unique_by_agency_new <- raw %>% 
  mutate(across(.cols = everything(), .fns = ~as.character(str_replace_all(.x, ",", "")))) %>% 
  mutate_at(vars(families_with_children:data_period), ~as.numeric(if_else(.x == "<10", "0", .x))) %>% 
  mutate(agency_abb = tolower(gsub("[()]", "", str_extract(agency, "\\([^)]+\\)"))),
         count = case_when(agency_abb == "dhs" & 
                             category == "Total number of individuals utilizing city-administered facilities" & 
                             facility_or_program_type == "DHS-administered facilities" ~ total_single_adults + total_adults_in_families + total_children,
                           agency_abb == "dycd" & 
                             category == "Total number of individuals utilizing city-administered facilities" & 
                             facility_or_program_type == "DYCD-administered facilities" ~ total_single_adults + total_adults_in_families + total_children,
                          
                           #do we remember why it's just domestic violence for HSA facilities and not emergency and transitional housing?
                           #there's also two conflicting categories for domestic violence shelters, one appears to just count families, the other individuals?
                           
                           agency_abb == "hra" & 
                             category == "Total number of individuals utilizing city-administered facilities" & 
                             facility_or_program_type == "HRA-administered facilities" | facility_or_program_type == "HRA-administered facilities 1" ~ total_single_adults + total_adults_in_families + total_children,
                           agency_abb == "hpd" & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "HPD-administered facilities" ~ total_single_adults + total_adults_in_families + total_children,
                           agency_abb == "dohmh" & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "Justice-informed supportive housing (JISH)" ~ total_single_adults,
                           
                           #not sure if we should keep these last two - as they are supportive housing, it seems, not shelter
                           
                           agency_abb == "dohmh" & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "Justice-informed supportive housing (JISH)" ~ total_single_adults,
                           agency_abb == "mocj" & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "Short-term reentry housing" ~ total_single_adults,
                           agency_abb == "oti" & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "Humanitarian Emergency Response and Relief Centers (HERRCs)"~ total,
                           T ~ NA
         ),
         date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d"),
         table = "number of unduplicated individuals",
         root = "ll79 new report") %>% 
  filter(!is.na(count)) %>% 
  select(agency_abb, date, count, table)

#current version
unique_by_agency <- read_csv("./data/ll79_data_unique_by_agency.csv",
                             col_names = T,
                             col_types = "cDnc")

latest_new_data_date <- max(unique_by_agency_new$date,
                            na.rm = T)

latest_old_data_date <- max(unique_by_agency$date,
                            na.rm = T)

if (latest_new_data_date > latest_old_data_date) {
  # Write to disk if new data
  unique_by_agency_new %>%
    arrange(desc(date), agency_abb) %>% 
    write_csv("./data/ll79_data_unique_by_agency.csv")
}


## TODO: Change `table` column to `series(?)` and have it be
## "number of unduplicated individuals" for all values except
## hra which will break out "DV" and "HASA" individuals

## add `root` column in final `select()` function to keep
## in final dataset.




  



