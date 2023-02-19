library(RSocrata)
library(lubridate)

DHS_Census <- read.socrata("https://data.cityofnewyork.us/resource/3pjg-ncn9.json")

#-----------------------------------------------------------------------------------

DHS_report <- read.socrata("https://data.cityofnewyork.us/resource/2mqz-v5im.json")

DYCD_report <- read.socrata("https://data.cityofnewyork.us/resource/2232-dj5q.json")

HPD_report <- read.socrata("https://data.cityofnewyork.us/Housing-Development/Local-Law-37-HPD-Monthly-Shelter-Census-Report/mdht-5s6e")

HRA_report <- read.socrata("https://data.cityofnewyork.us/resource/e4ty-r26d.json")

DHS_report_clean <- DHS_report %>% 
  clean_names() %>% 
  filter(ll37_report_row_name == "Number of unduplicated persons: DHS-administered facilities") %>% 
  mutate_at(c("total_single_adults", "total_adults_in_families", "total_children"), as.numeric) %>% 
  mutate(total = total_single_adults + total_adults_in_families + total_children,
         agency = "Department of Homeless Services",
         date = ym(data_period))

DYCD_report_clean <- DYCD_report %>% 
  clean_names() %>% 
  filter(category == "number of unduplicated persons - DYCD-administered facilities") %>% 
  mutate_at(c("total_single_adults", "total_adults_in_families", "total_children"), as.numeric) %>% 
  mutate(total = total_single_adults + total_adults_in_families + total_children,
         agency = "DYCD",
         date = ym(data_period))
  



