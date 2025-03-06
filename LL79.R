library(RSocrata)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(janitor)
library(stringr)
library(httr)
library(tidyr)

DW_API <- Sys.getenv("DW_API_KEY")

## Function to republish chart via Datawrapper API ##
republish_chart <- function(API_KEY, chartID, data, subtitle = NULL, 
                            title = NULL, colors = NULL, 
                            tooltip = NULL, legend = NULL, 
                            axes = NULL, notes) {
  
  # PUT request to refresh data as per: https://developer.datawrapper.de/reference/putchartsiddata
  dataRefresh <- PUT(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                  chartID, "/data"),
                     add_headers(authorization = paste("Bearer", 
                                                       API_KEY, 
                                                       sep = " ")),
                     body = format_csv(data))
  
  call_back <- list(metadata = list())
  
  # This section adds chart title, subtitle, colors, tooltip, legend, and axes, if needed
  if (!is.null(title)) {
    call_back$title <- title
  }
  
  if (!is.null(subtitle)) {
    call_back$metadata$describe$intro <- subtitle   
  }
  
  if (!is.null(colors)) {
    call_back$metadata$visualize$`custom-colors` <- colors
  }
  
  if (!is.null(tooltip)) {
    call_back$metadata$visualize$tooltip <- tooltip
  }
  
  if (!is.null(legend)) {
    call_back$metadata$visualize$legend <- legend
  }
  
  if (!is.null(axes)) {
    call_back$metadata$axes <- axes
  }
  
  # Typically I always need to update the caption, but this can be 
  # moved to a conditional
  call_back$metadata$annotate$notes <- notes
  
  # PATCH request to update chart properties as per
  # https://developer.datawrapper.de/reference/patchchartsid
  notesRes <- PATCH(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                 chartID),
                    add_headers(authorization = paste("Bearer", API_KEY, 
                                                      sep = " ")),
                    body = call_back,
                    encode = "json")
  
  # POST request to republish chart
  # https://developer.datawrapper.de/reference/postchartsidpublish
  publishRes <- POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", 
                 chartID, "/publish"),
    add_headers(authorization = paste("Bearer", 
                                      API_KEY, 
                                      sep = " "))
  )
  
  list(dataRefresh, notesRes, publishRes) -> resList
  
  # Check for errors
  if (any(map_lgl(resList, http_error))) {
    which(map_lgl(resList, http_error))[1] -> errorIdx
    
    stop_for_status(resList[[errorIdx]], task = paste0("update step ",
                                                       errorIdx, 
                                                       " of chart ", 
                                                       chartID))
    
  } else {
    message(paste0("Chart ", chartID, " updated successfully"))
  }
  
}


#-----------------------------------------------------------------------------------

#Local law 37 datasets

#historical data - I think we only need to do this once?
# unique_by_agency_37 <- read.socrata("https://data.cityofnewyork.us/resource/bdft-9t6c.csv") %>%
#   group_by(agency, data_period) %>%
#   filter((!(agency == "Human Resources Administration (HRA)" & category == "Census")) &
#            (!(agency == "Department of Homeless Services (DHS)" & category != "Number of unduplicated persons")) &
#            (!(agency == "Department of Homeless Services (DHS)" & str_detect(facility_or_program_type, "HRA")))) %>%
#   mutate(agency_abb = tolower(gsub("[()]", "", str_extract(agency, "\\([^)]+\\)"))),
#          count = case_when(agency_abb == "dhs" &
#                              category == "Number of unduplicated persons" &
#                              facility_or_program_type == "DHS-administered facilities" ~ sum(single_men, na.rm = T) + sum(single_women, na.rm = T) + total_adults_on_families + total_children,
#                            agency_abb == "dycd" &
#                              category == "number of unduplicated persons - DYCD-administered facilities" ~ total_single_adults + total_adults_on_families + total_children,
# 
#                            #do we remember why it's just domestic violence for HRA facilities and not emergency and transitional housing?
#                            agency_abb == "hra" &
#                              category == "Number of unduplicated persons" & (facility_or_program_type %in% c("HRA domestic violence shelters **", "HRA domestic violence shelters **(Data)"))~ sum(total_single_adults, na.rm = T) + sum(total_adults_on_families, na.rm = T) + sum(total_children, na.rm = T),
#                            agency_abb == "hpd" &
#                              category == "Census Total" ~ total_adults + total_children,
#                            T ~ NA
#          ),
#          date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d"),
#          table = "number of unduplicated individuals",
#          root = "ll37 historical") %>%
#   filter(!is.na(count)) %>%
#   ungroup() %>%
#   select(agency_abb, date, count, table, root) %>%
#   filter(date >= as.Date("2019-01-01"))
# 
# write_csv(unique_by_agency_37 %>% arrange(date), "./data/ll37_data_unique_by_agency.csv")

#new version

#MOCJ and DOHMH are not in the historical, they first appear here in May 2023

unique_by_agency_new <- read.socrata("https://data.cityofnewyork.us/resource/jiwc-ncpi.csv") %>% 
  mutate(across(.cols = everything(), .fns = ~as.character(str_replace_all(.x, ",|#", "")))) %>% 
  mutate_at(vars(families_with_children:data_period), ~as.numeric(if_else(.x == "<10", "0", .x))) %>% 
  mutate_at(vars(category:facility_or_program_type), ~str_trim(str_replace_all(.x, "[0-9]", ""), side = "both")) %>% 
  filter(category == "Total number of individuals utilizing city-administered facilities") %>% 
  mutate(data_period = if_else(data_period == 202301 & agency == "Department of Youth and Community Development (DYCD)", 202401, data_period)) %>% #fix year typo
  group_by(agency, data_period) %>% 
  mutate(agency_abb = tolower(gsub("[()]", "", str_extract(agency, "\\([^)]+\\)"))),
         count = case_when(agency_abb == "dhs" & 
                             category == "Total number of individuals utilizing city-administered facilities" & 
                             facility_or_program_type == "DHS-administered facilities" ~ sum(single_men, na.rm = T) + sum(single_women, na.rm = T) + sum(anyone_of_another_gender, na.rm = T) + total_adults_in_families + total_children,
                           agency_abb == "dycd" & 
                             category == "Total number of individuals utilizing city-administered facilities" & 
                             facility_or_program_type == "DYCD-administered facilities" ~ total_single_adults + total_adults_in_families + total_children,
                          
                           #do we remember why it's just domestic violence for HSA facilities and not emergency and transitional housing?
                           #there's also two conflicting categories for domestic violence shelters, one appears to just count families, the other individuals?
                           
                           agency_abb == "hra" & 
                             category == "Total number of individuals utilizing city-administered facilities" & 
                             facility_or_program_type == "HRA-administered facilities" | facility_or_program_type == "HRA-administered facilities 1" ~ single_men + single_women + anyone_of_another_gender,
                           agency_abb == "hpd" & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "HPD-administered facilities" ~ total_single_adults + total_adults_in_families + total_children,
                           #JISH is not shelter!
                           # agency_abb == "dohmh" & 
                           #   category == "Total number of individuals utilizing city-administered facilities" &
                           #   facility_or_program_type == "Justice-informed supportive housing (JISH)" ~ total_single_adults,
                           agency_abb == "mocj" & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "Short-term reentry housing" ~ total_single_adults,
                           (agency_abb == "oti"|agency =="Office of Technology & Innovation"|agency=="Office of Technology and Innovation") & 
                             category == "Total number of individuals utilizing city-administered facilities" &
                             facility_or_program_type == "Humanitarian Emergency Response and Relief Centers (HERRCs)"~ total,
                           T ~ NA
         ),
         date = base::as.Date(paste0(data_period, "01"), format = "%Y%m%d"),
         table = "number of unduplicated individuals",
         root = "ll79 new report",
         agency_abb = if_else((agency_abb == "oti"|agency =="Office of Technology & Innovation"|agency == "Office of Technology and Innovation"), "herrcs", agency_abb)) %>% 
  filter(!is.na(count)) %>% 
  ungroup() %>% 
  select(agency_abb, date, count, table, root) %>% 
  arrange(date, agency_abb)

#current version

unique_by_agency_old <- read_csv("./data/ll79_data_unique_by_agency.csv",
                             col_names = T,
                             col_types = "cDncc")

latest_new_data_date <- max(unique_by_agency_new$date,
                            na.rm = T)

latest_old_data_date <- max(unique_by_agency_old$date,
                            na.rm = T)

if (latest_new_data_date > latest_old_data_date) {
  # Write to disk if new data
  unique_by_agency_historical <- read_csv("./data/ll37_data_unique_by_agency.csv",
                                          col_names = T,
                                          col_types = "cDncc")
    
  #ensure all series are present in new data, if not, throw error
  stopifnot(unique_by_agency_new %>% filter(date ==latest_new_data_date) %>% nrow() == 6)
  
  unique_by_agency <- unique_by_agency_new %>%
    bind_rows(unique_by_agency_historical) %>% 
    arrange(desc(date), agency_abb)
  
  total <- floor(sum(filter(unique_by_agency_new, date==as.Date(latest_new_data_date))$count)/1000)
  
  unique_by_agency %>% 
    write_csv("./data/ll79_data_unique_by_agency.csv")
  
  unique_by_agency_DW <- unique_by_agency %>% 
    mutate(agency_abb = toupper(agency_abb)) %>% 
    distinct() %>% #there is a duplicated row in the city data from 2024-09-01
    pivot_wider(names_from = agency_abb, values_from = count) %>% 
    select(-table, -root) %>% 
    mutate(DHS = if_else(is.na(DHS) & date == "2021-09-01", round((lag(DHS)+lead(DHS))/2), DHS)) %>%  #impute value
    mutate(DHS = if_else(date == "2019-10-01", round((lag(DHS)+lead(DHS))/2), DHS)) %>%  #correct Oct. '19 DHS undercount value
    mutate(HRA = if_else(date == "2019-05-01", 8438, HRA)) %>%  #correct HRA type value
    .[,c("date", "DHS", "HRA", "DYCD", "HPD", "MOCJ", "HERRCS")]
  
  republish_chart(API_KEY = DW_API, chartID = "2CO79", 
                  data = unique_by_agency_DW, 
                  subtitle = paste0(
                    "NYC Shelters counted approximately ",
                    format(total, big.mark = ","),
                    "K people in latest reporting month, organized here by agency/system"
                  ),
                  notes = paste0(
                    "Chart reflects most recent LL79 report dated ",
                    format(
                      max(
                        unique_by_agency$date, na.rm = T), 
                      "%m/%Y"), "." 
                  ))
  
  
}

