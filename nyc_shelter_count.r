library(pdftools)
library(stringr)
library(dplyr)
library(purrr)
library(tabulizer)
library(janitor)
library(stringi)
library(tidyr)


# TODO: Write automated pdf downloader with curl() or download.file() with
# url: https://www1.nyc.gov/assets/dhs/downloads/pdf/dailyreport.pdf
pdf_text("dailyreport.pdf") %>%
  nth(1) -> daily_report

date <- str_extract(daily_report, "\\w+\\s+\\d{1,2},\\s+\\d{4}") %>% 
  strptime(format = "%B %d, %Y", tz = "UTC")

extract_tables("dailyreport.pdf") -> latest_dhs

detect_index(latest_dhs, ~all(dim(.x) == c(11, 5))) -> single_adults_family_intake_index

latest_dhs[[single_adults_family_intake_index]] -> single_adults_family_intake_matrix

as.data.frame(single_adults_family_intake_matrix,
              stringsAsFactors = F) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename(single_adults_count = x, sites_reported = x_2, family_intake_count = x_3) -> cleaned_safi 

if (all(names(cleaned_safi)[c(1, 4)] == c("single_adults", "family_intake"))) {
  cj_count <- cleaned_safi %>% 
    pull(single_adults_count) %>% 
    stri_remove_empty() %>% 
    last()
  
  sa_no_cj <- cleaned_safi %>% 
    slice(1:7) %>% 
    select(single_adults, single_adults_count)
  
  sa_cj <- sa_no_cj %>% 
    bind_rows(tibble(single_adults = "Criminal Justice Short-term Housing",
                     single_adults_count = cj_count))
  
  fi_no_cj <- cleaned_safi %>% 
    slice(1:4) %>% 
    select(family_intake, family_intake_count)
  

}

detect_index(latest_dhs, ~any(str_detect(.x, "TOTAL SHELTER CENSUS"))) -> tsc_index

latest_dhs[[tsc_index]] -> tsc_matrix

as.data.frame(tsc_matrix,
              stringsAsFactors = F) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  select(total_shelter_census) %>% 
  separate(col = total_shelter_census, into = c("total_shelter_census",
                                                "tsc_count"),
           sep = "\\s+(?=\\d)") %>% 
  mutate(tsc_count = as.numeric(str_remove_all(tsc_count, ","))) -> cleaned_tsc 




# all_lines <- unlist(str_split(daily_report, pattern = "\n+"))
# 
# detect_index(all_lines, ~str_detect(.x, "Clients Served")) -> line_one_idx
# detect_index(all_lines, ~str_detect(.x, "SHELTER CENSUS\\)")) -> line_two_idx
# 
# single_adults_and_family_intake <- all_lines[line_one_idx:line_two_idx]

# TODO: Write function to determine if line has both SINGLE ADULTS and
# FAMILY INTAKE table data in it and if it does, split out the two data points

# line_parser <- function(str) {
#   
#   first_word <- str_detect(string = str, pattern = "Families")
#   
#   if ()
#   
#   
# }

# TODO: Write function that organizes SINGLE ADULTS and FAMILY INTAKE data into tibble

# TODO: Write function that organizes TOTAL SHELTER CENSUS, SINGLE ADULTS, FAMILIES WITH CHILDREN, and ADULT FAMILES CENSUS
# summary tables into one tibble

# TODO: Compare data from NYC Open Data link, accessible with Socrata API: https://data.cityofnewyork.us/Social-Services/DHS-Daily-Report/k46n-sa2m


