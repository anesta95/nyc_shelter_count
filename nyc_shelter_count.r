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
daily_report <- pdf_text("dailyreport_2022-03-13.pdf") %>%
  nth(1) 

# Extracting the report date
report_date <- str_extract(daily_report, "\\w+\\s+\\d{1,2},\\s+\\d{4}") %>% 
  base::as.Date(format = "%B %d, %Y")

# TODO: This at times returns "ghost" columns with no data. Will have to write code to 
# filter them out.
latest_dhs <- extract_tables("dailyreport_2022-03-13.pdf")

table_names <- c("FAMILY INTAKE", "TOTAL SHELTER CENSUS",
                 "Total Single Adults", "FAMILIES WITH CHILDREN",
                 "ADULT FAMILIES CENSUS")

clean_tbls_3_6 <- function(table_name, list) {
  
  idx <- detect_index(list, ~any(str_detect(.x, table_name)))
  
  matrix <- list[[idx]]
  
  partial_df <- as.data.frame(matrix, stringsAsFactors = F) %>% 
                row_to_names(row_number = 1) %>% 
                clean_names()
  
  if (table_name == "Total Single Adults") {
    df <- partial_df %>% 
          rename(measure = x, count = single_adults) %>% 
          mutate(table = make_clean_names(table_name),
                 date = report_date)
    
  } else {
    col_name <- sym(make_clean_names(table_name))
    
    df <- partial_df %>% 
      select(col_name) %>% 
      separate(col = col_name, into = c("measure", "count"),
               sep = "\\s+(?=\\d)") %>% 
      mutate(table = make_clean_names(table_name),
             date = report_date)
  }
  
  return(df)
  
}


extract_dhs_daily_data <- function(table_name, list) {
  
  if (table_name == "FAMILY INTAKE") {
    # Note: Now grabbing by table dimentions. We need to decide most optimal way based on
    # returns from extract_tables()
    # idx <- detect_index(list, ~all(dim(.x) == c(11, 5)))
    # Now just looking for the 11 row tibble since FAMILY INTAKE can show up in table 1 and two
    # and sometimes correct table has 5 columns or six columns
    idx <- detect_index(list, ~nrow(.x) == 11)
    
    matrix <- list[[idx]]
    
    safi_initial_df <- as.data.frame(matrix,
                                  stringsAsFactors = F)
    
    single_adults_row <- which(safi_initial_df$V1 == "SINGLE ADULTS")
    family_intake_row <- which(safi_initial_df$V4 == "FAMILY INTAKE")
    
    ## TODO: This still needs some reworking based on the variable number of columns 
    ## the first table is returned with after extract_tables(). Might not need this check and check
    ## in next if block below for columns 1 & 4
    if (single_adults_row != family_intake_row) {
      stop(simpleError("SINGLE ADULTS and FAMILY INTAKE rows are not aligned."))
    }
    
  
    cleaned_safi <- safi_initial_df %>%
      slice(single_adults_row:nrow(.)) %>% 
      row_to_names(row_number = 1) %>% 
      clean_names() %>% 
      rename(single_adults_count = x, sites_reported = x_2, family_intake_count = x_3)
    
    ## Second check: is this needed?
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
                         single_adults_count = cj_count)) %>% 
        rename(measure = single_adults, count = single_adults_count) %>% 
        mutate(table = "single_adults", date = report_date)
      
      fi_no_sa <- cleaned_safi %>% 
        slice(1:4) %>% 
        select(family_intake, family_intake_count) %>% 
        rename(measure = family_intake, count = family_intake_count) %>% 
        mutate(table = "family_intake", date = report_date)
      
      df <- bind_rows(sa_cj, fi_no_sa)
      
    } else {
      stop(simpleError("SINGLE ADULTS and FAMILY INTAKE column placement misaligned"))
    }
    
  } else {
    df <- clean_tbls_3_6(table_name = table_name, list = list)
    
  }
  
  return(df)
  
}

map_dfr(table_names, ~extract_dhs_daily_data(.x, latest_dhs))


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


detect_index(latest_dhs, ~any(str_detect(.x, "Total Single Adults"))) -> sa_index

latest_dhs[[sa_index]] -> sa_matrix

as.data.frame(sa_matrix,
                stringsAsFactors = F) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename(sa_count = single_adults, single_adults = x) %>% 
  mutate(sa_count = as.numeric(str_remove_all(sa_count, ","))) -> cleaned_sa 


detect_index(latest_dhs, ~any(str_detect(.x, "FAMILIES WITH CHILDREN"))) -> fwc_index

latest_dhs[[fwc_index]] -> fwc_matrix

cleaned_fwc <- as.data.frame(fwc_matrix,
              stringsAsFactors = F) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  select(families_with_children) %>% 
  separate(col = families_with_children, into = c("families_with_children",
                                                "fwc_count"),
           sep = "\\s+(?=\\d)") %>% 
  mutate(fwc_count = as.numeric(str_remove_all(fwc_count, ","))) 


detect_index(latest_dhs, ~any(str_detect(.x, "ADULT FAMILIES CENSUS"))) -> afc_index

latest_dhs[[afc_index]] -> afc_matrix

cleaned_afc <- as.data.frame(afc_matrix,
                             stringsAsFactors = F) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  select(adult_families_census) %>% 
  separate(col = adult_families_census, into = c("adult_families_census",
                                                  "afc_count"),
           sep = "\\s+(?=\\d)") %>% 
  mutate(afc_count = as.numeric(str_remove_all(afc_count, ","))) 



# Old SINGLE ADULTS and FAMILY INTAKE table method
# detect_index(latest_dhs, ~any(str_detect(.x, "FAMILY INTAKE"))) -> single_adults_family_intake_index
# detect_index(latest_dhs, ~all(dim(.x) == c(11, 5))) -> single_adults_family_intake_index
# 
# latest_dhs[[single_adults_family_intake_index]] -> single_adults_family_intake_matrix
# 
# as.data.frame(single_adults_family_intake_matrix,
#               stringsAsFactors = F) %>% 
#   row_to_names(row_number = 1) %>% 
#   clean_names() %>% 
#   rename(single_adults_count = x, sites_reported = x_2, family_intake_count = x_3) -> cleaned_safi 
# 
# if (all(names(cleaned_safi)[c(1, 4)] == c("single_adults", "family_intake"))) {
#   cj_count <- cleaned_safi %>% 
#     pull(single_adults_count) %>% 
#     stri_remove_empty() %>% 
#     last()
#   
#   sa_no_cj <- cleaned_safi %>% 
#     slice(1:7) %>% 
#     select(single_adults, single_adults_count)
#   
#   sa_cj <- sa_no_cj %>% 
#     bind_rows(tibble(single_adults = "Criminal Justice Short-term Housing",
#                      single_adults_count = cj_count))
#   
#   fi_no_cj <- cleaned_safi %>% 
#     slice(1:4) %>% 
#     select(family_intake, family_intake_count)
#   
# 
# }



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


