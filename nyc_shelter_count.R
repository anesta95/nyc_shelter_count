library(pdftools)
library(stringr)
library(dplyr)
library(purrr)
library(tabulizer)
library(janitor)
library(stringi)
library(tidyr)
library(readr)
library(RSocrata)
library(ggplot2)

# Reading partial DHS data from Socrata Open NYC database:
dhs_census_socrata_new <- read.socrata("https://data.cityofnewyork.us/resource/3pjg-ncn9.json") %>% 
  pivot_longer(cols = -date_of_census, names_to = "measure", values_to = "count") %>% 
  mutate(table = "DHS daily census",
         count = as.numeric(count),
         date_of_census = base::as.Date(date_of_census))

# check main dataset for date of latest data
dhs_census_socrata <- read_csv(file = "./data/dhs_daily_report_open_data_nyc_socrata.csv",
                               col_names = T,
                               col_types = "Dcdc")

latest_socrata_new_data_date <- max(dhs_census_socrata_new$date_of_census,
                            na.rm = T)

latest_socrata_old_data_date <- max(dhs_census_socrata$date_of_census,
                            na.rm = T)

if (latest_socrata_new_data_date > latest_socrata_old_data_date) {
  # Write to disk if new data
  write_csv(dhs_census_socrata_new, "./data/dhs_daily_report_open_data_nyc_socrata.csv")
}


# DHS daily report extra data from here
# url: https://www1.nyc.gov/assets/dhs/downloads/pdf/dailyreport.pdf

download.file(url = "https://www1.nyc.gov/assets/dhs/downloads/pdf/dailyreport.pdf",
              destfile = "./dhs_daily_report_unhoused_report_pdfs/temp_daily_report.pdf")

# Getting latest report text
daily_report <- pdf_text("./dhs_daily_report_unhoused_report_pdfs/temp_daily_report.pdf") %>%
  nth(1)

# Extracting the report date
report_date <- str_extract(daily_report, "\\w+\\s+\\d{1,2},\\s+\\d{4}") %>% 
  base::as.Date(format = "%B %d, %Y")

# Renaming pdf with its report date

file.rename(from = "./dhs_daily_report_unhoused_report_pdfs/temp_daily_report.pdf",
            to = paste0("./dhs_daily_report_unhoused_report_pdfs/", report_date, "_daily_report.pdf"))

latest_dhs <- extract_tables(paste0("./dhs_daily_report_unhoused_report_pdfs/", report_date, "_daily_report.pdf"))

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
    # Now just looking for the 11 row tibble since FAMILY INTAKE can show up in table 1 and two
    # and sometimes correct table has 5 columns or six columns
    idx <- detect_index(list, ~nrow(.x) == 11)
    
    matrix <- list[[idx]]
    
    safi_initial_df <- as.data.frame(matrix,
                                  stringsAsFactors = F)
    
    single_adults_col <- map_lgl(safi_initial_df, ~any(str_detect(.x, "SINGLE ADULTS"))) %>% 
                          which() %>% 
                          unname()
    family_intake_col <- map_lgl(safi_initial_df, ~any(str_detect(.x, "FAMILY INTAKE"))) %>% 
                         which() %>% 
                         unname()
    
    family_intake_row <- which(safi_initial_df[, family_intake_col] == "FAMILY INTAKE")
    single_adults_row <- which(safi_initial_df[, single_adults_col] == "SINGLE ADULTS")
    
    # Check to see if SINGLE ADULTS and FAMILY INTAKE column headers are on the same 
    # row. If not throw an error
    if (single_adults_row != family_intake_row) {
      stop(simpleError("SINGLE ADULTS and FAMILY INTAKE rows are not aligned."))
    }
    
    # Dynamic way to only select four columns we need.
    safi_cols <- c(single_adults_col, single_adults_col + 1, family_intake_col, family_intake_col + 1)
    
    safi_unselected <- safi_initial_df %>%
      slice(single_adults_row:nrow(.)) %>% 
      row_to_names(row_number = 1) %>% 
      clean_names()
    
    cleaned_safi <- safi_unselected[, safi_cols]

    names(cleaned_safi) <- c("single_adults", "single_adults_count", "family_intake", "family_intake_count")    
  
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
    df <- clean_tbls_3_6(table_name = table_name, list = list)
    
  }
  
  return(df)
  
}

dhs_unhoused_report_new <- map_dfr(table_names, ~extract_dhs_daily_data(.x, latest_dhs)) %>% 
  mutate(count = as.numeric(str_remove_all(count, ",")))

dhs_unhoused_report <- read_csv("./data/dhs_daily_report.csv",
                                col_names = T,
                                col_types = "cdcD")

latest_dhs_pdf_new_data_date <- max(dhs_unhoused_report_new$date,
                            na.rm = T)

latest_dhs_pdf_old_data_date <- max(dhs_unhoused_report$date,
                            na.rm = T)

if (latest_dhs_pdf_new_data_date > latest_dhs_pdf_old_data_date) {
  # Bind rows
  dhs_unhoused_report_full <- bind_rows(dhs_unhoused_report_new, dhs_unhoused_report)
  # Write to disk if new data
  write_csv(dhs_unhoused_report_full, "./data/dhs_daily_report.csv")
}



