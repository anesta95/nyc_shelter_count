library(readr)
library(tidyr)
library(tabulapdf)
library(janitor)
library(lubridate)
library(xlsx)
library(purrr)
library(pdftools)
library(stringr)
library(dplyr)

current_date <- Sys.Date()

months <-  seq(as.Date("2023-05-01"), current_date %m-% months(2), by = "1 month") %>%
  data.frame(month_year = format(., "%m_%Y"))

old_months <- read_csv("data/shelter_exits.csv") %>% pull(report_date) %>% unique()

read_table <- function(table, agency_name) {
  as.data.frame(table) %>% 
    remove_empty() %>% 
    mutate(across(everything(), ~ifelse(row_number() == 1, gsub("[0-9]", "", .), .))) %>% 
    row_to_names(1) %>% 
    clean_names() %>% 
    mutate(agency = agency_name)
}

read_report <- function(month) {
  agency_names <- c("DHS", "HPD", "HRA", "DYCD")
  
  if(month %in% months$month_year[0:3]) {
    report <- extract_tables(paste0("./temporary_housing_report_pdfs/temporary_housing_report_", month, ".pdf"),
                             pages = c(7:10))
  } else {
    report <- extract_tables(paste0("./temporary_housing_report_pdfs/temporary_housing_report_", month, ".pdf"),
                             pages = c(8:11))
  }
  
  result <- map2_df(.x = report, .y=agency_names, ~read_table(.x, .y)) %>% 
    mutate(period = month)
  
  return(result)
  
}

if(length(months$month_year) > length(old_months)) { #if there is a new month run the whole thing and overwrite file

all_months <- map_df(months$month_year, ~read_report(.x)) #need to redo all months because of the 2 month lag

field_categorization <- all_months %>% 
  count(facility_or_program_type)

field_categorization %>% xlsx::write.xlsx("docs/field_names_categorization.xlsx")

field_validation <- xlsx::read.xlsx("docs/field_names_validated.xlsx", sheetIndex = 1) %>% 
  select(facility_or_program_type, category, housing_category, notes)

#need to lag families 1 month and lag single adults 2 months for DHS.
shelter_exits_clean <- all_months %>% 
  select(-starts_with("x")) %>% 
  mutate(across(.cols = -facility_or_program_type, .fns = ~as.character(str_replace_all(.x, ",|#", "")))) %>% 
  mutate_at(vars(families_with_children:total_single_adults), ~as.numeric(if_else(.x == "<10", "0", .x))) %>% #replace under 10 with 0
  mutate_at(vars(facility_or_program_type), ~str_trim(str_replace_all(.x, "[0-3]$", ""), side = "both")) %>%  #sometimes there are footnotes - can't be more than 8 or we lose S8
  left_join(months, by = c("period"="month_year")) %>% 
  rename("date" = ".",
         "report_date" = "period") %>% 
  pivot_longer(cols = families_with_children:total_single_adults, names_to = "series", values_to = "exits") %>% 
  mutate(date = case_when(
    agency == "DHS" & (series == "families_with_children" | series == "adult_families") ~ date-months(1),
    agency == "DHS" & series == "total_single_adults" ~ date-months(2),
    T ~ date
  )) %>% 
  left_join(field_validation, by = "facility_or_program_type") %>% 
  select(-data_period)

write_csv(shelter_exits_clean, "data/shelter_exits.csv")

#############################################################################
} else {
  print("no new data")
}
