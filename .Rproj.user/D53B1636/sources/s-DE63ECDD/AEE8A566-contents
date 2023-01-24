library(pdftools)
library(stringr)
library(dplyr)
library(purrr)

# TODO: Write automated pdf downloader with curl() or download.file() with
# url: https://www1.nyc.gov/assets/dhs/downloads/pdf/dailyreport.pdf
pdf_text("dailyreport.pdf") %>% 
  nth(1) -> daily_report

date <- str_extract(daily_report, "\\w+\\s\\d{1,2},\\s+\\d{4}") %>% 
  strptime(format = "%B %d, %Y", tz = "UTC")

all_lines <- unlist(str_split(daily_report, pattern = "\n+"))

detect_index(all_lines, ~str_detect(.x, "Clients Served")) -> line_one_idx
detect_index(all_lines, ~str_detect(.x, "SHELTER CENSUS\\)")) -> line_two_idx

single_adults_and_family_intake <- all_lines[line_one_idx:line_two_idx]

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


