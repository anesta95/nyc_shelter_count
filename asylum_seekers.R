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
library(lubridate)
library(stringr)

# Monthly asylum seekers reports here:
# url: https://council.nyc.gov/budget/wp-content/uploads/sites/54/2023/08/Asylum-Seekers-Report-July-2023.pdf


report_month <- month(Sys.Date() %m-% months(1), label = T, abbr = F) #gets the previous month
report_year <- year(Sys.Date() %m-% months(1))

current_month_abb <- str_pad(as.character(month(Sys.Date())), 2, "left", "0")
current_year <- year(Sys.Date())

download.file(url = paste0("https://council.nyc.gov/budget/wp-content/uploads/sites/54/",
                           current_year,
                           "/",
                           current_month_abb,
                           "/Asylum-Seekers-Report-",
                           report_month,
                           "-",
                           report_year,
                           ".pdf"),
              destfile = "./asylum_seekers_report_pdfs/temp_monthly_report.pdf",
              mode = "wb")


# Getting latest report text
asylum_report <- pdf_text("./asylum_seekers_report_pdfs/temp_monthly_report.pdf") %>%
  nth(1)

asylum_report_date <- str_extract(daily_report, "\\w+\\s+\\d{1,2},\\s+\\d{4}") %>% 
  base::as.Date(format = "%B %d, %Y")

latest_dhs <- extract_tables("./asylum_seekers_report_pdfs/temp_monthly_report.pdf")



# Extracting the report date
report_date <- str_extract(daily_report, "\\w+\\s+\\d{1,2},\\s+\\d{4}") %>% 
  base::as.Date(format = "%B %d, %Y")

#@adrian - here I conditionally change the report date to avoid typos that would stop the script from running if the report date is in the future
### This works! I just added an additional condition to check if the date wasn't able to be parsed. 
### I think the logic of just using the previous date from the Sys.Date() as the assumed
### date in these cases instead of just the next date (or weekdate) from the datafile
### we already have makes sense since it seems like a new report isn't *actually* published every day.
if (report_date > Sys.Date() | is.na(report_date)) {
  report_date <- Sys.Date() - 1
}

# Renaming pdf with its report date

file.rename(from = "./dhs_daily_report_unhoused_report_pdfs/temp_daily_report.pdf",
            to = paste0("./dhs_daily_report_unhoused_report_pdfs/", report_date, "_daily_report.pdf"))