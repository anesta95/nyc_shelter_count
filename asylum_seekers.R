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






##############################################################

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

asylum_report_date_string <- unlist(strsplit(asylum_report, "\n"))[3] %>% 
  str_trim(side = "left") %>% 
  str_replace(" ", "_")
  
asylum_report_date <- lubridate::my(asylum_report_date_string)

latest_asylum <- extract_tables("./asylum_seekers_report_pdfs/temp_monthly_report.pdf",
                                output = "data.frame",
                                pages = 3,
                                method = "decide")

aug_asylum <- extract_tables("./asylum_seekers_report_pdfs/Asylum-Seekers-Report-August-2023-1.pdf")

asylum_table_names <- c("ASYLUM SEEKER DAILY CENSUS")

df <- as.data.frame(latest_asylum[4])

aslyum_report_date_actual <- df[1, "X5"]

df_clean <- df %>%
  mutate(date = X5[1],
         table = X1[1]) %>% 
  filter(row_number()>2) %>% 
  separate(X1, sep = " (?=[0-9,])", into = c("category", "XX", "XY")) %>% 
  mutate(population = if_else(row_number()<6, "individuals", "families")) %>% 
  filter(!is.na(XX))

df_clean[1,]
unlist(df_clean[1, ])

names(df_clean) <- c("category", "DHS", "DYCD", "HPD", "H+H", "NYCEM", "total", "date", "table", "population")

df_long <- df_clean %>% 
  pivot_longer(c("DHS", "DYCD", "HPD", "H+H", "NYCEM", "total"), names_to = "agency", values_to = "count")

write_csv(df_long, "data/asylum_seekers_report.csv")

file.rename(from = "./asylum_seekers_report_pdfs/temp_monthly_report.pdf",
            to = paste0("./asylum_seekers_report_pdfs/", aslyum_report_date_actual, "_asylum_monthly_report.pdf"))
