#DONT RUN THIS AGAIN! I TOOK OUT THE BAD DATES

# dhs_unhoused_report <- read_csv("./data/dhs_daily_report.csv",
#                               col_names = T,
#                               col_types = "cdcD")%>%
#   filter(date <= as.Date("2023-07-01")) %>% 
#   write_csv("./data/dhs_daily_report.csv")

keepdates <- seq(as.Date("2023-07-02"), as.Date("2023-09-12"), by="days")

files <- list.files(path = "dhs_daily_report_unhoused_report_pdfs")

filtered_dates <- keepdates[keepdates %in% str_sub(files,0,10)]

# this needs the functions loaded from nyc_shelter_count to run
# libraries too
fill_in_report <- function(report_date) {
  
dhs_unhoused_report <- read_csv("./data/dhs_daily_report.csv",
                                  col_names = T,
                                  col_types = "cdcD")

latest_dhs <- extract_tables(paste0("./dhs_daily_report_unhoused_report_pdfs/", report_date, "_daily_report.pdf"))

dhs_unhoused_report_new <- map_dfr(table_names, ~extract_dhs_daily_data(.x, latest_dhs)) %>% 
  mutate(count = as.numeric(str_remove_all(count, ",")))

dhs_unhoused_report_full <- bind_rows(dhs_unhoused_report_new, dhs_unhoused_report)
# Write to disk if new data
write_csv(dhs_unhoused_report_full, "./data/dhs_daily_report.csv")

}

walk(filtered_dates, .f = ~fill_in_report(.x))

