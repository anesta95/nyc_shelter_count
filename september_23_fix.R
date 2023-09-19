#DONT RUN THIS AGAIN! I TOOK OUT THE BAD DATES

# dhs_unhoused_report_7.4.23 <- read_csv("./data/dhs_daily_report.csv",
#                               col_names = T,
#                               col_types = "cdcD") %>%
#   filter(date <= as.Date("2023-07-01"))
#   
# write_csv(dhs_unhoused_report_7.4.23, "./data/dhs_daily_report.csv")

keepdates <- seq(as.Date("2023-07-02"), as.Date("2023-09-12"), by="days")

files <- list.files(path = "dhs_daily_report_unhoused_report_pdfs")

filtered_dates <- keepdates[keepdates %in% str_sub(files,0,10)]

table_names <- c("FAMILY INTAKE", "TOTAL SHELTER CENSUS",
                 "Total Single Adults", "FAMILIES WITH CHILDREN",
                 "ADULT FAMILIES CENSUS")

# this needs the functions loaded from nyc_shelter_count to run
# libraries too
fill_in_report <- function(report_date, tables) {
  
# dhs_unhoused_report <- read_csv("./data/dhs_daily_report.csv",
#                                   col_names = T,
#                                   col_types = "cdcD")
# 
# print(dhs_unhoused_report)

latest_dhs <- extract_tables(paste0("./dhs_daily_report_unhoused_report_pdfs/", report_date, "_daily_report.pdf"))

# print(latest_dhs)
report_date_dte <- base::as.Date(report_date)

#this fails - I think because we can't pass report_date down to the other nested functions that need it
#since it's not a global in this script like it is in the main script
dhs_unhoused_report_new <- map_dfr(tables, ~extract_dhs_daily_data(.x, latest_dhs, report_date_dte)) %>% 
  mutate(count = as.numeric(str_remove_all(count, ",")))

message(paste("Done with date", report_date))
return(dhs_unhoused_report_new)



# dhs_unhoused_report_full <- bind_rows(dhs_unhoused_report_new, dhs_unhoused_report)
# Write to disk if new data
# write_csv(dhs_unhoused_report_full, "./data/dhs_daily_report1.csv")

}

fill_in_report(report_date = "2023-07-02", tables = table_names)

missing_shelter_cnt_daily <- map_dfr(filtered_dates, .f = ~fill_in_report(.x, tables = table_names))

missing_may_7 <- tibble(measure = c("Drop-in Center Clients Served",
                   "Drop-in Center Overnight Census",
                   "Faith Bed Census",
                   "Outreach Contacts",
                   "Outreach Placements",
                   "Safe Haven Utilization",
                   "Veterans In Short-term Housing",
                   "Criminal Justice Short-term Housing",
                   "Families with Children Requesting Temporary Housing",
                   "Adult Families Requesting Temporary Housing",
                   "Families Placed in Overnight Accommodations",
                   "Families w/children at PATH Overnight (pre 10PM)",
                   "Adults",
                   "Children",
                   "Total Individuals",
                   "Men",
                   "Women",
                   "Total Single Adults",
                   "Families",
                   "Adults",
                   "Children",
                   "Individuals",
                   "Families",
                   "Individuals (Adults)"),
       table = c(
         "single_adults",
         "single_adults",
         "single_adults",
         "single_adults",
         "single_adults",
         "single_adults",
         "single_adults",
         "single_adults",
         "family_intake",
         "family_intake",
         "family_intake",
         "family_intake",
         "total_shelter_census",
         "total_shelter_census",
         "total_shelter_census",
         "total_single_adults",
         "total_single_adults",
         "total_single_adults",
         "families_with_children",
         "families_with_children",
         "families_with_children",
         "families_with_children",
         "adult_families_census",
         "adult_families_census"
       ),
       date = rep(base::as.Date("2023-05-07"), 24),
       count = c(
         400,
         157,
         27,
         280,
         15,
         1509,
         133,
         155,
         103,
         25,
         0,
         0,
         52777,
         25796,
         78573,
         18069,
         4589,
         22658,
         15165,
         23741,
         25796,
         49537,
         3015,
         6378
       ))

missing_shelter_cnt_daily_total <- bind_rows(
  missing_shelter_cnt_daily,
  missing_may_7
)

dhs_unhoused_report <- read_csv("./data/dhs_daily_report.csv",
                                  col_names = T,
                                  col_types = "cdcD")

dhs_unhoused_report_recombined <- bind_rows(dhs_unhoused_report,
                                            missing_shelter_cnt_daily_total) %>% 
  arrange(desc(date))

write_csv(dhs_unhoused_report_recombined, "./data/dhs_daily_report.csv")


