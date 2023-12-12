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
library(httr)

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



# Reading partial DHS data from Socrata Open NYC database:
dhs_census_socrata_new <- read.socrata("https://data.cityofnewyork.us/resource/3pjg-ncn9.json") %>% 
  pivot_longer(cols = -date_of_census, names_to = "measure", values_to = "count") %>% 
  mutate(table = "DHS daily census",
         count = as.numeric(count),
         date_of_census = base::as.Date(date_of_census)) %>% 
  arrange(desc(date_of_census))

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
  
  ### Updating Visualization ###
  ## DHS daily report from socrata families with children line graph (bK11f)
  
  dhs_d_families_w_children <- dhs_census_socrata_new %>% 
    filter(measure == "families_with_children_in_shelter") %>% 
    select(date_of_census, count) %>% 
    rename(families_with_children_in_shelter = count)
  
  republish_chart(API_KEY = DW_API, chartID = "bK11f", 
                  data = dhs_d_families_w_children, 
                  notes = paste0(
                    "Chart reflects most recent data published by the NYC Department of Homeless Services.",
                    " Data current as of ", format(
                      max(
                        dhs_d_families_w_children$date_of_census, na.rm = T), 
                      "%m/%d/%Y"), "." 
                  ))

}


# DHS daily report extra data from here
# url: https://www1.nyc.gov/assets/dhs/downloads/pdf/dailyreport.pdf

# download.file(url = "https://www1.nyc.gov/assets/dhs/downloads/pdf/dailyreport.pdf",
#               destfile = "./dhs_daily_report_unhoused_report_pdfs/temp_daily_report.pdf")

# Getting latest report text
daily_report <- pdf_text("./dhs_daily_report_unhoused_report_pdfs/temp_daily_report.pdf") %>%
  nth(1)

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

latest_dhs <- extract_tables(paste0("./dhs_daily_report_unhoused_report_pdfs/", report_date, "_daily_report.pdf"))

table_names <- c("FAMILY INTAKE", "TOTAL SHELTER CENSUS",
                 "Total Single Adults", "FAMILIES WITH CHILDREN",
                 "ADULT FAMILIES CENSUS")

clean_tbls_3_6 <- function(table_name, list, report_date) {
  
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
      select(all_of(col_name)) %>% 
      separate(col = col_name, into = c("measure", "count"),
               sep = "\\s+(?=\\d)") %>% 
      mutate(table = make_clean_names(table_name),
             date = report_date)
  }
  
  return(df)
  
}


extract_dhs_daily_data <- function(table_name, list, report_date) {
  
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
      select(c(single_adults, single_adults_count))
    
    sa_cj <- sa_no_cj %>% 
      bind_rows(tibble(single_adults = "Criminal Justice Short-term Housing",
                       single_adults_count = cj_count)) %>% 
      rename(measure = single_adults, count = single_adults_count) %>% 
      mutate(table = "single_adults", date = report_date)
    
    fi_no_sa <- cleaned_safi %>% 
      slice(1:4) %>% 
      select(c(family_intake, family_intake_count)) %>% 
      rename(measure = family_intake, count = family_intake_count) %>% 
      mutate(table = "family_intake", date = report_date)
    
    df <- bind_rows(sa_cj, fi_no_sa)
      
    
  } else {
    df <- clean_tbls_3_6(table_name = table_name, list = list, report_date = report_date)
    
  }
  
  return(df)
  
}

dhs_unhoused_report_new <- map_dfr(table_names, ~extract_dhs_daily_data(.x, latest_dhs, report_date)) %>% 
  mutate(count = as.numeric(str_remove_all(count, ",")))

# Adding in aggregated single adult and total individuals rows

extra_single_adults <- filter(dhs_unhoused_report_new, 
                              (table == "single_adults" &
                                 !(measure %in% c("Drop-in Center Clients Served",
                                                  "Criminal Justice Short-term Housing",
                                                  "Outreach Contacts",
                                                  "Outreach Placements"))
                               )
                              ) %>% 
  pull(count) %>% 
  sum(., na.rm = T)

dhs_aggregated_rows <- tibble(
  measure = c("Single Adults", "Total Individuals"),
  count = c(
    (filter(dhs_unhoused_report_new, measure == "Total Single Adults")$count + 
       extra_single_adults),
    (extra_single_adults + 
       filter(dhs_unhoused_report_new, measure == "Total Individuals")$count)
  ),
  table = c("combined_total_single_adults", "combined_total_shelter_census"),
  date = unique(dhs_unhoused_report_new$date)
)


dhs_unhoused_report_new_combo <- bind_rows(dhs_unhoused_report_new,
                                           dhs_aggregated_rows)

dhs_unhoused_report <- read_csv("./data/dhs_daily_report.csv",
                                col_names = T,
                                col_types = "cdcD")

latest_dhs_pdf_new_data_date <- max(dhs_unhoused_report_new_combo$date,
                            na.rm = T)

latest_dhs_pdf_old_data_date <- max(dhs_unhoused_report$date,
                            na.rm = T)

if (latest_dhs_pdf_new_data_date > latest_dhs_pdf_old_data_date) {
  # Bind rows
  dhs_unhoused_report_full <- bind_rows(dhs_unhoused_report_new_combo, dhs_unhoused_report)
  # Write to disk if new data
  write_csv(dhs_unhoused_report_full, "./data/dhs_daily_report.csv")
  
  ### Updating Visualization ###
  ## DHS daily total shelter population line graph (UmiCQ)
  
  dhs_d_total_individuals_dw <- dhs_unhoused_report_full %>% 
    filter(table == "combined_total_shelter_census" & measure == "Total Individuals") %>% 
    select(date, count)
  
  republish_chart(API_KEY = DW_API, chartID = "UmiCQ", 
                  data = dhs_d_total_individuals_dw, 
                  notes = paste0(
                    "These totals include SafeHaven shelters, overnight drop-in centers, veterans shelters, and faith-based shelters as well as shelters for single adults, families with children and adult families. Some daily totals are missing because DHS does not report certain shelter types every day.",
                    " Data current as of ", format(
                      max(
                        dhs_d_total_individuals_dw$date, na.rm = T), 
                      "%m/%d/%Y"), "." 
                  ))
  
  ### Updating Visualization ###
  ## DHS daily program breakout line graph (zVEuB)
  
  # Dates and series to remove outliers
  # Savehaven: 
  # * 2022-06-13
  # * 2022-09-11
  # * 2022-09-18
  # Drop-in overnight:
  # * 2022-12-07
  
  dhs_d_program_dw <- dhs_unhoused_report_full %>% 
    filter(measure %in% c("Criminal Justice Short-term Housing",
                          "Drop-in Center Overnight Census",
                          "Faith Bed Census",
                          "Safe Haven Utilization",
                          "Veterans In Short-term Housing")) %>% 
    select(-table) %>% 
    pivot_wider(names_from = measure, values_from = count) %>% 
    rename(`Drop-in Overnight` = `Drop-in Center Overnight Census`,
           `Faith Bed` = `Faith Bed Census`,
           SafeHaven = `Safe Haven Utilization`,
           Veterans = `Veterans In Short-term Housing`)
  
  # Removing data outlier probable errors for the viz
  dhs_d_program_dw[which(dhs_d_program_dw$date %in% c(base::as.Date("2022-06-13"),
                                     base::as.Date("2022-09-11"),
                                     base::as.Date("2022-09-18"))), "SafeHaven"] <- NA
  
  dhs_d_program_dw[which(dhs_d_program_dw$date == base::as.Date("2022-12-07")), 
                   "Drop-in Overnight"] <- NA
  
  republish_chart(API_KEY = DW_API, chartID = "zVEuB", 
                  data = dhs_d_program_dw, 
                  notes = paste0(
                    "DHS does not publish a report every day, despite a legal mandate. DHS does not report the number of people staying in its stabilization beds.",
                    " Data current as of ", format(
                      max(
                        dhs_d_program_dw$date, na.rm = T), 
                      "%m/%d/%Y"), "." 
                  ))
  
  ### Updating Visualization ###
  ## DHS daily family composition breakout line graph (0omhO)
  
  dhs_d_fam_comp_dw <- dhs_unhoused_report_full %>% 
    filter(measure %in% c("Single Adults",
                          "Children",
                          "Adults",
                          "Individuals (Adults)")) %>% 
    filter(!(measure == "Children" & table == "total_shelter_census")) %>% 
    filter(!(measure == "Adults" & table == "total_shelter_census")) %>% 
    select(-table) %>% 
    pivot_wider(names_from = measure, values_from = count) %>% 
    rename(`Adults with Children` = Adults,
           `Individuals in Adults Families` = `Individuals (Adults)`)
  
  republish_chart(API_KEY = DW_API, chartID = "0omhO", 
                  data = dhs_d_fam_comp_dw, 
                  notes = paste0(
                    "DHS does not publish a report every day, despite a legal mandate. DHS does not report the number of people staying in its stabilization beds.",
                    " Data current as of ", format(
                      max(
                        dhs_d_fam_comp_dw$date, na.rm = T), 
                      "%m/%d/%Y"), "." 
                  ))
  
  
}

