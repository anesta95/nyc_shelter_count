# NYC Shelter Census & Tracker

New York City is facing a historic homelessness crisis. But getting data on how many people are using NYC shelters is complicated. This repository, built in collaboration with City Limits News, seeks to liberate all the available data on New York City's shelter population and publish the data together in one place.

You can view the project tracker and read more at [City Limits](https://citylimits.org/nyc-shelter-count/).

The repository creates several key datasets for understanding the shelter population in NYC:
1. A daily count of the population in Department of Homeless Services (DHS) shelter
2. A monthly count of the population of all city agency facilities
3. A monthly count of the population of asylum seekers

The data is sourced from city webpages and the open data portal
- Data on the daily population of DHS shelters comes from the DHS daily report PDFs posted to the city's website. This data also includes adults in specialized programs
- Data on the historic daily population of DHS shelters comes from the NYC open data portal
- Data on the monthly population in all city facilities comes from the Local Law 37 (historical) and Local Law 79 (current) temporary housing assistance usage reports published monthly as pdfs and with limited data available on NYC open data
- Data on the asylum seeking population is published in pdfs on the City Council Budget webpage

Additional data on shelter exits scraped from the city's monthly temporary housing report is available in a companion repository, [NYC Shelter Exits](https://github.com/pspauster/shelter-exits).

Daily DHS data is scraped from a pdf on the city's webpage each day and saved in this repository, then added to a time series dataset which automatically updates the tracker's charts hosted by City Limits. This pdf data includes single adults in specialized programs that are not included in DHS total counts.
Monthly LL79 data is pulled from the NYC Open Data portal and knitted together with historical LL37 data. Note that in May 2023, the city changed its reporting process between the laws, changing some field names. The change is detailed in the `root` column that specifies which underlying dataset that data comes from.
As of November 2023 the asylum seekers report template has not been standardized, limiting the ability to automate that data. As is, that data is manually inputted to create summary charts.

## Use this data

The `dhs_daily_report.csv` file in the `data` folder is cleaned and tidy version of the [daily DHS report pdfs](https://www.nyc.gov/assets/dhs/downloads/pdf/dailyreport.pdf) on shelter usage statistics released by the NYC Department of Homeless Services.

The `ll79_data_unique_by_agency.csv` file in the `data` folder is the time series data on the shelter population by agency over time from LL37 and LL79 reports.

The data is available for public use with attribution. You can cite Patrick Spauster and Adrian Nesta for City Limits

## Data Dictionaries

**DHS unhoused report**
| Variable    | Type | Description |
| -------- | ------- | ------- |
| measure  | character    | The population being counted. Each measure is a data label from the daily report pdf |
| count | number     | The number of people in that category, that day, according to the pdf table |
| table    | character   | The name of the table that value is found in in the daily report pdf. Values with the prefix `combined_` are sums of the individual populations recorded in the main tables + uncounted individuals in the `single_adults` table in the pdf. |
| date  | date   | The date of the pdf the count came from |

**LL79 Data by Agency**
| Variable    | Type | Description |
| -------- | ------- | ------- |
| agency_abb  | character    | The agency the shelter count is associated with. Note: HERRCs (Humanitarian Emergency Relief & Response Centers) are classified as OTI (Office of Technology & Innovation) in the data, but are renamed because they are actually run by a number of city agencies. |
| date  | date   | The first of the month for that report. Note: this is not the date the count takes place, it is the first of the report month in all cases. |
| count | number     | The number of people in that category, that month |
| table    | character   | The field name from open data. |
| root | character | the name of the dataset this value comes from, either the historical LL37 report or the new LL79 report |

This repository is created and maintained by Adrian Nesta and Patrick Spauster. With questions or corrections you can reach them at adriannesta@gmail.com and patrick.spauster@gmail.com

Updated November 2023





