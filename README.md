Kettering SDA Church Baptisms Dashboard

A Shiny dashboard for tracking baptisms and professions of faith at Kettering Seventh-day Adventist Church.

The dashboard reads records from a Google Sheet, summarizes the current year, and displays annual totals in an interactive stacked bar chart.

Features

* Current-year summary of:
    * Baptisms
    * Professions of Faith
    * Total decisions
* Interactive annual stacked bar chart
* Hover details showing category and yearly totals
* Downloadable high-resolution PNG graph
* Responsive Bootstrap 5 interface
* Data loaded directly from Google Sheets
* Automatic cleaning and standardization of record types

Dashboard Layout

The dashboard contains two main sections:

Year-to-Date Summary

Displays the number of baptisms, professions of faith, and total decisions recorded during the current calendar year.

Annual History

Displays baptisms and professions of faith by year as a stacked bar chart.

The chart is interactive and includes hover information for:

* Year
* Category
* Category count
* Total decisions for the year

The graph can also be downloaded as a 300-DPI PNG file.

Data Structure

The dashboard expects a Google Sheet with four columns:

Column	Description
Year	Four-digit calendar year
Month	Month associated with the record
Name	Name of the person
Type	Baptism or Profession of Faith

The application standardizes values in the Type column as:

* Baptism
* Profession of Faith

Rows with a missing year, name, or recognized record type are excluded from the dashboard.

Privacy

The public dashboard displays aggregate totals only.

Individual names are used when reading and validating the source data but are not displayed in the user interface.

Care should be taken before making the underlying Google Sheet publicly accessible, since it may contain personally identifiable information.

Requirements

The application uses the following R packages:

library(shiny)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(plotly)
library(DT)
library(bslib)

Install the required packages with:

install.packages(
  c(
    "shiny",
    "tidyverse",
    "lubridate",
    "googlesheets4",
    "plotly",
    "DT",
    "bslib"
  )
)

Google Sheets Authentication

The application uses googlesheets4 to read the source spreadsheet.

For local development, authentication can use a cached OAuth credential:

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "path/to/.secrets/"
)
gs4_auth(email = "your-email@example.com")

The .secrets directory should not be committed to GitHub.

Add the following entries to .gitignore:

.secrets/
.Rhistory
.RData
.Ruserdata
rsconnect/

For deployment, a Google service account is generally preferable to an interactive OAuth login.

Running the Dashboard

Clone the repository:

git clone https://github.com/jrmilks74/baptisms.git
cd baptisms

Open the project in R or RStudio, install the required packages, and run:

shiny::runApp()

Downloading the Graph

Select Download Graph below the annual chart.

The dashboard creates a PNG file with:

* 300-DPI resolution
* 12-by-7-inch dimensions
* White background
* Date-stamped filename

Example:

kettering-baptisms-professions-of-faith-2026-07-10.png

Project Structure

baptisms/
├── app.R
├── README.md
├── .gitignore
└── .secrets/        # Local authentication files; not committed

Updating the Dashboard

The dashboard reads the Google Sheet whenever the application session loads the data.

To update the displayed totals:

1. Add or edit records in the source Google Sheet.
2. Confirm that the year, name, and type fields are complete.
3. Reload the dashboard.

No manual data export is required.

Deployment

The application can be deployed to shinyapps.io or another Shiny hosting environment.

Before deployment, confirm that:

* Google Sheets authentication works in the hosted environment.
* Authentication files are stored securely.
* The source spreadsheet is accessible to the authenticated account.
* The spreadsheet URL and sheet structure have not changed.

Version

Version 1

Updated July 10, 2026.

Author

Created by Jim Milks.

Kettering Seventh-day Adventist Church

Source Code

Code and documentation are available at:

https://github.com/jrmilks74/baptisms
