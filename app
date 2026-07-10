# app.R

library(shiny)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(plotly)
library(DT)
library(bslib)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Set these values in .Renviron, shinyapps.io environment variables,
# or another secure deployment configuration.
#
# Required:
#   BAPTISMS_SHEET_URL
#
# Optional:
#   GOOGLE_SERVICE_ACCOUNT_JSON
#
# GOOGLE_SERVICE_ACCOUNT_JSON should be the path to a service-account JSON file.
# Do not commit the JSON file to GitHub.

sheet_url <- Sys.getenv("BAPTISMS_SHEET_URL")
service_account_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")

if (!nzchar(sheet_url)) {
        stop(
                paste(
                        "The BAPTISMS_SHEET_URL environment variable is not set.",
                        "Add it to your .Renviron file or deployment settings."
                ),
                call. = FALSE
        )
}

# ------------------------------------------------------------------------------
# Google Sheets authentication
# ------------------------------------------------------------------------------

authenticate_google_sheets <- function() {
        
        if (
                nzchar(service_account_json) &&
                file.exists(service_account_json)
        ) {
                gs4_auth(path = service_account_json)
        } else if (interactive()) {
                # Local interactive OAuth.
                gs4_auth()
        } else {
                stop(
                        paste(
                                "Google Sheets authentication is unavailable.",
                                "Set GOOGLE_SERVICE_ACCOUNT_JSON to the path",
                                "of a valid service-account JSON file."
                        ),
                        call. = FALSE
                )
        }
}

authenticate_google_sheets()

# Use Eastern Time when determining the current year and download date.
app_timezone <- "America/New_York"

today_eastern <- function() {
        as.Date(with_tz(Sys.time(), app_timezone))
}

current_year <- year(today_eastern())

# ------------------------------------------------------------------------------
# Data import and cleaning
# ------------------------------------------------------------------------------

read_baptism_data <- function() {
        
        read_sheet(
                ss = sheet_url,
                sheet = 1,
                range = "A:D",
                col_types = "cccc",
                na = c("", "NA")
        ) %>%
                rename(
                        Year = 1,
                        Month = 2,
                        Name = 3,
                        Type = 4
                ) %>%
                mutate(
                        Year = suppressWarnings(as.integer(Year)),
                        Month = str_squish(as.character(Month)),
                        Name = str_squish(as.character(Name)),
                        Type = str_squish(as.character(Type)),
                        Type = case_when(
                                str_detect(
                                        str_to_lower(Type),
                                        "bapt"
                                ) ~ "Baptism",
                                str_detect(
                                        str_to_lower(Type),
                                        "profession"
                                ) ~ "Profession of Faith",
                                TRUE ~ NA_character_
                        )
                ) %>%
                filter(
                        !is.na(Year),
                        !is.na(Name),
                        Name != "",
                        !is.na(Type)
                )
}

# ------------------------------------------------------------------------------
# User interface
# ------------------------------------------------------------------------------

ui <- page_fluid(
        
        theme = bs_theme(
                version = 5,
                bootswatch = "flatly"
        ),
        
        tags$head(
                tags$meta(
                        name = "viewport",
                        content = paste(
                                "width=device-width,",
                                "initial-scale=1"
                        )
                ),
                
                tags$style(
                        HTML("
                                .main-title {
                                        text-align: center;
                                        margin-top: 30px;
                                        margin-bottom: 5px;
                                        font-weight: 700;
                                }

                                .subtitle {
                                        text-align: center;
                                        margin-top: 0;
                                        margin-bottom: 30px;
                                        font-weight: 400;
                                }

                                .card {
                                        margin-bottom: 25px;
                                }

                                .dataTables_wrapper {
                                        font-size: 18px;
                                }

                                table.dataTable tbody td {
                                        padding: 12px;
                                }

                                .graph-download {
                                        margin-top: 10px;
                                        margin-bottom: 5px;
                                        text-align: right;
                                }

                                .app-footer {
                                        margin-top: 20px;
                                        margin-bottom: 35px;
                                        font-size: 15px;
                                }

                                .app-footer a {
                                        overflow-wrap: anywhere;
                                }
                        ")
                )
        ),
        
        h1(
                "Kettering SDA Church",
                class = "main-title"
        ),
        
        h3(
                "Baptisms and Professions of Faith",
                class = "subtitle"
        ),
        
        layout_columns(
                col_widths = c(4, 8),
                
                card(
                        card_header(
                                paste(
                                        current_year,
                                        "Year-to-Date Summary"
                                )
                        ),
                        
                        card_body(
                                DTOutput("current_year_table")
                        )
                ),
                
                card(
                        card_header(
                                "Baptisms and Professions of Faith"
                        ),
                        
                        card_body(
                                plotlyOutput(
                                        "stacked_bar",
                                        height = "500px"
                                ),
                                
                                div(
                                        class = "graph-download",
                                        
                                        downloadButton(
                                                outputId = "download_graph",
                                                label = "Download Graph",
                                                class = "btn-primary"
                                        )
                                )
                        )
                )
        ),
        
        div(
                class = "app-footer",
                
                hr(),
                
                h4("Created by: Jim Milks"),
                
                "Version 1",
                br(),
                
                "Updated 10 July 2026",
                br(),
                
                "Code and data available at: ",
                
                tags$a(
                        href = paste0(
                                "https://github.com/",
                                "jrmilks74/baptisms"
                        ),
                        target = "_blank",
                        rel = "noopener noreferrer",
                        paste0(
                                "https://github.com/",
                                "jrmilks74/baptisms"
                        )
                )
        )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
        
        baptism_data <- reactive({
                
                tryCatch(
                        read_baptism_data(),
                        error = function(error) {
                                showNotification(
                                        paste(
                                                "The baptism data could not",
                                                "be loaded:",
                                                error$message
                                        ),
                                        type = "error",
                                        duration = NULL
                                )
                                
                                tibble(
                                        Year = integer(),
                                        Month = character(),
                                        Name = character(),
                                        Type = character()
                                )
                        }
                )
        })
        
        current_year_summary <- reactive({
                
                data <- baptism_data()
                
                tibble(
                        Category = c(
                                "Baptisms",
                                "Professions of Faith",
                                "Total"
                        ),
                        Count = c(
                                sum(
                                        data$Year == current_year &
                                                data$Type == "Baptism",
                                        na.rm = TRUE
                                ),
                                sum(
                                        data$Year == current_year &
                                                data$Type ==
                                                "Profession of Faith",
                                        na.rm = TRUE
                                ),
                                sum(
                                        data$Year == current_year,
                                        na.rm = TRUE
                                )
                        )
                )
        })
        
        yearly_summary <- reactive({
                
                data <- baptism_data()
                
                validate(
                        need(
                                nrow(data) > 0,
                                "No baptism or profession-of-faith records are available."
                        )
                )
                
                data %>%
                        count(
                                Year,
                                Type,
                                name = "Count"
                        ) %>%
                        complete(
                                Year,
                                Type = c(
                                        "Baptism",
                                        "Profession of Faith"
                                ),
                                fill = list(Count = 0)
                        ) %>%
                        mutate(
                                Type = factor(
                                        Type,
                                        levels = c(
                                                "Profession of Faith",
                                                "Baptism"
                                        )
                                )
                        ) %>%
                        group_by(Year) %>%
                        mutate(
                                Year_Total = sum(Count)
                        ) %>%
                        ungroup()
        })
        
        stacked_bar_plot <- reactive({
                
                chart_data <- yearly_summary()
                
                ggplot(
                        chart_data,
                        aes(
                                x = factor(Year),
                                y = Count,
                                fill = Type,
                                text = paste0(
                                        "Year: ",
                                        Year,
                                        "<br>Category: ",
                                        Type,
                                        "<br>Count: ",
                                        Count,
                                        "<br>Year total: ",
                                        Year_Total
                                )
                        )
                ) +
                        geom_col(width = 0.7) +
                        labs(
                                x = "Year",
                                y = "Total",
                                fill = NULL
                        ) +
                        scale_fill_manual(
                                values = c(
                                        "Baptism" = "#2C7FB8",
                                        "Profession of Faith" = "#F28E2B"
                                ),
                                breaks = c(
                                        "Baptism",
                                        "Profession of Faith"
                                ),
                                drop = FALSE
                        ) +
                        theme_minimal(base_size = 15) +
                        theme(
                                axis.title = element_text(size = 14),
                                axis.text = element_text(size = 13),
                                legend.position = "bottom",
                                panel.grid.minor = element_blank()
                        )
        })
        
        output$current_year_table <- renderDT({
                
                datatable(
                        current_year_summary(),
                        rownames = FALSE,
                        colnames = c(
                                "Category",
                                "Count"
                        ),
                        options = list(
                                dom = "t",
                                ordering = FALSE,
                                paging = FALSE,
                                searching = FALSE,
                                info = FALSE,
                                columnDefs = list(
                                        list(
                                                className = "dt-left",
                                                targets = 0
                                        ),
                                        list(
                                                className = "dt-center",
                                                targets = 1
                                        )
                                )
                        )
                )
        })
        
        output$stacked_bar <- renderPlotly({
                
                ggplotly(
                        stacked_bar_plot(),
                        tooltip = "text"
                ) %>%
                        layout(
                                barmode = "stack",
                                legend = list(
                                        orientation = "h",
                                        x = 0.25,
                                        y = -0.25
                                ),
                                margin = list(
                                        l = 60,
                                        r = 30,
                                        t = 30,
                                        b = 120
                                )
                        ) %>%
                        config(
                                displaylogo = FALSE,
                                responsive = TRUE
                        )
        })
        
        output$download_graph <- downloadHandler(
                
                filename = function() {
                        paste0(
                                "kettering-baptisms-professions-of-faith-",
                                today_eastern(),
                                ".png"
                        )
                },
                
                content = function(file) {
                        
                        ggsave(
                                filename = file,
                                plot = stacked_bar_plot(),
                                device = "png",
                                width = 12,
                                height = 7,
                                units = "in",
                                dpi = 300,
                                bg = "white"
                        )
                },
                
                contentType = "image/png"
        )
}

shinyApp(ui, server)
