# app.R

library(shiny)
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(plotly)
library(DT)
library(bslib)

# ---- Google Sheet ----

options(
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = "KetSDA/Data_Science/Baptisms_PoF/.secrets/"
)

suppressMessages(gs4_auth(email = "jrmilks@gmail.com"))

sheet_url <- paste0(
        "https://docs.google.com/spreadsheets/d/",
        "1R-SLPqHHxkj3p1XaxCNxClGy5u06MQaIP0psctPYFnQ/",
        "edit?gid=0#gid=0"
)

current_year <- year(Sys.Date())

# ---- Read and clean data ----

read_baptism_data <- function() {
        
        read_sheet(
                ss = sheet_url,
                sheet = 1,
                range = "A:D",
                col_types = "cccc"
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
                                str_detect(str_to_lower(Type), "bapt") ~
                                        "Baptism",
                                str_detect(str_to_lower(Type), "profession") ~
                                        "Profession of Faith",
                                TRUE ~ NA_character_
                        )
                ) %>%
                filter(
                        !is.na(Year),
                        !is.na(Name),
                        !is.na(Type)
                )
}

# ---- UI ----

ui <- page_fluid(
        
        theme = bs_theme(
                version = 5,
                bootswatch = "flatly"
        ),
        
        tags$head(
                tags$style(HTML("
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
    "))
        ),
        
        h1("Kettering SDA Church", class = "main-title"),
        h3(
                "Baptisms and Professions of Faith",
                class = "subtitle"
        ),
        
        layout_columns(
                col_widths = c(4, 8),
                
                card(
                        card_header(
                                paste(current_year, "Year-to-Date Summary")
                        ),
                        DTOutput("current_year_table")
                ),
                
                card(
                        card_header(
                                "Baptisms and Professions of Faith"
                        ),
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
        ),
        
        div(
                class = "app-footer",
                hr(),
                h4("Created by: Jim Milks"),
                "Version 1", br(),
                "Updated 10 July 2026", br(),
                "Code and data available at:",
                tags$a(
                        href = paste0(
                                "https://github.com/",
                                "jrmilks74/baptisms"
                        ),
                        target = "_blank",
                        rel = "noopener noreferrer",
                        "https://github.com/jrmilks74/baptisms"
                )
        )
)

# ---- Server ----

server <- function(input, output, session) {
        
        baptism_data <- reactive({
                read_baptism_data()
        })
        
        current_year_summary <- reactive({
                
                baptism_data() %>%
                        filter(Year == current_year) %>%
                        summarise(
                                Baptisms = sum(Type == "Baptism"),
                                `Professions of Faith` =
                                        sum(Type == "Profession of Faith"),
                                Total = n(),
                                .groups = "drop"
                        ) %>%
                        pivot_longer(
                                cols = everything(),
                                names_to = "Category",
                                values_to = "Count"
                        )
        })
        
        yearly_summary <- reactive({
                
                baptism_data() %>%
                        count(Year, Type, name = "Count") %>%
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
        
        # One ggplot object is used for both the interactive chart
        # and the downloaded PNG.
        stacked_bar_plot <- reactive({
                
                yearly_summary() %>%
                        ggplot(
                                aes(
                                        x = factor(Year),
                                        y = Count,
                                        fill = Type,
                                        text = paste0(
                                                "Year: ", Year,
                                                "<br>Category: ", Type,
                                                "<br>Count: ", Count,
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
                                )
                        ) +
                        theme_minimal(base_size = 15) +
                        theme(
                                plot.title = element_text(
                                        hjust = 0.5,
                                        face = "bold",
                                        size = 20
                                ),
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
                        colnames = c("Category", "Count"),
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
                                        t = 70,
                                        b = 120
                                )
                        )
        })
        
        output$download_graph <- downloadHandler(
                
                filename = function() {
                        paste0(
                                "kettering-baptisms-professions-of-faith-",
                                Sys.Date(),
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
