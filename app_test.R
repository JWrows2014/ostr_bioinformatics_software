# app.R
# Shiny app: Learn + compare commercial bioinformatics software capabilities
#
# Install once:
# install.packages(c("shiny","bslib","dplyr","tidyr","DT","ggplot2","stringr","purrr"))

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(stringr)
library(purrr)

capability <- read.csv("capability_matrix_2026-01-26.csv")

# -----------------------------
# Shiny UI
# -----------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("OSTR Commercial Bioinformatics Software Finder"),
  layout_sidebar(
    sidebar = sidebar(
      h5("Filter"),
      selectizeInput(
        "pkg",
        "Packages",
        choices = capability$package,
        selected = capability$package,
        multiple = TRUE,
        options = list(plugins = list("remove_button"),placeholder="Choose packages...")
      ),
      selectizeInput(
        "cat",
        "Capability categories",
        choices = sort(unique(capability$category)),
        selected = sort(unique(capability$category)),
        multiple = TRUE,
        options = list(plugins = list("remove_button"),placeholder="Choose packages...")
      ),
      textInput("q", "Search capability text", value = ""),
      checkboxGroupInput(
        "support_filter",
        "Show support levels",
        choices = c("Yes","Partial","No"),
        selected = c("Yes","Partial","No"),
        inline = TRUE
      ),
    ),
    navset_card_tab(
      nav_panel(
        "Compare",
        p("Select packages + categories on the left. Use this to compare 'what each package can do'."),
        DTOutput("matrix_table"),
        br(),
       ),
      nav_panel(
         "Visual comparison",
         plotOutput("heatmap", height = "auto")
      ),
      nav_panel(
        "Learn",
        p("Curated jump-links into manuals/tutorials (edit these in learn_links)."),
        DTOutput("learn_table")
      ),
      nav_panel(
        "Data (edit me)",
        p("This is the underlying long-form dataset the app uses. Export it, version-control it, and refine."),
        downloadButton("download_csv", "Download capability matrix (CSV)"),
        br(), br(),
        DTOutput("raw_table")
      )
    )
  )
)

# -----------------------------
# Shiny server
# -----------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    df <- capability %>%
      filter(package %in% input$pkg) %>%
      filter(category %in% input$cat) %>%
      filter(support %in% input$support_filter)
    
    q <- str_trim(input$q)
    if (nzchar(q)) {
      df <- df %>% filter(str_detect(str_to_lower(capability), str_to_lower(q)))
    }
    df
  })
  
  output$matrix_table <- renderDT({
    df <- filtered() %>%
      select(package, availability, category, capability, support, manual_url) %>%
      arrange(category, capability, package)
    
    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE),
      escape = FALSE
    )
  })
}
shinyApp(ui, server)
