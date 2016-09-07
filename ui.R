library(shiny)

shinyUI(
  pageWithSidebar(
    
    headerPanel(
      HTML('Google Analytics Dashboard
			<a href="https://www.veritasgenetics.com/" target="_blank"><img align="right" height = "50px" alt="VG Logo" src="VG-logo.png" /></a>'
      )),
    
    sidebarPanel(#googleAuthUI(id = "loginButton"),
      h4("Date ranges:"), width = 3,
      dateRangeInput(inputId = "dateRange1", label = NULL, start = update(Sys.Date(), day = 1) - months(2), end = update(Sys.Date(), day = 1) - months(1) - days(1), format = "M-dd-yyyy", max = Sys.Date()),
      dateRangeInput(inputId = "dateRange2", label = NULL, start = update(Sys.Date(), day = 1) - months(1), end = update(Sys.Date(), day = 1) - days(1), format = "M-dd-yyyy", max = Sys.Date()),
      dateRangeInput(inputId = "dateRange3", label = NULL, start = update(Sys.Date(), day = 1), end = Sys.Date(), format = "M-dd-yyyy", max = Sys.Date()),
      submitButton("Submit New Date Ranges")
    ),
    
    mainPanel(width = 8,
              wellPanel(fluidRow(downloadButton('downloadData', 'Download .csv'))),
              tags$head(tags$style("#table  {white-space: nowrap;  }")),
              fluidRow(dataTableOutput("table")),
              fluidRow(plotlyOutput("plot"))
    )
  )
)
