#### google analytics options ####
options("googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/analytics.readonly")

shinyServer(function(input, output, session){
  
  #### google analytics local authentication ####
  gar_auth(new_user = T) # creates an .httr-oauth file for access
  
  #### dashboard table ####
  GAtable <- reactive({
    ranges = bind_cols(
      getGAstats(c(input$dateRange1[1], input$dateRange1[2]), "Range1"),
      getGAstats(c(input$dateRange2[1], input$dateRange2[2]), "Range2"),
      getGAstats(c(input$dateRange3[1], input$dateRange3[2]), "Range3")
    ) %>% tbl_df %>% 
      mutate(
        Norm_R1_R2 = (Range1 + Range2)/(Range1[1] + Range2[1]), 
        Norm_R3 = Range3/Range3[1], 
        Tracking = round(((Norm_R3/Norm_R1_R2) - 1), 2)) %>%
      select(1:3, 6)
    colnames(ranges) <- c(
      col1 <- paste(format(as.Date(input$dateRange1[1]), "%b %d"), "-", format(as.Date(input$dateRange1[2]), "%b %d")),
      col2 <- paste(format(as.Date(input$dateRange2[1]), "%b %d"), "-", format(as.Date(input$dateRange2[2]), "%b %d")),
      col3 <- paste(format(as.Date(input$dateRange3[1]), "%b %d"), "-", format(as.Date(input$dateRange3[2]), "%b %d")),
      col4 <- "Tracking"
    )
    ranges
  })
  
  output$table <- renderDataTable({
    dat <- datatable(GAtable(), options = list(pageLength = 20, autoWidth = TRUE, dom = 't'),
                     caption = htmltools::tags$caption(
                       style = 'caption-side: top; text-align: right;',
                       'Tracking = (column 3 / (column 1 + column 2) - 1) * 100 | normalized by day (except session duration)'),
                     # colnames = GArowNames(),
                     rownames = GAtable.rownames) %>%
      formatPercentage('Tracking', 0) %>%
      formatStyle('Tracking',
                  background = styleColorBar(GAtable()$Tracking, 'steelblue'),
                  backgroundSize = '100% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      )
    return(dat)
  })
  
  #### dahboard plot ####
  GAplot <- reactive({
    getGAplot(c(input$dateRange1, input$dateRange2, input$dateRange3))
  })
  output$plot <- renderPlotly(GAplot())
  
  #### download button ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('GAdata-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(GAtable(), file = con, row.names = GAtable.rownames)
    }
  )
  
})
