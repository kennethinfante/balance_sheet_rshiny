library("tidyverse")

bs_all <- read_csv("data-raw/rpt_balance_sheet.csv")

date_filters <- read_csv("data-raw/date_filters.csv")

function(input, output, session) {
  
  updateSelectInput(session,
                    "selected_year",
                    choices = date_filters$year)
  
  observeEvent(input$selected_year,
               {
                 quarter_opt <- date_filters %>%
                   filter(year == input$selected_year) %>%
                   pull(quarter_name)
                 
                 updateSelectInput(session,
                                   "selected_quarter",
                                   choices = quarter_opt)
                 
               })
  
  observeEvent(input$selected_quarter,
               {
                 month_opt <- date_filters %>%
                   filter(quarter_name == input$selected_quarter) %>%
                   pull(month_name)
                 
                 updateSelectInput(session,
                                   "selected_month",
                                   choices = month_opt)
                 
               })
  
  
  output$balance_sheet <- renderDT({
    
    print(input$update_balance_sheet)
    
    if(input$update_balance_sheet == 0){
      return()
    }
    
    bs_all %>%
      filter(year == isolate(input$selected_year)) %>%
      filter(quarter_name == isolate(input$selected_quarter)) %>%
      filter(month_name == isolate(input$selected_month)) %>%
      datatable(rownames = FALSE)
  })
  
}