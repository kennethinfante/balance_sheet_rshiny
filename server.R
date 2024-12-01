library("tidyverse")
library(htmlwidgets)
library(pivottabler)
library(glue)

bs_all <- read_csv("data-raw/balance_sheet_model.csv")

# add custom columns
bs_all <- bs_all %>%
  mutate(BS_Flag = if_else(bs_flag == "Assets", "Assets", "Liabilities & Equity")) %>% 
  mutate(NS_BS_Flag = if_else(ns_bs_flag == "Assets", "Assets", "Liabilities & Equity")) %>% 
  arrange(year, quarter_name, month)

date_filters <- read_csv("data-raw/date_filters.csv")

date_filters <- date_filters %>% arrange(desc(year), quarter_name, month)

bs_updated <- 0
ns_bs_updated <- 0

function(input, output, session) {
  
  ################## Helpers ##################
  try_filter <- function(.data, col, selection) { 
    if (!is.null(selection)) {
      .data %>% filter({{col}} %in% isolate({{selection}}))
    } else {
      .data
    }
  }
  
  ################## Adjusted ##################
  updateCheckboxGroupInput(session,
                    "selected_year",
                    choices = unique(date_filters$year))
                     
  observeEvent(input$selected_year,
               {
                 quarter_opt <- date_filters %>%
                   filter(year %in% input$selected_year) %>%
                   distinct(quarter_name) %>% 
                   pull(quarter_name)
                 
                 updateCheckboxGroupInput(session,
                                   "selected_quarter",
                                   choices = quarter_opt)
                 
               })
  
  observeEvent(input$selected_quarter,
               {
                 month_opt <- date_filters %>%
                   filter(year %in% input$selected_year) %>%
                   filter(quarter_name %in% input$selected_quarter) %>%
                   pull(month_name)
                 
                 updateCheckboxGroupInput(session,
                                   "selected_month",
                                   choices = month_opt)
                 
               })
  
  output$balance_sheet <- renderPivottabler({
    
    print(input$update_balance_sheet)
    
    if(input$update_balance_sheet == 0){
      return()
    }
    
    # created a custom filter function
    bs_filtered <- bs_all %>%
      try_filter(year, input$selected_year) %>%
      try_filter(quarter_name, input$selected_quarter) %>%
      try_filter(month_name, input$selected_month)
    
    pt <- PivotTable$new(totalStyle = list(
      "font"="1em arial",
      "font-weight"="bold",
      "background-color"="#F2F2F2"))
    pt$addData(bs_filtered)
    pt$addColumnDataGroups("year", addTotal=FALSE)
    pt$addColumnDataGroups("quarter_name", addTotal=FALSE)
    pt$addColumnDataGroups("month_name", addTotal=FALSE)
    pt$addRowDataGroups("BS_Flag", addTotal=FALSE)
    pt$addRowDataGroups("category")
    pt$addRowDataGroups("account_full_name", addTotal=FALSE)
    pt$defineCalculation(calculationName="Total", summariseExpression="sum(std_amount_gbp)", format=list(nsmall=0, big.mark=",", decimal.mark=".", scientific=FALSE))
    pt$evaluatePivot()
    
    pivottabler(pt)
  
  })
  
  output$last_update <- renderText({
    # another way of updating global variable
    if (input$update_balance_sheet > bs_updated) {
      bs_updated <<- input$update_balance_sheet
      paste("Last updated ", format(Sys.time(), "%b %d %Y %H:%M:%S"))
    }

  })
  
################## Netsuite ##################
  updateCheckboxGroupInput(session,
                           "ns_selected_year",
                           choices = unique(date_filters$year))
  
  observeEvent(input$ns_selected_year,
               {
                 quarter_opt <- date_filters %>%
                   filter(year %in% input$ns_selected_year) %>%
                   distinct(quarter_name) %>% 
                   pull(quarter_name)
                 
                 updateCheckboxGroupInput(session,
                                          "ns_selected_quarter",
                                          choices = quarter_opt)
                 
               })
  
  observeEvent(input$ns_selected_quarter,
               {
                 month_opt <- date_filters %>%
                   filter(year %in% input$ns_selected_year) %>%
                   filter(quarter_name %in% input$ns_selected_quarter) %>%
                   pull(month_name)
                 
                 updateCheckboxGroupInput(session,
                                          "ns_selected_month",
                                          choices = month_opt)
                 
               })
  
  output$ns_balance_sheet <- renderPivottabler({
    
    print(input$ns_update_balance_sheet)
    
    if(input$ns_update_balance_sheet == 0){
      return()
    }
    
    # created a custom filter function
    bs_filtered <- bs_all %>%
      try_filter(year, input$ns_selected_year) %>%
      try_filter(quarter_name, input$ns_selected_quarter) %>%
      try_filter(month_name, input$ns_selected_month)
    
    pt <- PivotTable$new(totalStyle = list(
      "font"="1em arial",
      "font-weight"="bold",
      "background-color"="#F2F2F2"))
    pt$addData(bs_filtered)
    pt$addColumnDataGroups("year", addTotal=FALSE)
    pt$addColumnDataGroups("quarter_name", addTotal=FALSE)
    pt$addColumnDataGroups("month_name", addTotal=FALSE)
    pt$addRowDataGroups("NS_BS_Flag", addTotal=FALSE)
    pt$addRowDataGroups("ns_category")
    pt$addRowDataGroups("account_full_name", addTotal=FALSE)
    pt$defineCalculation(calculationName="Total", summariseExpression="sum(ns_std_amount_gbp)", format=list(nsmall=0, big.mark=",", decimal.mark=".", scientific=FALSE))
    pt$evaluatePivot()
    
    pivottabler(pt)
    
  })
  
  output$ns_last_update <- renderText({
    # another way of updating global variable
    if (input$ns_update_balance_sheet > ns_bs_updated) {
      ns_bs_updated <<- input$ns_update_balance_sheet
      paste("Last updated ", format(Sys.time(), "%b %d %Y %H:%M:%S"))
    }
    
  })
  
}