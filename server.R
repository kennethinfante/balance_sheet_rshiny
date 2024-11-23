library("tidyverse")
library(htmlwidgets)
library(pivottabler)

bs_all <- read_csv("data-raw/rpt_balance_sheet.csv")

# add custom columns
bs_all <- bs_all %>%
  mutate(BS_Flag = if_else(bs_flag == "Assets", "Assets", "Liabilities & Equity")) %>% 
  mutate(NS_BS_Flag = if_else(ns_bs_flag == "Assets", "Assets", "Liabilities & Equity")) %>% 
  arrange(year, quarter_name, month)

date_filters <- read_csv("data-raw/date_filters.csv")

date_filters <- date_filters %>% arrange(desc(year), quarter_name, month)

function(input, output, session) {
  
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
    
    bs_filtered <- bs_all %>%
      filter(year == isolate(input$selected_year)) %>%
      filter(quarter_name == isolate(input$selected_quarter)) %>%
      filter(month_name == isolate(input$selected_month))
    
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
    pt$defineCalculation(calculationName="TotalDKK", summariseExpression="sum(std_amount)", format=list(nsmall=0, big.mark=",", decimal.mark=".", scientific=FALSE))
    pt$evaluatePivot()
    
    pivottabler(pt)
  })
}