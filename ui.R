library(htmlwidgets)
library(pivottabler)
library(shinycustomloader)

navbarPage(
  "Balance Sheet",
  tabPanel(
    "Per Adjusted",
    fluidPage(fluidRow(
      column(
        checkboxGroupInput("selected_year",
                    "Select a year",
                    choices = NULL),
        width = 3
      ),
      column(
        checkboxGroupInput("selected_quarter",
                    "Select a quarter",
                    choices = NULL),
        width = 3
      ),
      column(
        checkboxGroupInput("selected_month",
                    "Select an month",
                    choices = NULL,
                    width = "100%"),
        width = 3
      )
    )),
    actionButton("update_balance_sheet", label = "Update Balance Sheet", width = "100%"),
    textOutput("last_update"),
    withLoader(pivottablerOutput('balance_sheet'))
  ),
  tabPanel(
    "Per Netsuite",
    fluidPage(fluidRow(
      column(
        checkboxGroupInput("ns_selected_year",
                           "Select a year",
                           choices = NULL),
        width = 3
      ),
      column(
        checkboxGroupInput("ns_selected_quarter",
                           "Select a quarter",
                           choices = NULL),
        width = 3
      ),
      column(
        checkboxGroupInput("ns_selected_month",
                           "Select an month",
                           choices = NULL,
                           width = "100%"),
        width = 3
      )
    )),
    actionButton("ns_update_balance_sheet", label = "Update Balance Sheet", width = "100%"),
    textOutput("ns_last_update"),
    withLoader(pivottablerOutput('ns_balance_sheet'))
  ),
  collapsible = TRUE
)