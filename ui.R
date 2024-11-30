library(htmlwidgets)
library(pivottabler)
library(shinycustomloader)

navbarPage(
  "Balance Sheet",
  tabPanel(
    "Adjusted",
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
  collapsible = TRUE
)