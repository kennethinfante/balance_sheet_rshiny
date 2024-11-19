library("DT")

navbarPage(
  "Balance Sheet",
  tabPanel(
    "Select date filters",
    fluidPage(fluidRow(
      column(
        selectInput("selected_year",
                    "Select a year",
                    choices = NULL),
        width = 3
      ),
      column(
        selectInput("selected_quarter",
                    "Select a quarter",
                    choices = NULL),
        width = 3
      ),
      column(
        selectInput("selected_month",
                    "Select an month",
                    choices = NULL,
                    width = "100%"),
        width = 3
      )
    )),
    actionButton("update_balance_sheet", label = "Update Balance Sheet", width = "100%"),
    DTOutput("balance_sheet")
  ),
  collapsible = TRUE
)