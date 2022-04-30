summary_tab <- function(id = "summary"){
  ns <- NS(id)
  menuItem("Summary", tabName = ns("tab"), icon = icon("hat-wizard"))
}

summary_ui <- function(id = "summary"){
  ns <- NS(id)

  tabItem(
    tabName = ns("tab"),
    box(
      width = 12,
      status = "danger",
      closable = FALSE,
      title = "Summary",
      DTOutput(ns("scores"))
    ),
    br(),
    box(
      width = 12,
      status = "danger",
      closable = FALSE,
      title = "Entries",
      DTOutput(ns("entries"))
    )
  )
}

summary_server <- function(entries,scores,id="summary"){
  moduleServer(
    id,
    function(input,output,session){

      ns <- session$ns

      colourlist <- colorRampPalette(RColorBrewer::brewer.pal(3, "PRGn"))

      output$scores <- renderDT({
        scores %>%
          datatable(
            # width = '100%',
            class = 'compact stripe nowrap',
            rownames = FALSE,
            options = list(
              paging = FALSE,
              scrollX = TRUE
            )
          ) %>%
          formatStyle(
            columns = 0:5,
            valueColumns = "Score",
            backgroundColor = styleInterval(
              quantile(scores$Score,
                       probs = seq(0.05, 0.95, 0.05),
                       na.rm = TRUE),
              colourlist(20)))
      })

      output$entries <- renderDT({
        entries %>%
          datatable(
            # width = '100%',
            class = 'compact stripe nowrap',
            rownames = FALSE,
            options = list(
              scrollX = TRUE,
              pageLength = 50
            )
          ) %>%
          formatStyle(
            columns = 0:9,
            valueColumns = "Score",
            backgroundColor = styleInterval(
              quantile(entries$Score,
                       probs = seq(0.05, 0.95, 0.05),
                       na.rm = TRUE),
              colourlist(20)))
      })

    }
  )
}
