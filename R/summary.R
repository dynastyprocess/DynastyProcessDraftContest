summary_tab <- function(id){
  ns <- NS(id)
  menuItem("Summary", tabName = ns("tab"), icon = icon("hat-wizard"))
}

summary_ui <- function(id){
  ns <- NS(id)

  tabItem(
    tabName = ns("tab"),
    box(
      width = 12,
      status = "danger",
      closable = FALSE,
      title = "Summary",
      DTOutput(ns("scores")),
    )
  )
}

summary_server <- function(id,entries,scores){
  moduleServer(
    id,
    function(input,output,session){

      ns <- session$ns

      output$entries

      output$scores <- renderDT({
        scores %>%
          datatable(width = '100%',
                    class = 'compact stripe nowrap',
                    rownames = FALSE,
                    options = list(
                      paging = FALSE
                    )
                    ) %>%
          tantastic::fmt_dtcol(scores,col_id = seq_len(ncol(scores)))
      })

    }
  )
}
