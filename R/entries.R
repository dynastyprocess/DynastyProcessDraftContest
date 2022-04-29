entry_ui <- function(id = "entries"){
  ns <- NS(id)
  tabItem(
    tabName = ns("tab"),
    box(
      width = 12,
      inputId = ns("box-inputs"),
      status = "danger",
      title = "ABOUT",
      closable = FALSE,
      includeMarkdown("about.md"),
      hr(),
      fluidRow(
        column(3, textInput(ns("entry_name"), label = "Name")),
        column(3, textInput(ns("nickname"), label = "Discord/Twitter Username")),
        column(3, textInput(ns("email"), label = "Email"))
      )
    ),
    box(
      width = 12,
      status = "danger",
      closable = FALSE,
      title = "SELECTIONS",
      DTOutput(ns("draftcontest_table")),
      footer = div(actionButton(ns("review_entry"),
                                label = "Review your entry!",
                                # class = "btn-success",
                                status = "success"
      ),
      style = "text-align:center;"
      )
    ),
    # br(),
    # actionButton("debug", label = "debug")
  )
}

entry_tab <- function(id = "entries"){
  ns <- NS(id)
  menuItem("Draft Contest", tabName = ns("tab"), icon = icon("hat-wizard"))
}

entry_server <- function(id = "entries", rookies, teams){
  moduleServer(
    id,
    function(input,output,session){

      ns <- session$ns

      output$draftcontest_table <- renderDT({
        df <- rookies %>%
          transmute(
            Player,
            Position,
            Top64 = purrr::map_chr(
              fantasypros_id,
              ~checkboxInput(ns(paste0("top64_",fantasypros_id)),
                             label = NULL,
                             width = "50px") %>%
                as.character()),
            DraftedBy = purrr::map_chr(
              fantasypros_id,
              ~selectInput(ns(paste0("team_",fantasypros_id)),
                           label = NULL,
                           choices = c("Select",teams),
                           width = "100px") %>%
                as.character()),
          ) %>%
          datatable(
            rownames = FALSE,
            escape = FALSE,
            class = "compact stripe nowrap",
            selection = "none",
            options = list(
              paging = FALSE,
              ordering = FALSE,
              searching = FALSE,
              info = FALSE,
              scrollX = TRUE,
              preDrawCallback = JS("function() {Shiny.unbindAll(this.api().table().node()); }"),
              drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } ")
            )
          )
      })

      selections <- reactive({
        rookies %>%
          transmute(Player,
                    Position,
                    Top64 = paste0("top64_", fantasypros_id),
                    DraftedBy = paste0("team_", fantasypros_id),
                    Top64 = read_inputs(Top64, nullarg = FALSE, type = "lgl"),
                    DraftedBy = read_inputs(DraftedBy, nullarg = NA)
          ) %>%
          dplyr::filter(Top64)
      })

      output$selections_list <- renderDT({
        selections() %>%
          datatable(
            class = "compact stripe",
            rownames = FALSE,
            width = "100%",
            options = list(
              paging = FALSE,
              ordering = FALSE,
              searching = FALSE,
              scrollX = TRUE,
              info = FALSE
            )
          )
      })

      observeEvent(input$review_entry, {
        showModal(modalDialog(
          title = "Review Entry", size = "l",
          need(nrow(selections()) > 0, "Please make some selections!"),
          need(
            !("Select" %in% selections()$DraftedBy),
            "Please select a team for each player you think will be drafted!"
          ),
          need(input$entry_name, "Please provide your name!"),
          need(input$nickname, "Please provide your nickname/handle!"),
          need(input$email, "Please provide your email!"),
          br(),
          glue("Selections for {input$entry_name}:"),
          br(),
          DTOutput(ns("selections_list")),
          footer = list(
            actionButton(ns("submit_entry"), label = "Submit!", class = "btn-success"),
            modalButton("Cancel")
          )
        ))
      })

      observeEvent(input$submit_entry,{

        req(nrow(selections())>0,
            !("Select" %in% selections()$DraftedBy),
            input$entry_name,
            input$nickname,
            input$email)

        showModal(modalDialog('Saving your entry!'))

        entries_append <- selections() %>%
          mutate(entry_name = input$entry_name,
                 entry_nickname = input$nickname,
                 entry_email = input$email,
                 entry_date = Sys.time()) %>%
          select(starts_with('entry'),everything())

        write_dataset(dataset = entries_append,
                      format = "parquet",
                      path = "storage",
                      partitioning = c("entry_email"),
                      hive_style = TRUE,
                      basename_template = glue::glue("{as.numeric(Sys.time())}_{{i}}.parquet"))

        Sys.sleep(2)

        showModal(modalDialog(title = "Successfully saved to server!",
                              shiny::markdown("Please make sure you submit a copy of your donation receipt (including receipt number) either to Discord or by email!")))
      })
    })
}
