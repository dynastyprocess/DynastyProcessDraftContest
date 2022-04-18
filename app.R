options(dplyr.summarise.inform = FALSE)

# fp_rookies <- ffpros::fp_rankings("rookies") |>
#   select(Player = player_name, Position = pos, fantasypros_id) |>
#   arrange(Position)
# write_parquet(fp_rookies,"data/rookies.parquet")
rookies <- read_parquet("data/rookies.parquet") %>% arrange(Position)
teams <- read_parquet("data/teams.parquet") %>% pull(team)

ui <- dashboardPage(
  # dark = NULL,
  sidebar_collapsed = TRUE,
  title = "Draft Contest",
  navbar = ui_header("2022 Draft Contest - DynastyProcess.com"),
  sidebar = ui_sidebar(
    menuItem("Draft Contest", tabName = "draftcontest", icon = icon("hat-wizard")),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "quidditch")
  ),
  body = dashboardBody(
    includeCSS("dp.css"),
    use_waiter(),
    waiter_on_busy(html = spin_dots(), color = transparent(0.3)),
    use_sever(),
    tabItems(
      tabItem(
        tabName = "draftcontest",
        box(
          width = 12,
          inputId = "box-inputs",
          status = "danger",
          title = "ABOUT",
          closable = FALSE,
          includeMarkdown("about.md"),
          hr(),
          fluidRow(
            column(3, textInput("entry_name", label = "Name")),
            column(3, textInput("nickname", label = "Discord/Twitter Username")),
            column(3, textInput("email", label = "Email"))
          )
        ),
        box(
          width = 12,
          status = "danger",
          closable = FALSE,
          title = "SELECTIONS",
          DTOutput("draftcontest_table"),
          footer = div(actionButton("review_entry",
            label = "Review your entry!",
            class = "btn-success"
          ),
          style = "text-align:center;"
          )
        ),
        # br(),
        # actionButton("debug", label = "debug")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$debug, browser())

  sever_dp()

  output$draftcontest_table <- renderDT({
    df <- rookies %>%
      transmute(Player,
        Position,
        Top64 = gen_input_map(checkboxInput,
          id_prefix = "top64_",
          uid = fantasypros_id,
          label = NULL,
          width = "50px"
        ),
        DraftedBy = gen_input_map(selectInput,
          id_prefix = "team_",
          uid = fantasypros_id,
          label = NULL,
          choices = c("Select", teams),
          width = "100px"
        )
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
      DTOutput("selections_list"),
      footer = list(
        actionButton("submit_entry", label = "Submit!", class = "btn-success"),
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

}

shinyApp(ui, server)
