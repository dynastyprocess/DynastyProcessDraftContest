options(dplyr.summarise.inform = FALSE)

# fp_rookies <- ffpros::fp_rankings("rookies") |>
#   select(Player = player_name, Position = pos, fantasypros_id) |>
#   arrange(Position)
# write_parquet(fp_rookies,"data/rookies.parquet")

rookies <- read_parquet("data/rookies.parquet") %>% arrange(Position)
teams <- read_parquet("data/teams.parquet") %>% pull(team)

entries <- compare_entries()
scores <- summarise_entries(entries)

ui <- dashboardPage(
  # dark = NULL,
  sidebar_collapsed = TRUE,
  title = "Draft Contest",
  navbar = ui_header("2022 Draft Contest - DynastyProcess.com"),
  sidebar = ui_sidebar(
    # entry_tab(id = "entries"),
    external_menuItem("More by DynastyProcess", "https://dynastyprocess.com", icon = "quidditch")
  ),
  body = dashboardBody(
    includeCSS("dp.css"),
    meta_tag(),
    use_waiter(),
    waiter_on_busy(html = spin_dots(), color = transparent(0.3)),
    use_sever(),
    tabItems(
      # entry_ui(id = "entries")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$debug, browser())

  sever_dp()

  # entry_server(id = "entries", rookies = rookies, teams = teams)

}

shinyApp(ui, server)
