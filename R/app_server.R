#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Scenario tab
  params_scenarios <-  mod_input_params_server("scenarios", interval = FALSE)
  mod_plot_output_server(
    "scenarios",
    params = reactive(params_scenarios())
  )

  # Explore tab
  params_explore <- mod_input_params_server("distribution")
  dist_explore <- reactive(
    do.call(calc_dist, params_explore()),
    label = "calc_dist_explore()"
  )
  mod_table_server(
    "distribution",
    dist = reactive(dist_explore())
  )
}
