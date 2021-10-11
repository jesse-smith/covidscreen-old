#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  dist_params <- mod_input_params_server("explore")
  dist <- reactive(do.call(calc_dist, dist_params()), label = "calc_dist()")
  mod_table_server(
    "table",
    dist = reactive(dist(), label = "dist_arg()")
  )
}
