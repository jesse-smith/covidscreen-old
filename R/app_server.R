#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic

  dist_params <- mod_input_params_server("mod_input_params_sim")

  mod_table_server("mod_table_probs", dist_params = reactive(dist_params()))
}
