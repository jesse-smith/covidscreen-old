#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_table_ui_row_sort(ns),
    mod_table_ui_row_table(ns)
  )
}

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, dist_params){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Calculate distribution
    dist <- reactive(dist_yn(
      do.call(calc_dist, dist_params()),
      cols = c("vac", "inf", "symp", "test", "detect")
    ))

    # Define all possible table columns
    col_def <- list(
      vac = react_col_def("Vaccinated"),
      inf = react_col_def("Infected"),
      symp = react_col_def("Symptomatic"),
      test = react_col_def("Tested"),
      detect = react_col_def("Detected"),
      p = react_col_def(
        "Probability",
        format = reactable::colFormat(digits = 1, percent = TRUE)
      )
    )

    # Output table
    output$table <- reactable::renderReactable(reactable::reactable(
      dist_data(dist(), cols = input$col_select),
      columns = col_def[c("p", input$col_select)],
      resizable = TRUE,
      defaultSortOrder = "desc",
      pagination = FALSE,
      selection = "multiple",
      onClick = "select",
      highlight = TRUE,
      striped = TRUE,
      compact = TRUE
    ))
  })
}

#' Create Row Containing Sortable Column List
#'
#' @param ns Module namespace
#'
#' @return `[material_row]` Row with sortable bucket list
#'
#' @noRd
mod_table_ui_row_sort <- function(ns) {

  # Create a sortable list of hidden table columns
  col_list_all <- sortable::add_rank_list(
    "Available Columns",
    labels = list(
      vac = "Vaccinated",
      symp = "Symptomatic",
      test = "Tested"
    ),
    input_id = ns("col_all")
  )

  # Create a sortable list of shown table columns
  col_list_select <- sortable::add_rank_list(
    "Selected Columns",
    labels = c(inf = "Infected", detect = "Detected"),
    input_id = ns("col_select")
  )

  # Combine into bucket list
  col_bucket <- sortable::bucket_list(
    header = NULL,
    col_list_all,
    col_list_select,
    group_name = "col_bucket"
  )

  # Show bucket list
  shinymaterial::material_row(
    shinymaterial::material_card(title = "Column Order", col_bucket)
  )
}


#' Create Row containing Joint Distribution Table
#'
#' @param ns Module namespace
#'
#' @return `[material_row]`
#'
#' @noRd
mod_table_ui_row_table <- function(ns) {
  # Show table
  shinymaterial::material_row(
    shinymaterial::material_card(
      reactable::reactableOutput(ns("table"))
    )
  )
}

#' Summarize Distribution into Table
#'
#' @param dist `[datatable]` A data table containing the full joint distribution
#'
#' @param cols `[character()]` Columns to summarize and show
#'
#' @return `[datatable]` A data table with the chosen columns and summarized
#'   probabilities
#'
#' @noRd
dist_data <- function(dist, cols) {
  dist_summ <- suppressWarnings(data.table::setorderv(
    dist[, .(p = sum(.SD$p)), by = cols],
    cols = cols,
    order = -1L,
    na.last = TRUE
  ))

  dist_summ[]
}

dist_yn <- function(data, cols) {
  data[, c(cols) := lapply(.SD, lgl_to_str), .SDcols = cols][]
}

lgl_to_str <- function(x, true = "Yes", false = "No", na = "—") {
  data.table::fifelse(x, yes = true, no = false, na = na)
}



#' Simplified Column Definitions for {reactable}
#'
#' @param name `[character(1)]` Column name
#'
#' @param format `[colFormat]` A reactable `colFormat()` definition
#'
#' @return `[colDef]` A {reactable} column definition
#'
#' @noRd
react_col_def <- function(name = NULL, format = NULL) {
  reactable::colDef(
    name,
    sortNALast = TRUE,
    format = format,
    na = "—"
  )
}

## To be copied in the UI
# mod_table_ui("table_ui_1")

## To be copied in the server
# mod_table_server("table_ui_1")
