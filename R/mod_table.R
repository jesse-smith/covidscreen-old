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

    cols <- c("vac", "inf", "symp", "test", "detect")

    # Calculate distribution
    dist <- reactive(dist_yn(
      do.call(calc_dist, dist_params()),
      cols = cols
    ))

    # Get grouped columns
    grp <- reactive(c(
      input$vac_group,
      input$inf_group,
      input$symp_group,
      input$test_group,
      input$detect_group
    ))

    col_grouped <- reactive(intersect(cols[grp()], input$col_select))
    col_select <- reactive(setdiff(input$col_select, col_grouped()))

    # Output table
    output$table <- reactable::renderReactable(
      create_table(
        dist(),
        select  = col_select(),
        grouped = col_grouped()
      )
    )
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

  vac <- tags$div(
    HTML(paste(tags$b("V"), "- Vaccinated")),
    shinymaterial::material_switch(ns("vac_group"), on_label = "Grouped")
  )

  inf <- tags$div(
    HTML(paste(tags$b("I"), "- Infected")),
    shinymaterial::material_switch(ns("inf_group"), on_label = "Grouped", initial_value = TRUE)
  )

  symp <- tags$div(
    HTML(paste(tags$b("S"), "- Symptomatic")),
    shinymaterial::material_switch(ns("symp_group"), on_label = "Grouped")
  )

  test <- tags$div(
    HTML(paste(tags$b("T"), "- Tested")),
    shinymaterial::material_switch(ns("test_group"), on_label = "Grouped")
  )

  detect <- tags$div(
    HTML(paste(tags$b("D"), "- Detected")),
    shinymaterial::material_switch(ns("detect_group"), on_label = "Grouped")
  )

  # Create a sortable list of hidden table columns
  col_list_all <- sortable::add_rank_list(
    "Available",
    labels = list(
      vac = vac,
      symp = symp
    ),
    input_id = ns("col_all")
  )

  # Create a sortable list of shown table columns
  col_list_select <- sortable::add_rank_list(
    "Selected",
    labels = list(
      test = test,
      detect = detect,
      inf = inf
    ),
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
    shinymaterial::material_card(title = "Columns", col_bucket)
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
    shinymaterial::material_card(reactable::reactableOutput(ns("table")))
  )
}

dist_yn <- function(data, cols) {
  data[, c(cols) := lapply(.SD, lgl_to_str), .SDcols = cols][]
}

lgl_to_str <- function(x, true = "Yes", false = "No", na = "—") {
  data.table::fifelse(x, yes = true, no = false, na = na)
}

create_table <- function(dist, select, grouped) {

  # Define all possible table columns
  col_def <- list(
    vac = react_col_def("V"),
    inf = react_col_def("I"),
    symp = react_col_def("S"),
    test = react_col_def("T"),
    detect = react_col_def("D"),
    p = react_col_def(
      "%",
      aggregate = "sum",
      format = reactable::colFormat(digits = 1, percent = TRUE)
    )
  )

  dist_grp <- dist_data(dist, cols = select)

  data_grp <- dist_grp[, -"p"]

  detail_fn <- function(i) {
    dist_nest <- dist[data_grp[i], on = select]
    tags$div(
      style = "padding: 16px",
      reactable::reactable(
        dist_data(dist_nest, cols = grouped, norm = TRUE),
        columns = col_def[c(grouped, "p")],
        outlined = TRUE,
        highlight = TRUE,
        compact = TRUE
      )
    )
  }

  reactable::reactable(
    dist_grp,
    details = detail_fn,
    columns = col_def[c(select, "p")],
    resizable = TRUE,
    defaultSortOrder = "desc",
    pagination = FALSE,
    highlight = TRUE,
    compact = TRUE
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
dist_data <- function(dist, cols, norm = FALSE) {
  dist_summ <- suppressWarnings(data.table::setorderv(
    dist[, .(p = sum(.SD$p)), by = c(cols)][],
    cols = cols,
    order = -1L,
    na.last = TRUE
  ))

  if (norm) dist_summ[, p := .SD$p / sum(.SD$p)][] else dist_summ[]
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
react_col_def <- function(name = NULL, aggregate = NULL, format = NULL) {
  reactable::colDef(
    name,
    aggregate = aggregate,
    sortNALast = TRUE,
    format = format,
    na = "—"
  )
}

## To be copied in the UI
# mod_table_ui("table_ui_1")

## To be copied in the server
# mod_table_server("table_ui_1")
