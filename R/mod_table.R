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
    mod_table_ui_table(ns),
    mod_table_ui_sort(ns)
  )
}

#' table Server Functions
#'
#' @param id `[character(1)]` A unique identifier for the module instance
#'
#' @param dist `[data.table]` A distribution from `calc_dist()`
#'
#' @noRd
mod_table_server <- function(id, dist){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    cols <- c("vac", "inf", "symp", "test", "detect")

    # Calculate distribution
    dist_chr <- reactive(dist_yn(dist(), cols = cols))

    # Get grouped columns
    grp <- reactive(c(
      input$vac_group,
      input$inf_group,
      input$symp_group,
      input$test_group,
      input$detect_group
    ))

    col_grouped <- reactive(intersect(cols[grp()], input$col_select))
    col_select  <- reactive(setdiff(input$col_select, col_grouped()))

    # Output table
    output$table <- renderReactable({
      material_spinner_show(session, ns("table"))
      tbl <- create_table(
        dist_chr(),
        select  = col_select(),
        grouped = col_grouped()
      )
      material_spinner_hide(session, ns("table"))
      tbl
    })
  })
}

#' Create Row containing Joint Distribution Table
#'
#' @param ns Module namespace
#'
#' @return `[material_row]`
#'
#' @noRd
mod_table_ui_table <- function(ns, depth = NULL) {
  # Show table
  material_card(
    depth = depth,
    reactableOutput(ns("table"))
  )
}

#' Create Row Containing Sortable Column List
#'
#' @param ns Module namespace
#'
#' @return `[material_row]` Row with sortable bucket list
#'
#' @noRd
mod_table_ui_sort <- function(ns, depth = NULL) {

  vac <- tags$div(
    "Vaccinated",
    material_switch(ns("vac_group"), on_label = "Grouped")
  )

  inf <- tags$div(
    "Infected",
    material_switch(ns("inf_group"), on_label = "Grouped", initial_value = TRUE)
  )

  symp <- tags$div(
    "Symptomatic",
    material_switch(ns("symp_group"), on_label = "Grouped")
  )

  test <- tags$div(
    "Tested",
    material_switch(ns("test_group"), on_label = "Grouped")
  )

  detect <- tags$div(
    "Detected",
    material_switch(ns("detect_group"), on_label = "Grouped")
  )

  # Create a sortable list of hidden table columns
  col_list_all <- add_rank_list(
    "Available",
    labels = list(
      vac = vac,
      symp = symp
    ),
    input_id = ns("col_all")
  )

  # Create a sortable list of shown table columns
  col_list_select <- add_rank_list(
    "Selected",
    labels = list(
      test = test,
      detect = detect,
      inf = inf
    ),
    input_id = ns("col_select")
  )

  # Combine into bucket list
  col_bucket <- bucket_list(
    header = NULL,
    col_list_all,
    col_list_select,
    group_name = "col_bucket"
  )

  # Show bucket list
  material_card(title = "Columns", depth = depth, col_bucket)
}

#' Convert `logical` Values to `"Yes"`/`"No"`/`"-"` in `data.table` Distribution
#'
#' @param data `[data.table]` Distribution from `calc_dist()`
#'
#' @param cols `[character]` Columns to convert
#'
#' @return The modified `data.table`
#'
#' @noRd
dist_yn <- function(data, cols) {
  data[, c(cols) := lapply(.SD, lgl_to_str), .SDcols = cols][]
}

#' Convert `logical` Vector to `character`
#'
#' @param x `[logical]` Vector to convert
#'
#' @param true,false,na `[character(1)]` The value to assign to
#'   `TRUE`/`FALSE`/`NA`
#'
#' @return `[character]` The converted vector
lgl_to_str <- function(x, true = "Yes", false = "No", na = "-") {
  fifelse(x, yes = true, no = false, na = na)
}

#' Create a `reactable` Table from a Distribution
#'
#' @param dist `[data.table]` A distribution from `calc_dist()`
#'
#' @param select `[character]` Columns for unconditional probabilities and
#'   grouping
#'
#' @param grouped `[character]` Columns for conditional probabilities within
#'   groups defined by `select`
#'
#' @return `[reactable]` A table containing a distribution view
create_table <- function(dist, select, grouped) {

  # Define all possible table columns
  col_def <- list(
    vac = react_col_def("Vaccinated"),
    inf = react_col_def("Infected"),
    symp = react_col_def("Symptomatic"),
    test = react_col_def("Tested"),
    detect = react_col_def("Detected"),
    p = react_col_def(
      "%",
      aggregate = "sum",
      format = colFormat(digits = 1, percent = TRUE)
    )
  )

  # Create distribution from `select`
  dist_grp <- dist_data(dist, cols = select)

  # Get variable columns
  data_grp <- dist_grp[, -"p"]

  # Create conditional probability table
  detail_fn <- function(i) {
    # Get nested (conditional) probabilities for a given row
    dist_nest <- dist[data_grp[i], on = select]
    # Return a `reactable` with the `grouped` distribution within this row
    tags$div(
      style = "padding: 16px",
      reactable(
        dist_data(dist_nest, cols = grouped, norm = TRUE),
        columns = col_def[c(grouped, "p")],
        outlined = TRUE,
        highlight = TRUE,
        compact = TRUE
      )
    )
  }

  # Return a `reactable`
  reactable(
    dist_grp,
    details = detail_fn,
    columns = col_def[c(select, "p")],
    resizable = TRUE,
    defaultSortOrder = "desc",
    highlight = TRUE,
    compact = TRUE,
    wrap = FALSE,
    onClick = "expand"
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
  dist_summ <- suppressWarnings(setorderv(
    dist[, .(p = sum(.SD$p)), by = c(cols)][],
    cols = cols,
    order = -1L,
    na.last = TRUE
  ))

  if (norm) dist_summ[, "p" := .SD$p / sum(.SD$p)][] else dist_summ[]
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
  colDef(
    name,
    aggregate = aggregate,
    sortNALast = TRUE,
    format = format,
    na = "-"
  )
}
