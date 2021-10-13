#' plot_output UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_output_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Undetected infections by total tests

    # Total tests output
    plt_card(
      title = "Tests per Day (by Testing Frequency)",
      switch_id = ns("plt_tests_3d"),
      plot_id = ns("plt_tests")
    )
    # Undetected infections output

    # False positives output
  )
}

#' plot_output Server Functions
#'
#' @noRd
mod_plot_output_server <- function(id, params){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    test_freq <- reactive(calc_test_df(n = 1000, params = params()))

    inf_freq <- reactive(NULL)

    output$plt_tests <- plotly::renderPlotly({
      scatter_tests(test_freq(), z = input$plt_tests_3d)
    })
  })
}

plt_card <- function(plot_id, switch_id = NULL, title = NULL, ...) {
  material_card(
    title = title,
    if (!is.null(switch_id)) material_switch(switch_id, off_label = "2D", on_label = "3D"),
    if (!is.null(switch_id)) tags$br(),
    plotly::plotlyOutput(plot_id)
  )
}

scatter_tests <- function(data, z = FALSE) {

  plt_args <- scatter_test_plt(
    list(
      data = data,
      x = ~t_vac,
      y = ~t_unvac,
      color = ~`% Tested`,
      size = ~Tests,
      text = ~Tests,
      sizes = c(36, 360),
      mode = "markers",
      hovertemplate = paste("<b>Tests: %{text:.0f}</b> (%{marker.color:.0f}&#37;)<extra></extra>")
    ),
    z = if (z) (~Tests) else NULL
  )

  plt <- do.call(plotly::plot_ly, args = plt_args)

  layout_args <- scatter_test_layout(plt, z = if (z) "Tests" else NULL)

  do.call(plotly::layout, args = layout_args)
}

calc_test_df <- function(n, params) {

  # Create parameter grid for testing frequencies
  t_df <- expand_freq(params$test$p_asymp_vac, params$test$p_asymp_unvac)

  # Calculate distribution for each parameter set
  t_df[,
    Tests := calc_tests(
      n,
      do.call(calc_dist, replace_freq(params, f_v = .SD$t_vac, f_u = .SD$t_unvac))
    ),
    by = seq_len(NROW(t_df))
  ][,
    `% Tested` := 100 * .SD$Tests / n
  ][]
}

replace_freq <- function(params, f_v, f_u) {
  modifyList(
    params,
    val = list(
      test = list(
        p_asymp_vac = 1 / f_v,
        p_asymp_unvac = 1 / f_u
      )
    )
  )
}

scatter_test_plt <- function(args, z = NULL) {
  if (is.null(z)) {
    c(args, type = "scatter")
  } else {
    c(args, z = z, type = "scatter3d")
  }
}

scatter_test_layout <- function(p, z = NULL) {

  axes <- scatter_test_axes(z = z)

  if (is.null(z)) {
    append(list(p = p), axes)
  } else {
    list(p = p, scene = axes)
  }
}

scatter_test_axes <- function(z = NULL) {
  if (is.null(z)) {
    list(
      xaxis = list(title = "Vaccinated Test Frequency (Days)"),
      yaxis = list(title = "Unvaccinated Test Frequency (Days)")
    )
  } else {
    list(
      xaxis = list(title = "Vaccinated"),
      yaxis = list(title = "Unvaccinated"),
      zaxis = list(title = z, rangemode = "tozero")
    )
  }
}

scatter_inf <- function(data, z = FALSE) {
  plt_args <- scatter_test_plt(
    list(
      data = data,
      x = ~t_vac,
      y = ~t_unvac,
      color = ~`% Undetected`,
      size = ~undetect_inf,
      text = ~undetect_inf,
      sizes = c(36, 360),
      mode = "markers",
      hovertemplate = paste("<b>Tests: %{text:.0f}</b> (%{marker.color:.0f}&#37;)<extra></extra>")
    ),
    z = z
  )



}

## To be copied in the UI
# mod_plot_output_ui("plot_output_ui_1")

## To be copied in the server
# mod_plot_output_server("plot_output_ui_1")
