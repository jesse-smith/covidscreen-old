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
    intro_card(),
    setting_card(ns),
    # Tests/% Detected Slope
    slope_card(ns),
    # Undetected infections by total tests
    two_d_card(ns),
    # 3D Exploration
    three_d_card(ns)
  )
}

#' plot_output Server Functions
#'
#' @noRd
mod_plot_output_server <- function(id, params){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Create tests by undetected data
    freqs <- reactive(
      calc_freq(n = input$n_org, params = params())(),
      label = "calc_freq()"
    )

    output$plt_slopes <- plotly::renderPlotly(plot_slopes(freqs()))

    # Plot undetected
    output$plt_undetected <- plotly::renderPlotly(
      plot_undetected(
        freqs(),
        x = input$undetected_x,
        y = input$undetected_y,
        target_cases = input$n_cases_target,
        target_tests = input$n_tests_target
      )
    )

    # Plot 3D
    output$plt_3d <- plotly::renderPlotly({
      plot_3d(freqs(), z = input$drop_3d)
    })
  })
}

intro_card <- function() {
  material_card(
    title = "Overview",
    tags$p(
      "The 'Scenarios' tab allows you to generate 'what-if' situations and compare ",
      "screening timings within each situation. There are two main outcomes of interest: ",
      "undetected active cases and total tests. We are interested in the former ",
      "because these are the individuals with the potential to spread COVID-19 to ",
      "others. We are also interested in the number of tests performed, as this is ",
      "the primary modifiable cost in our scenario. Ideally, we would like to minimize ",
      "both; however, they are generally inversely related, with more testing leading ",
      "to fewer undetected active cases. The task, then, is to find an appropriate balance."
    )
  )
}

setting_card <- function(ns) {
  material_card(
    title = "Setting",
    tags$br(),
    material_number_box(
      ns("n_org"),
      label = "Organization size (people)",
      initial_value = 100,
      min_value = 1,
      max_value = Inf
    ),
    tags$br(),
    material_number_box(
      ns("n_cases_target"),
      label = "Maximum Undetected Cases",
      initial_value = 1,
      min_value = 0,
      max_value = Inf
    ),
    tags$br(),
    material_number_box(
      ns("n_tests_target"),
      label = "Maximum Tests per Day",
      initial_value = 10,
      min_value = 0,
      max_value = Inf
    )
  )
}

slope_card <- function(ns) {
  material_card(
    title = "Testing Benefits by Vaccination Status",
    tags$p(HTML(
      "The number of tests needed to detect a certain number of cases changes ",
      "with the scenario. We can see how many additional cases we detect per ",
      "test performed. This increases for <b>both</b> groups as more people ",
      "are vaccinated in the organization, although the relative benefits stay ",
      "the same. Here, higher is better."
    )),
    tags$br(),
    plotly::plotlyOutput(ns("plt_slopes"))
  )
}

two_d_card <- function(ns) {
  material_card(
    # Title
    title = "Costs & Benefits by Test Frequencies",
    tags$p(
      "Undetected active cases are the primary source of within-organization COVID-19 ",
      "risk. The goal of any screening strategy is to minimize these cases."
    ),
    tags$p(
      "The graph below shows undetected cases across both vaccinated and ",
      "unvaccinated screening strategies, as well as total tests. You can change which ",
      "screening strategy is shown on the x-axis using the dropdown; you may also ",
      "directly compare against total tests. The benefits of testing are ",
      "directly (linearly) related to the number of tests performed. Here, ",
      "lower is better."
    ),
    tags$br(),
    # Dropdown
    material_dropdown(
      ns("undetected_x"),
      label = "X-axis",
      choices = c(
        "Unvaccinated Testing Frequency" = "ut",
        "Vaccinated Test Frequency"      = "vt",
        "Total Tests"                    = "t",
        "Undetected Active Cases"        = "u"
      ),
      selected = "ut",
      multiple = FALSE
    ),
    material_dropdown(
      ns("undetected_y"),
      label = "Y-axis",
      choices = c(
        "Undetected Active Cases" = "u",
        "Total Tests" = "t"
      ),
      selected = "u",
      multiple = FALSE
    ),
    # Undetected Cases
    plotly::plotlyOutput(ns("plt_undetected"))
  )
}

three_d_card <- function(ns) {
  material_card(
    title = "3D Costs and Benefits by Test Frequencies",
    tags$p(
      "The graph below gives a 3D view of total tests or detected cases by ",
      "testing frequency. This view emphasizes the change in cost or benefit ",
      "of testing more frequently in a group. Here, lower tests are better, ",
      "but higher detected cases are as well."
    ),
    tags$br(),
    material_dropdown(
      ns("drop_3d"),
      label = "Z-axis",
      choices = c("Tests" = "t", "Detected Cases" = "d"),
      selected = "t",
      multiple = FALSE
    ),
    plotly::plotlyOutput(ns("plt_3d"))
  )
}

plot_slopes <- function(data) {
  slopes <- calc_slopes(data)
  ratio <- slopes[[1]] / slopes[[2]]
  nms <- names(slopes)
  plotly::plot_ly(
    x = nms,
    y = slopes,
    color = nms,
    type = "bar",
    hovertemplate = "%{y:.2f}%"
  ) %>%
    plotly::layout(
      yaxis = list(title = "% Active Cases Detected per Test"),
      annotations = list(
       x = 0.4,
       xanchor = "left",
       align = "left",
       y = slopes[[2L]] + abs(diff(slopes))/2,
       text = paste0(
         "Testing unvaccinated people\n",
         "has <b>", round(ratio, 1), "x the benefit</b>\n",
         "of testing vaccinated people"
        ),
       showarrow = FALSE
      )
    )
}

plot_undetected <- function(data, x, y, target_cases, target_tests) {
  # Text shown on hover
  hovertemplate <- c(
    "%{text}<extra></extra>"
  )

  if (x == "v") {
    pal <- c(viridisLite::viridis(data.table::uniqueN(data$t_u)), "#cc1e27", "#000000")
    names(pal) <- c(levels(data$t_u_fct), "Active", "Target")
  } else {
    pal <- c(viridisLite::viridis(data.table::uniqueN(data$t_v)), "#cc1e27", "#000000")
    names(pal) <- c(levels(data$t_v_fct), "Active", "Target")
  }

  # Build plot
  plt <- plotly::plot_ly(
    data = data,
    x = switch(x, ut = ~t_u, vt = ~t_v, t = ~tests, u = ~undetected),
    y = if (y == "u") (~undetected) else (~tests),
    color = if (x == "ut") (~t_v_fct) else (~t_u_fct),
    colors = pal,
    size = if (x %in% c("t", "u")) (~t_v) else (~tests),
    text = ~freq_txt,
    type = if (x %in% c("t", "u")) "scatter" else "bar",
    mode = if (x %in% c("t", "u")) "markers+lines" else NULL,
    opacity = 0.75,
    hovertemplate = hovertemplate
  ) %>%
    plotly::add_lines(
      y = if (y == "u") target_cases else target_tests,
      color = "Target",
      colors = "#000000",
      name = paste("Target", if (y == "u") "Cases" else "Tests"),
      hovertemplate = paste0(
        "Target: %{y:.1f} ",
        if (y == "u") "Cases" else "Tests",
        "<extra></extra>"
      )
    )

  if (y == "u") {
    plt <- plotly::add_lines(
      plt,
      y = ~n_inf,
      color = "Active",
      colors = "#cc1e27",
      name = "Active Cases",
      text = ~pct_active,
      hovertemplate = "Active Cases: %{y:.1f} (%{text:.1f}&#37; of organization)<extra></extra>"
    )
  }

  # X title is reactive to dropdown
  x_title <- switch(
    x,
    ut = "Unvaccinated Test Frequency (Days)",
    vt = "Vaccinated Test Frequency (Days)",
    t  = "Total Tests",
    u  = "Undetected Active Cases"
  )
  y_title <- switch(
    y,
    t = "Total Tests",
    u = "Undetected Active Cases"
  )
  lgnd_title <- paste0(
    (if (x == "ut") "V" else ("Unv")),
    "accinated Test\nFrequency"
  )



  # Customize layout
  plotly::layout(
    plt,
    legend = list(title = list(text = lgnd_title)),
    xaxis = list(
      title = x_title,
      rangemode = if (x %in% c("t", "u")) "tozero" else "normal"
    ),
    yaxis = list(
      title = y_title,
      rangemode = "tozero"
    )
  )
}

plot_3d <- function(data, z) {

  hovertemplate <- paste0(
    (if (z == "t") "Tests: " else "Detected Cases: "),
    "%{text:.1f} (%{marker.color:.1f}&#37; of ",
    (if (z == "t") "organization" else "active cases"),
    ")<extra></extra>"
  )

  plt <- plotly::plot_ly(
    data = data,
    x = ~t_v,
    y = ~t_u,
    z = if (z == "t") ~tests else ~detected,
    color = if (z == "t") ~pct_tested else ~pct_detected,
    size = if (z == "t") ~detected else ~tests,
    text = if (z == "t") ~tests else ~detected,
    mode = "markers",
    type = "scatter3d",
    hovertemplate = hovertemplate
  )

  plt_layout <- plotly::layout(
    p = plt,
    scene = list(
      xaxis = list(title = "Vax Freq. (Days)"),
      yaxis = list(title = "Unvax Freq. (Days)"),
      zaxis = list(
        title = if (z == "t") "Total Tests" else "Detected Cases",
        rangemode = "tozero",
        range = c(0, if (z == "t") data$n[[1L]] else data$n_inf[[1L]])
      ),
      aspectmode = "cube",
      camera = list(eye = list(x = 1.75, y = 1.75, z = 0.5))
    )
  )

  plotly::hide_colorbar(plt_layout)
}

calc_slopes <- function(data) {

  # Interested in pct_detected / tests
  vars <- c("tests", "pct_detected")
  # Get a testing level - doesn't matter which one, so first one
  t <- data$t_v[[1L]]

  # Unvaccinated data (within a vaccinated testing level)
  u_data <- data[data$t_v == t, ..vars]

  # Vaccinated data (within an unvaccinated testing level)
  v_data <- data[data$t_u == t, ..vars]

  c(
    Unvaccinated = calc_slope(u_data, vars),
    Vaccinated = calc_slope(v_data, vars)
  )
}

calc_slope <- function(data, vars) {
  diffs <- data[NROW(data), ..vars] - data[1L, ..vars]

  diffs$pct_detected / diffs$tests
}

calc_intercepts <- function(data) {
  n <- NROW(data)
  test_min   <- data$tests[[n]]
  detect_min <- data$pct_detected[[n]]

  detect_min - test_min * calc_slopes(data)
}
