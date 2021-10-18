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
    material_card(
      title = "Setting",
      tags$br(),
      material_number_box(
        ns("n_org"),
        label = "Organization size (people)",
        initial_value = 100,
        min_value = 1,
        max_value = Inf
      )
    ),
    # Undetected infections by total tests
    undetected_card(ns),
    # 3D Exploration
    material_card(
      title = "3D Test Frequencies",
      material_dropdown(
        ns("drop_3d"),
        label = "Z-axis",
        choices = c("Tests" = "t", "Detected Cases" = "d"),
        selected = "t",
        multiple = FALSE
      ),
      plotly::plotlyOutput(ns("plt_3d"))
    )
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

    # Plot undetected
    output$plt_undetected <- plotly::renderPlotly(
      plot_undetected(freqs(), x = input$undetected_x, y = input$undetected_y)
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

undetected_card <- function(ns) {
  material_card(
    # Title
    title = "Undetected Active Cases",
    tags$p(
      "Undetected active cases are the primary source of within-organization COVID-19 ",
      "risk. The goal of any screening strategy is to minimize these cases."
    ),
    tags$p(
      "The graph below shows undetected cases across both vaccinated and ",
      "unvaccinated screening strategies, as well as total tests. You can change which ",
      "screening strategy is shown on the x-axis using the dropdown; you may also ",
      "directly compare against total tests. Generally, the benefits of testing are ",
      "directly (linearly) related to the number of tests performed."
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

plot_undetected <- function(data, x, y) {
  # Text shown on hover
  hovertemplate <- c(
    "%{text}<extra></extra>"
  )

  if (x == "v") {
    pal <- c(viridisLite::viridis(data.table::uniqueN(data$t_u)), "#000000")
    names(pal) <- c(levels(data$t_u_fct), "Active")
  } else {
    pal <- c(viridisLite::viridis(data.table::uniqueN(data$t_v)), "#000000")
    names(pal) <- c(levels(data$t_v_fct), "Active")
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
  )

  if (y == "u") {
    plt <- plotly::add_lines(
      plt,
      y = ~n_inf,
      color = "Active",
      colors = "#000000",
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
