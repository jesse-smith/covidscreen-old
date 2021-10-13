#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    material_sidebar_page(
      tags$div(style = "padding: 0.9em", tags$br()),
      material_card(
        material_tabs(c(Home = "home", Scenarios = "scenarios", Explore = "explore"))
      ),
      material_row(
        material_tab_content("home", "intro text"),
        material_tab_content(
          "scenarios",
          material_column(
            mod_input_params_ui("scenarios", interval = FALSE),
            width = 4
          ),
          material_column(mod_plot_output_ui("scenarios"), width = 8)
        ),
        material_tab_content(
          "explore",
          material_column(mod_input_params_ui("explore"), width = 4),
          material_column(mod_table_ui("table"), width = 8)
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'covidscreen'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

material_sidebar_page <- function(
  sidebar_panel,
  ...,
  position = c("left", "right"),
  fluid = TRUE
) {
  material_page(
    title = "COVID Screening Strategies",
    nav_bar_fixed = FALSE,
    primary_theme_color = "#cc1e27",
    secondary_theme_color = "#e57373",
    ...
  )
}
