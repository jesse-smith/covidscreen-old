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
    material_page_ui(
      shinymaterial::material_row(
        shinymaterial::material_column(
          width = 4,
          mod_input_params_ui("mod_input_params_sim")
        ),
        shinymaterial::material_column(
          width = 8,
          mod_table_ui("mod_table_probs")
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

material_page_ui <- function(...) {
  shinymaterial::material_page(
    title = "COVID-19 Screening",
    nav_bar_fixed = TRUE,
    primary_theme_color = "#1565c0",
    secondary_theme_color = "#82b1ff",
    ...
  )
}
