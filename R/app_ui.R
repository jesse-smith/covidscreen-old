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
      tags$div(style = "padding: 0.9em", tags$br(), mod_input_params_ui("params")),
      material_card(
        material_tabs(c(About = "about", Table = "table"))
      ),
      material_row(tags$div(material_column(
        offset = 1,
        width = 10,
        material_tab_content("about", mod_about_ui("about")),
        material_tab_content("table", mod_table_ui("table"))
      )))
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
    favicon(ext = "png"),
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
    title = "COVID Screening",
    nav_bar_fixed = FALSE,
    primary_theme_color = "#cc1e27",
    secondary_theme_color = "#e57373",
    ...,
    material_side_nav(
      sidebar_panel,
      fixed = TRUE,
      image_source = "www/favicon_resized.png",
      background_color = "grey lighten-4"
    )
  )
}
