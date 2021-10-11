#' input_params UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_params_ui <- function(id, depth = NULL, interval = FALSE){
  ns <- NS(id)
  tagList(
    mod_input_params_ui_intervention(ns, depth = depth),
    mod_input_params_ui_context(ns, depth = depth),
    mod_input_params_ui_advanced(ns, depth = depth, interval = interval)
  )
}

#' input_params Server Functions
#'
#' @noRd
mod_input_params_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Intervention
    vac_org <- reactive(input$vac_org * 0.01)
    test_p_asymp_vac   <- reactive(1/input$test_freq_vac)
    test_p_asymp_unvac <- reactive(1/input$test_freq_unvac)

    # Context
    p_incid  <- reactive(input$incid / 1e5)
    vac_comm <- reactive(input$vac_comm * 0.01)

    # Advanced
    vac_eff <- reactive(input$vac_eff * 0.01)
    inf_t_symp    <- reactive(input$inf_t_symp)
    inf_t_presymp <- reactive(input$inf_t_presymp)
    symp_p_inf_vac   <- reactive((100 - input$asymp_p_inf_vac)   * 0.01)
    symp_p_inf_unvac <- reactive((100 - input$asymp_p_inf_unvac) * 0.01)
    symp_p_uninf     <- reactive((100 - input$asymp_p_uninf)     * 0.01)
    test_p_symp <- reactive(input$test_p_symp * 0.01)
    detect_sens <- reactive(input$detect_sens * 0.01)
    detect_spec <- reactive(input$detect_spec * 0.01)

    # Group into `calc_dist()` inputs
    vac <- reactive(list(
      p_comm = vac_comm(),
      p_org  = vac_org(),
      eff    = vac_eff()
    ))

    inf <- reactive(list(
      p_incid   = p_incid(),
      t_symp    = inf_t_symp(),
      t_presymp = inf_t_presymp()
    ))

    symp <- reactive(list(
      p_inf_vac   = symp_p_inf_vac(),
      p_inf_unvac = symp_p_inf_unvac(),
      p_uninf     = symp_p_uninf()
    ))

    test <- reactive(list(
      p_symp        = test_p_symp(),
      p_asymp_vac   = test_p_asymp_vac(),
      p_asymp_unvac = test_p_asymp_unvac()
    ))

    detect <- reactive(list(
      sens = detect_sens(),
      spec = detect_spec()
    ))

    # Return as `list`
    reactive(list(
      vac    = vac(),
      inf    = inf(),
      symp   = symp(),
      test   = test(),
      detect = detect()
    ))
  })
}

mod_input_params_ui_intervention <- function(ns, depth = NULL) {
  material_card(
    depth = depth,
    material_button(
      ns("btn_interventions"),
      label = "Interventions",
      icon = "tune"
    ),
    conditionalPanel(
      condition = "input.btn_interventions % 2 == 0",
      ns = ns,
      tags$br(),
      material_number_box(
        ns("test_freq_vac"),
        label = "Testing Frequency - Vaccinated (Days)",
        min_value = 1,
        max_value = 365,
        initial_value = 7,
        step_size = 1
      ),
      material_number_box(
        ns("test_freq_unvac"),
        label = "Testing Frequency - Unvaccinated (Days)",
        min_value = 1,
        max_value = 365,
        initial_value = 7,
        step_size = 1
      ),
      mat_slider(
        ns("vac_org"),
        label = "Vaccinated (% in Organization)",
        min_value = 0,
        max_value = 100,
        initial_value = 50,
        step_size = 1
      )
    )
  )
}

mod_input_params_ui_context <- function(ns, depth = NULL) {
  material_card(
    depth = depth,
    material_button(
      ns("btn_context"),
      label = "Community Context",
      icon  = "people"
    ),
    conditionalPanel(
      condition = "input.btn_context % 2 == 0",
      ns = ns,
      tags$br(),
      material_slider(
        ns("incid"),
        label = "Case Rate (per 100k per Day)",
        min_value = 0,
        max_value = 1e5,
        initial_value = 100,
        step_size = 10
      ),
      material_slider(
        ns("vac_comm"),
        label = "Vaccinated (% in Community)",
        min_value = 0,
        max_value = 100,
        initial_value = 50,
        step_size = 1
      )
    )
  )
}

mod_input_params_ui_advanced <- function(ns, depth = NULL, interval = FALSE) {
  material_card(
    depth = depth,
    material_button(
      ns("btn_advanced"),
      "Advanced Settings",
      icon = "settings"
    ),
    conditionalPanel(
      condition = "input.btn_advanced % 2 != 0",
      ns = ns,
      shinyWidgets::chooseSliderSkin("Flat", color = "#e57373"),
      material_card(
        depth = depth,
        material_slider(
          ns("vac_eff"),
          label = "Vaccine Efficacy (%)",
          min_value = 0,
          max_value = 100,
          initial_value = 70,
          step_size = 1
        ),
        if (interval) {
          sliderInput(
            ns("vac_eff_range"),
            label = NULL,
            min = 0,
            max = 100,
            value = c(50, 95),
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        }
      ),
      material_card(
        depth = depth,
        material_number_box(
          ns("inf_t_symp"),
          label = "Symptomatic Period (Days)",
          min_value = 1,
          max_value = 21,
          initial_value = 10,
          step_size = 1
        )
      ),
      material_card(
        depth = depth,
        material_number_box(
          ns("inf_t_presymp"),
          label = "Pre-symptomatic Period (Days)",
          min_value = 0,
          max_value = 14,
          initial_value = 3,
          step_size = 1
        )
      ),
      material_card(
        depth = depth,
        material_slider(
          ns("asymp_p_inf_unvac"),
          label = "% Asymptomatic: Unvaccinated Infections",
          min_value = 0,
          max_value = 100,
          initial_value = 50,
          step_size = 1
        ),
        if (interval) {
          sliderInput(
            ns("asymp_p_inf_unvac_range"),
            label = NULL,
            min = 0,
            max = 100,
            value = c(30, 70),
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        }
      ),
      material_card(
        depth = depth,
        material_slider(
          ns("asymp_p_inf_vac"),
          label = "% Asymptomatic: Vaccinated Infections",
          min_value = 0,
          max_value = 100,
          initial_value = 70,
          step = 1
        ),
        if (interval) {
          sliderInput(
            ns("asymp_p_inf_vac_range"),
            label = NULL,
            min = 0,
            max = 100,
            value = c(50, 90),
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        }
      ),
      material_card(
        depth = depth,
        material_slider(
          ns("asymp_p_uninf"),
          label = "% Asymptomatic: Uninfected",
          min_value = 90,
          max_value = 100,
          initial_value = 98,
          step_size = 1
        ),
        if (interval) {
          sliderInput(
            ns("asymp_p_uninf_range"),
            label = NULL,
            min = 90,
            max = 100,
            value = c(97, 99),
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        }
      ),
      material_card(
        depth = depth,
        material_slider(
          ns("test_p_symp"),
          label = "% of Symptomatics Tested",
          min_value = 0,
          max_value = 100,
          initial_value = 95,
          step_size = 1
        ),
        if (interval) {
          sliderInput(
            ns("test_p_symp_range"),
            label = NULL,
            min = 0,
            max = 100,
            value = c(90, 100),
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        }
      ),
      material_card(
        depth = depth,
        material_slider(
          ns("detect_sens"),
          label = "Test Sensitivity (%)",
          min_value = 50,
          max_value = 100,
          initial_value = 85,
          step_size = 1
        ),
        if (interval) {
          sliderInput(
            ns("detect_sens"),
            label = NULL,
            min = 50,
            max = 100,
            value = c(70, 90),
            step = 1,
            ticks = FALSE,
            width = "100%"
          )
        }
      ),
      material_card(
        depth = depth,
        material_slider(
          ns("detect_spec"),
          label = "Test Specificity (%)",
          min_value = 90,
          max_value = 100,
          initial_value = 99.7,
          step = 0.1
        ),
        if (interval) {
          sliderInput(
            ns("detect_spec_range"),
            label = NULL,
            min = 90,
            max = 100,
            value = c(99, 100),
            step = 0.1,
            ticks = FALSE,
            width = "100%"
          )
        }
      )
    )
  )
}

mat_slider <- function(
  input_id,
  label,
  min_value,
  max_value,
  initial_value,
  step_size = 1
) {
  if (NROW(initial_value) != 2) {
    material_slider(
      input_id = input_id,
      label = label,
      min_value = min_value,
      max_value = max_value,
      step_size = step_size,
      initial_value = initial_value
    )
  } else {
    sliderInput(
      inputId = input_id,
      label = label,
      min = min_value,
      max = max_value,
      value = initial_value,
      step = step_size,
      ticks = FALSE
    )
  }
}

## To be copied in the UI
# mod_input_params_ui("input_params_ui_1")

## To be copied in the server
# mod_input_params_server("input_params_ui_1")
