#' input_params UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_params_ui <- function(id, depth = NULL){
  ns <- NS(id)
  tagList(
    mod_input_params_ui_intervention(ns, depth = depth),
    mod_input_params_ui_context(ns, depth = depth),
    mod_input_params_ui_advanced(ns, depth = depth)
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
    test_p_asymp <- reactive(1/input$test_freq)

    # Context
    incid <- reactive(input$incid / 1e5)
    vac_comm <- reactive(input$vac_comm * 0.01)

    # Advanced
    vac_eff <- reactive(input$vac_eff * 0.01)
    inf_t_symp   <- reactive(input$inf_t_symp)
    inf_t_presymp <- reactive(input$inf_t_presymp)
    inf_p_symp <- reactive((100 - input$inf_p_asymp) * 0.01)
    test_p_symp <- reactive(input$test_p_symp * 0.01)
    detect_sens <- reactive(input$detect_sens * 0.01)

    # Group into `calc_dist()` inputs

    # `incid` already done

    vac <- reactive(list(
      p_comm = vac_comm(),
      p_org  = vac_org(),
      eff    = vac_eff()
    ))

    inf <- reactive(list(
      p_symp    = inf_p_symp(),
      t_symp     = inf_t_symp(),
      t_presymp = inf_t_presymp()
    ))

    test <- reactive(list(
      p_symp  = test_p_symp(),
      p_asymp = test_p_asymp()
    ))

    detect <- reactive(list(
      sens = 0.97,
      spec = 0.97
    ))

    reactive(list(
      incid  = incid(),
      vac    = vac(),
      inf    = inf(),
      test   = test(),
      detect = detect()
    ))
  })
}

mod_input_params_ui_intervention <- function(ns, depth = NULL) {
  shinymaterial::material_card(
    depth = depth,
    shinymaterial::material_button(
      ns("btn_interventions"),
      label = "Interventions",
      icon = "tune"
    ),
    conditionalPanel(
      condition = "input.btn_interventions % 2 == 0",
      ns = ns,
      tags$br(),
      shinymaterial::material_number_box(
        ns("test_freq"),
        label = "Testing Frequency (Days)",
        min_value = 1,
        max_value = 365,
        initial_value = 7,
        step_size = 1
      ),
      shinymaterial::material_slider(
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
  shinymaterial::material_card(
    depth = depth,
    shinymaterial::material_button(
      ns("btn_context"),
      label = "Community Context",
      icon  = "people"
    ),
    conditionalPanel(
      condition = "input.btn_context % 2 == 0",
      ns = ns,
      tags$br(),
      shinymaterial::material_number_box(
        ns("incid"),
        label = "Case Rate (per 100k per Day)",
        min_value = 0,
        max_value = 1e5,
        initial_value = 100,
        step_size = 10
      ),
      shinymaterial::material_slider(
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

mod_input_params_ui_advanced <- function(ns, depth = NULL) {
  shinymaterial::material_card(
    depth = depth,
    shinymaterial::material_button(
      ns("btn_advanced"),
      "Advanced Settings",
      icon = "settings"
    ),
    conditionalPanel(
      condition = "input.btn_advanced % 2 != 0",
      ns = ns,
      tags$br(),
      shinymaterial::material_slider(
        ns("vac_eff"),
        label = "Vaccine Efficacy (%)",
        min_value = 0,
        max_value = 100,
        initial_value = 70,
        step_size = 1
      ),
      shinymaterial::material_number_box(
        ns("inf_t_symp"),
        label = "Symptomatic Period (Days)",
        min_value = 1,
        max_value = 21,
        initial_value = 10,
        step_size = 1
      ),
      shinymaterial::material_number_box(
        ns("inf_t_presymp"),
        label = "Pre-symptomatic Period (Days)",
        min_value = 0,
        max_value = 14,
        initial_value = 3,
        step_size = 1
      ),
      shinymaterial::material_slider(
        ns("inf_p_asymp"),
        label = "Asymptomatic Infections (% All Infections)",
        min_value = 0,
        max_value = 100,
        initial_value = 50,
        step_size = 1
      ),
      shinymaterial::material_slider(
        ns("test_p_symp"),
        label = "Symptomatics Tested (% All Symptomatics)",
        min_value = 0,
        max_value = 100,
        initial_value = 95,
        step_size = 1
      ),
      shinymaterial::material_slider(
        ns("detect_sens"),
        label = "Test Sensitivity (%)",
        min_value = 0,
        max_value = 100,
        initial_value = 85,
        step_size = 1
      ),
      shinymaterial::material_slider(
        ns("detect_spec"),
        label = "Test Specificity (%)",
        min_value = 0,
        max_value = 100,
        initial_value = 100,
        step_size = 1
      )
    )
  )
}

## To be copied in the UI
# mod_input_params_ui("input_params_ui_1")

## To be copied in the server
# mod_input_params_server("input_params_ui_1")
