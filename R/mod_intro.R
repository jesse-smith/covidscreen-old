#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$p(
      "The covidscreen app allows you to see the effects of different levels of ",
      "asymptomatic testing in your organization. You can tailor the context to ",
      "your specific use case, and you can vary most of the assumptions of the ",
      "model if you wish. The app concerns itself with cases imported from ",
      "the larger community; it does not attempt to model COVID-19 spread ",
      "inside an organization."
    ),
    tags$h3("Major Idea"),
    tags$p(
      "The model plays out, mathematically, what you would expect given the ",
      "inputs you will find on the left in the next two tabs. At its core, it ",
      "simply calculates probabilities for given (ranges) of scenarios and ",
      "displays them in what is hopefully an easy-to-act-on format."
    ),
    tags$p(
      "Qualitatively, most of what's happening behind the scenes is easy to ",
      "grasp. The setup is this: there are people with COVID-19 in the ",
      "community where you are situated. Some of those people are likely also ",
      "part of your organization. You can find those people by testing your ",
      "members at regular intervals, and the more frequently you test, the ",
      "more cases you find early, and the fewer people you have spreading ",
      "COVID-19 within your organization. Vaccination helps this cause by ",
      "making people less likely to contract COVID-19 in the first place. ",
      "How many infections you have also depend on the specifics of the ",
      "virus, its vaccinations, and its tests. By defining and playing out ",
      "the interactions between all of these inputs, you can gain a sense of ",
      "the level of intervention you need for your budget and risk tolerance."
    ),
    tags$p(
      "There are a couple of concepts that may be useful in using this app. ",
      "The first is the difference between new cases and active cases. New ",
      "cases are the newly infected people each day. Each new day brings more ",
      "new cases, in addition to previous cases who are still spreading ",
      "COVID-19. The sum of all of these new and still-spreading cases is ",
      "called active cases. Since the risk of infection comes from all ",
      "still-spreading cases, this model is concerned with active cases. ",
      "Specifically, it looks at what happens if you keep the same number of ",
      "new cases for a while, which leads to a constant level of active cases ",
      "as well."
    ),
    tags$p(
      "The second useful concept is a distinction between asymptomatic, ",
      "pre-symptomatic, and symptomatic cases. Not everyone with COVID-19 ",
      "develops symptoms, and even those that do usually take several days ",
      "to display them. People that never show symptoms are called ",
      "asymptomatic; we believe they make up a large portion of total active ",
      "cases. However, many of the eventually symptomatic cases have not ",
      "started showing symptoms; these are called pre-symptomatic. In ",
      "modeling the likelihood that someone gets tested, we have to account ",
      "for both types of non-symptomaticity. This is necessary because we ",
      "assume that most symptomatic people will get tested if they can ",
      "(though this is variable in the advanced parameters section)."
    ),
    tags$h3("Instructions"),
    "To use the app, simply navigate to the 'Scenarios' tab and input the ",
    "number of people in your organization, the highest number of undetected ",
    "cases you are comfortable with, and the highest number of tests you can ",
    "perform in a day. Next, set the parameters on the left to the testing ",
    "frequencies you are considering, your current vaccination levels, and ",
    "the numbers matching your community case rate and vaccination levels.",
    "The graphics on the right will update as you do so, showing you the ",
    "benefits of testing unvaccinated vs vaccinated people, the number of ",
    "tests and undetected cases you can expect. You can change the parameters ",
    "on the right to see how the outcomes react."
  )
}

#' intro Server Functions
#'
#' @noRd
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_intro_ui("intro_ui_1")

## To be copied in the server
# mod_intro_server("intro_ui_1")
