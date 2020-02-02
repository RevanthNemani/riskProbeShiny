#' QBox UI module
#'
#' Shiny UI Module for qualitative page
#'
#' Call via \code{QBoxUI("your_id", ...)}
#'
#' @param id Shiny id
#' @param question.box.1.title question box 1 title
#' @param question.box.1.status question box 1 status
#' @param question.box.1.tab.1 tab name
#' @param question.box.1.tab.2 tab name
#' @param question.box.2.title question box 2 title
#' @param question.box.2.status question box 2 status
#' @param question.box.2.tab.1 tab name
#' @param question.box.2.tab.2 tab name
#' @param question.box.3.title question box 3 title
#' @param question.box.3.status question box 3 status
#' @param question.box.3.tab.1 tab name
#' @param question.box.3.tab.2 tab name
#' @param question.box.3.tab.3 tab name
#' @param question.box.3.tab.4 tab name
#' @param question.box.4.title question box 4 title
#' @param question.box.4.status question box 4 status
#' @param question.box.4.tab.1 tab name
#' @param question.box.4.tab.2 tab name
#' @param submit.button.label label for the submit button
#'
#' @import shiny
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets radioGroupButtons actionBttn
#'
#' @return Shiny UI
#'
#' @author Revanth Nemani, \email{raynemani@gmail.com}
#'
#' @export
QBoxUI <- function(id,
                   question.box.1.title = "Input Data",
                   question.box.1.status = "primary",
                   question.box.1.tab.1 = "Input Data Validation",
                   question.box.1.tab.2 = "Data Archiving",
                   question.box.2.title = "Model Use",
                   question.box.2.status = "success",
                   question.box.2.tab.1 = "Input Data Validation",
                   question.box.2.tab.2 = "Data Archiving",
                   question.box.3.title = "ModelGovernance",
                   question.box.3.status = "danger",
                   question.box.3.tab.1 = "Input Data Validation",
                   question.box.3.tab.2 = "Data Archiving",
                   question.box.3.tab.3 = "Input Data Validation",
                   question.box.3.tab.4 = "Data Archiving",
                   question.box.4.title = "Model Design",
                   question.box.4.status = "warning",
                   question.box.4.tab.1 = "Input Data Validation",
                   question.box.4.tab.2 = "Data Archiving",
                   submit.button.label = "Submit") {
  ns <- shiny::NS(id)

  qualitative.choice.names <-
    c("Yes, evidence available (or, not required)",
      "Yes, but evidence not available",
      "No")
  qualitative.choice.values <- c(1, 2, 3)

  list(
    shinydashboard::box(
      title = question.box.1.title,
      status = question.box.1.status,
      width = 6,
      collapsible = T,
      collapsed = T,
      shiny::tabsetPanel(
        type = "tabs",
        id = "id1",
        shiny::tabPanel(
          title = question.box.1.tab.1,
          icon = NULL,
          shiny::br(),
          shinyWidgets::radioGroupButtons(
            inputId = "rgb1",
            label = "Have inadequate or missing data been re-assessed and reviewed for model feasibility?",
            choiceNames = qualitative.choice.names,
            choiceValues = qualitative.choice.values,
            justified = T,
            direction = "vertical",
            individual = T,
            status = "primary",
            size = "sm"
          ),
          shiny::br(),
          shinyWidgets::radioGroupButtons(
            inputId = "rgb2",
            label = "Were the outliers (if applicable) in the data adequatly treated?",
            choiceNames = qualitative.choice.names,
            choiceValues = qualitative.choice.values,
            justified = T,
            direction = "vertical",
            individual = T,
            status = "primary",
            size = "sm"
          ),
          shiny::br(),
          shinyWidgets::radioGroupButtons(
            inputId = "rgb3",
            label = "Was it verified that data are representative of the company's portfolio?",
            choiceNames = qualitative.choice.names,
            choiceValues = qualitative.choice.values,
            justified = T,
            direction = "vertical",
            individual = T,
            status = "primary",
            size = "sm"
          ),
          shiny::br(),
          shinyWidgets::radioGroupButtons(
            inputId = "rgb10",
            label = "Selection of macroeconomic indicators for the model process was as per the industry standard?",
            choiceNames = qualitative.choice.names,
            choiceValues = qualitative.choice.values,
            justified = T,
            direction = "vertical",
            individual = T,
            status = "primary",
            size = "sm"
          )
        ),
        shiny::tabPanel(
          title = question.box.1.tab.2,
          icon = NULL,
          shiny::br(),
          shinyWidgets::radioGroupButtons(
            inputId = "rgb4",
            label = "Was a check for inconsistent representation of data fields due to multiple data sourcing platforms performed? For example, certain variables may have different meanings for different segments or when sourced from different platforms, conversion of such variables to a unique representation is must.",
            choiceNames = qualitative.choice.names,
            choiceValues = qualitative.choice.values,
            justified = T,
            direction = "vertical",
            individual = T,
            status = "primary",
            size = "sm"
          ),
          shiny::br(),
          shinyWidgets::radioGroupButtons(
            inputId = "rgb5",
            label = "Was a check for adequate analysis to decide bad definition performed? Standard Industry practices require roll rate analysis to justify the use of bad definition.",
            choiceNames = qualitative.choice.names,
            choiceValues = qualitative.choice.values,
            justified = T,
            direction = "vertical",
            individual = T,
            status = "primary",
            size = "sm"
          ),
          shiny::br(),
          shinyWidgets::radioGroupButtons(
            inputId = "rgb6",
            label = "Was a check for adequate analysis to decide the vintage of the development data performed? As per standard industry practice, vintage analysis is used to justify the use of data from a certain vintage",
            choiceNames = qualitative.choice.names,
            choiceValues = qualitative.choice.values,
            justified = T,
            direction = "vertical",
            individual = T,
            status = "primary",
            size = "sm"
          )
        )
      )
    ),
    shinydashboard::box(
      title = question.box.2.title,
      status = question.box.2.status,
      width = 6,
      collapsible = T,
      collapsed = T
    ),
    shinydashboard::box(
      title = question.box.3.title,
      status = question.box.3.status,
      width = 6,
      collapsible = T,
      collapsed = T
    ),
    shinydashboard::box(
      title = question.box.4.title,
      status = question.box.4.status,
      width = 6,
      collapsible = T,
      collapsed = T
    ),
    shinydashboard::box(
      title = NULL,
      width = 12,
      collapsible = T,
      collapsed = F,
      shiny::div(
        shinyWidgets::actionBttn(
          inputId = ns("button"),
          label = submit.button.label,
          icon = NULL,
          style = "unite",
          color = "primary",
          size = "sm",
          no_outline = T
        ),
        style = "text-align: center;"
      ),
      shiny::uiOutput(outputId = ns("scoring"))
    )
  )
}




#' QBox server module
#'
#' Shiny server Module for qualitative page
#'
#' Call via \code{shiny::callModule(QBox, "your_id", ...)}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param submit.button.label The submit button title
#'
#' @import shiny
#' @importFrom shinyWidgets knobInput
#' @importFrom shinyWidgets actionBttn
#'
#' @return Shiny UI
#'
#' @author Revanth Nemani, \email{raynemani@gmail.com}
#'
#' @export
QBox <- function(input,
                 output,
                 session,
                 submit.button.label = "Submit Again") {
  observeEvent(input$button, {
    output$scoring <- renderUI({
      list(
        column(
          width = 3,
          align = "center",
          ScoreBox(
            id = "sc1",
            score.box.title = "Input Data",
            progress.bar.title = "Status",
            progress.bar.status = "primary",
            progress.bar.value = 20,
            knob.title = "Score",
            knob.value = 20
          ),
          ScoreBox(
            id = "sc2",
            score.box.title = "Model Governance",
            progress.bar.title = "Status",
            progress.bar.status = "primary",
            progress.bar.value = 40,
            knob.title = "Score",
            knob.value = 30
          )
        ),
        shiny::column(
          width = 6,
          align = "center",
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::br(),
          shiny::div(h2("Qualitative Score"), style = "text-align: center;"),
          shinyWidgets::knobInput(
            height = 300,
            width = 300,
            inputId = "qual_sc",
            label = NULL,
            value = 60,
            angleOffset = -125,
            angleArc = 250,
            readOnly = TRUE,
            fgColor = "#8E44AD"
          )
        ),
        shiny::column(
          width = 3,
          align = "center",
          ScoreBox(
            id = "sc3",
            score.box.title = "Input Data",
            progress.bar.title = "Status",
            progress.bar.status = "success",
            progress.bar.value = 60,
            knob.title = "Score",
            knob.value = 50
          ),
          ScoreBox(
            id = "sc4",
            score.box.title = "Model Governance",
            progress.bar.title = "Status",
            progress.bar.status = "danger",
            progress.bar.value = 10,
            knob.title = "Score",
            knob.value = 70
          )
        )
      )
    })
  })
}
