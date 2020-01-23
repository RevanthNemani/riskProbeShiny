#' Box1 UI module
#'
#' Shiny UI Module for use with \link{Box1}
#'
#' Call via \code{Box1UI("your_id")}
#'
#' @param id Shiny id
#' @param run.box.title header title of the run box
#' @param run.box.color run box outline color
#' @param button.label run button label
#' @param button.icon run button icon
#' @param button.style run button style refer \code{\link[shinyWidgets]{actionBttn}}
#' @param button.color run button color refer \code{\link[shinyWidgets]{actionBttn}}
#' @param button.size run button size refer \code{\link[shinyWidgets]{actionBttn}}
#' @param description.box.title description box header title
#' @param description.box.color description box color
#' @param path.html.document path for html document of the description
#'
#' @import shiny
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinydashboard box
#'
#' @return Shiny UI
#'
#' @author Revanth Nemani, \email{raynemani@gmail.com}
#'
#' @export


Box1UI <- function(id,
                   run.box.title = "title",
                   run.box.color = "warning",
                   button.label = "Calculate",
                   button.icon = shiny::icon("refresh"),
                   button.style = "unite",
                   button.color = "primary",
                   button.size = "sm",
                   description.box.title = "Description",
                   description.box.color = "primary",
                   path.html.document = NULL) {
  ns <- shiny::NS(id)

  list(shiny::column(
    width = 6,
    shinydashboard::box(
      title = run.box.title,
      status = run.box.color,
      width = 12,
      collapsible = T,
      shiny::uiOutput(ns("content")),
      shiny::column(
        width = 7,
        offset = 5,
        shinyWidgets::actionBttn(
          inputId = ns("button"),
          label = button.label,
          icon = button.icon,
          style = button.style,
          color = button.color,
          size = button.size,
          no_outline = T
        )
      )
    ),
    shinydashboard::box(
      title = description.box.title,
      status = description.box.color,
      collapsible = T,
      collapsed = T,
      width = 12,
      shiny::includeHTML(path.html.document)
    )
  ))
}

#' Box1 Server module
#'
#' Shiny Server Module for use with \link{Box1UI}
#'
#' Call via \code{shiny::callModule(Box1, "your_id", ...)}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param progress.bar.value the progress bar value to diplay on calculating
#' @param progress.bar.status the status of the progress bar which determines the colors. Eg: "primary", "info", "warning", "danger", "success"
#' @param stars.total total number of stars to display
#' @param stars.grade total number of stars to fill out of the total displayed stars
#' @param stars.color color of the stars
#' @param label.title label to indicate the test results
#' @param label.status label status which determines the label colors. Eg: "primary", "info", "warning", "danger", "success"
#' @param brief.description a line or two description of the test
#' @param display.plot plot to show
#' @param plot.text text to show under the plot
#'
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinydashboard box
#' @importFrom graphics hist
#'
#'
#' @author Revanth Nemani, \email{raynemani@gmail.com}
#'
#' @export


Box1 <- function(input,
                 output,
                 session,
                 progress.bar.value = 0,
                 progress.bar.status = "primary",
                 stars.total = 5,
                 stars.grade = 0,
                 stars.color = "blue",
                 label.title = "Watchlist",
                 label.status = "primary",
                 brief.description = "Description of test",
                 display.plot = graphics::hist(x = c(6, 7, 8)),
                 plot.text = "Plot values") {
  shiny::observeEvent(input$button, {
    output$content <- shiny::renderUI({
      list(
        shinyWidgets::progressBar(
          id = "pbar",
          value = progress.bar.value,
          display_pct = T,
          status = progress.bar.status
        ),
        column(
          offset = 5,
          width = 7,
          shinydashboardPlus::starBlock(
            maxstar = stars.total,
            grade = stars.grade,
            color = stars.color
          )
        ),
        shiny::br(), shiny::br(),
        shiny::column(
          width = 12,
          shiny::div(
            style = "text-align: center;",
            shinydashboardPlus::dashboardLabel(label.title,
                                               status = label.status,
                                               style = "square")
          )
        ),
        shiny::br(), shiny::br(),
        renderText(brief.description),
        shiny::br(),
        renderPlot(display.plot),
        shiny::br(), shiny::br(),
        renderText(plot.text)
      )
    })
  })
}
