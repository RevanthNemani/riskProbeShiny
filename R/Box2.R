#' Box2 UI module
#'
#' Shiny UI Module for use with \link{Box2}
#'
#' Call via \code{Box2UI("your_id")}
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


Box2UI <- function(id,
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
      shinyWidgets::actionBttn(
        inputId = ns("button"),
        label = button.label,
        icon = button.icon,
        style = button.style,
        color = button.color,
        size = button.size,
        no_outline = T
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

