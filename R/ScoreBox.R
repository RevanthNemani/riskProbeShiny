#' ScoreBox UI module
#'
#' Shiny UI Module for showing scores
#'
#' Call via \code{ScoreBox("your_id", ...)}
#'
#' @param id Shiny id
#' @param score.box.title header title of the score box
#' @param progress.bar.title progress bar title
#' @param progress.bar.status the status of the progress bar which determines the colors. Eg: "primary", "info", "warning", "danger", "success"
#' @param progress.bar.value the value of the progress bar
#' @param knob.title title of the knob
#' @param knob.value value of the knob
#' @param knob.color color of the knob
#'
#' @import shiny
#' @importFrom shinyWidgets knobInput progressBar
#'
#' @return Shiny UI
#'
#' @author Revanth Nemani, \email{raynemani@gmail.com}
#'
#' @export

ScoreBox <- function(id,
                     score.box.title = NULL,
                     progress.bar.title = "Status",
                     progress.bar.status = "primary",
                     progress.bar.value = 0,
                     knob.title = "Score",
                     knob.value = 0,
                     knob.color = "#8E44AD") {
  ns <- shiny::NS(id)

  list(
    shiny::div(h3(score.box.title), style = "text-align: center;"),
    shiny::br(),
    shiny::div(
      shinyWidgets::progressBar(
        id = ns("progressbar"),
        value = progress.bar.value,
        title = tags$b(progress.bar.title),
        display_pct = T,
        status = progress.bar.status
      ),
      style = "text-align: center;"
    ),
    shiny::column(
      width = 9,
      offset = 3,
      shiny::div(
        shinyWidgets::knobInput(
          height = 150,
          width = 150,
          inputId = ns("knob"),
          label = knob.title,
          value = knob.value,
          angleOffset = 90,
          lineCap = "round",
          readOnly = T,
          fgColor = knob.color
        ),
        style = "text-align: center;"
      )
    )
  )
}



