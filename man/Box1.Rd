% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Box1.R
\name{Box1}
\alias{Box1}
\title{Box1 Server module}
\usage{
Box1(input, output, session, progress.bar.value = 0,
  progress.bar.status = "primary", stars.total = 5, stars.grade = 0,
  stars.color = "blue", label.title = "Watchlist",
  label.status = "primary", brief.description = "Description of test",
  display.plot = graphics::hist(x = c(6, 7, 8)),
  plot.text = "Plot values")
}
\arguments{
\item{input}{shiny input}

\item{output}{shiny output}

\item{session}{shiny session}

\item{progress.bar.value}{the progress bar value to diplay on calculating}

\item{progress.bar.status}{the status of the progress bar which determines the colors. Eg: "primary", "info", "warning", "danger", "success"}

\item{stars.total}{total number of stars to display}

\item{stars.grade}{total number of stars to fill out of the total displayed stars}

\item{stars.color}{color of the stars}

\item{label.title}{label to indicate the test results}

\item{label.status}{label status which determines the label colors. Eg: "primary", "info", "warning", "danger", "success"}

\item{brief.description}{a line or two description of the test}

\item{display.plot}{plot to show}

\item{plot.text}{text to show under the plot}
}
\description{
Shiny Server Module for use with \link{Box1UI}
}
\details{
Call via \code{shiny::callModule(Box1, "your_id", ...)}
}
\author{
Revanth Nemani, \email{raynemani@gmail.com}
}
