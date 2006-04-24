\name{display}
\alias{display}
\alias{display.guiDlg}

\title{ The display method (for flexible dialog boxes) }
\description{
  Display a dialog box previously calculated. For instance, display a flexible dialog box.
}
\synopsis{
display(x, ...)
display.guiDlg(x, parent = 0, GUI = getOption("guiWidgets"), debug = FALSE, ...)
}
\usage{
display(x, ...)
display(x, parent = 0, GUI = getOption("guiWidgets"), debug = FALSE, ...)
}

\arguments{
  \item{x}{ An object that accepts a display method }
  \item{parent}{ Which window is the perent of this flexible dialog box? }
  \item{GUI}{ Which GUI to use to display this dialog box? }
  \item{debug}{ Do we debug this dialog box? }
  \item{\dots}{ Further arguments passed to the display method }
}

\value{
  The R code constructed by this dialog box
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiDlg}}, \code{\link{guiEval}} }
\examples{
#### TO DO...
}
\keyword{misc}
