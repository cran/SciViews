\name{guiDlg}
\alias{guiDlg}

\title{ Create a flexible dialog box }
\description{
  This function creates a flexible dialog boxe, constituted by one or several 'panes'
}
\usage{
guiDlg(title = "Input", message = NULL, help = NULL, sep = NULL, width = 50,
    labelwidth = 0, panes = list(list(type = "entry", message = "Enter data:",
    default = NULL)), GUI = getOption("guiWidgets"))
}

\arguments{
  \item{title}{ The title of the dialog box }
  \item{message}{ The message to display on top of the dialog box }
  \item{help}{ The help topic to trigger with the button 'help' }
  \item{sep}{ Do we use separators? }
  \item{width}{ The width of the dialog box }
  \item{labelwidth}{ The width reserved for labels }
  \item{panes}{ A list with the panes to add to this flexible dialog box }
  \item{GUI}{ Which GUI toolkit do we use? }
}

\value{
  Returns a guiDlg object that can be shown using \code{display} method.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{display}}, \code{\link{guiEval}}, \code{\link{guiPane.tcltk}} }
\examples{
### TO DO...
  }
}
\keyword{misc}
