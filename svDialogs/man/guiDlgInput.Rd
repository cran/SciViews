\name{guiDlgInput}
\alias{guiDlgInput}
\alias{guiDlgInput.wxPython}

\title{ A simple input box }
\description{
  Enter one (textual) value with this input box
}
\synopsis{
guiDlgInput(message = "Enter a value", title = "Input", default = "", parent = 0, GUI = getOption("guiWidgets"))
guiDlgInput.wxPython(message = "Enter a value", title = "Input", default = "", parent = 0)
}
\usage{
guiDlgInput(message = "Enter a value", title = "Input", default = "", parent = 0, GUI = getOption("guiWidgets"))
}

\arguments{
  \item{message}{ the message to display on top of the text box }
  \item{title}{ the title of the dialog box }
  \item{default}{ the default value in the text box }
  \item{parent}{ the parent of this dialog box (not implemented yet) }
  \item{GUI}{ which GUI widgets do you want to use (tcltk, wxPython, ...) }
}

\value{
  The text entered by the user at the input box
}
\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiDlgMessage}}, \code{\link{guiDlgText}} }
\examples{
    #### TO DO...
}
\keyword{ misc }
