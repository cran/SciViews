\name{guiDlgMessage}
\alias{guiDlgMessage}
\alias{guiDlgMessage.wxPython}

\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\synopsis{
guiDlgMessage(message, title = "Message", type = c("ok", "okcancel", "yesno", "yesnocancel"), default = 1, icon = c("info", "question", "error", "warning"), parent = 0, GUI = getOption("guiWidgets"))
guiDlgMessage.wxPython(message, title = "Message", type = "ok", default = 1, icon = "info", parent = 0)
}
\usage{
guiDlgMessage(message, title = "Message", type = c("ok", "okcancel", "yesno", "yesnocancel"),
    default = 1, icon = c("info", "question", "error", "warning"), parent = 0,
    GUI = getOption("guiWwidgets"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{message}{ the message to display in the dialog box }
  \item{title}{ the title of the dialog box }
  \item{type}{ the type of dialog box: ok, okcancel, yesno or yesnocancel }
  \item{default}{ the default button (if more than one) }
  \item{icon}{ the icon: info, question, error or warning }
  \item{parent}{ the parent of this dialog box (not implemented yet) }
  \item{GUI}{ which GUI widgets do you want to use (tcltk, wxPython, ...) }
}

\value{
  A string with the caption of the button that the user pressed.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiDlgInput}} }
\examples{
    #### TO DO...
}
\keyword{ misc }
