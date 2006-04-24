\name{guiDlgDir}
\alias{guiDlgDir}
\alias{guiDlgDir.wxPython}

\title{ A select directory dialog box }
\description{
  Allows to easily select a directory
}
\synopsis{
guiDlgDir(title = "Select a directory", dir = getwd(), new = TRUE, parent = 0, GUI = getOption("guiWidgets"))
guiDlgDir.wxPython(title = "Select a directory", dir = getwd(), new = TRUE, parent = 0)
}

\usage{
guiDlgDir(title = "Select a directory", dir = getwd(), new = TRUE, parent = 0, GUI = getOption("guiWidgets"))
}

\arguments{
  \item{title}{ the title of the dialog box }
  \item{dir}{ the default directory }
  \item{new}{ is a new directory allowed? }
  \item{parent}{ the parent of this dialog box (not implemented yet) }
  \item{GUI}{ which GUI widgets do you want to use (tcltk, wxPython, ...) }
}

\value{
  The choosen directory.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiDlgOpen}},  \code{\link{guiDlgSave}} }
\examples{
    #### TO DO...
}
\keyword{ misc }
