\name{guiStart}
\alias{guiInstall}
\alias{guiUninstall}
\alias{guiStart}
\alias{guiStop}

\title{ Install/Uninstall and Start/Stop a GUI client }
\description{
  A GUI needs several changes in \R, and needs also a way to communicate with it. These functions provide required features for SciViews compatible GUI clients.
}
\usage{
guiInstall(HideTkWindow = TRUE)
guiUninstall()
guiStart(gui = "\"%SciViews_Home%/bin/RConsole.exe\" -connect")
guiStop()
}

\arguments{
  \item{HideTkWindow}{ Whether the Tk communication window should be hidden or not }
  \item{gui}{ The path to the GUI client executable }
}
\value{
  TRUE if success, error message otherwise.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{Windows}} }
\examples{
    #### TO DO...
}
\keyword{misc}
