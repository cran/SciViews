\name{guiCmd}
\alias{guiCmd}

\title{ Send a command to the GUI client }
\description{
  This function is not intended to be used at the command line (except for
  debugging purposes). It sends a command string to a GUI client.
}
\usage{
guiCmd(command)
}

\arguments{
  \item{command}{ The command string to send }
}

\value{
  TRUE if the command succeed, the content of \code{command}, otherwise.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link[svGUI]{guiInstall}}, \code{\link[svGUI]{guiStart}} }

\keyword{misc}
