\name{getSocketServers}
\alias{getSocketServers}

\title{ Get the ports of current R socket servers. }
\description{
  Returns a list with all the ports of currently running R socket servers.
}

\usage{
getSocketServers()
}

\value{
  A chyaracter vector, or \code{NULL} if no R socket server is currently running.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{getSocketClients}}, \code{\link{getSocketServerName}}, \code{\link{startSocketServer}} }

\examples{
## TO DO...
}
\keyword{ IO }
\keyword{ utilities }
