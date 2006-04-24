\name{getSocketClients}
\alias{getSocketClients}

\title{ Get infos about socket clients }
\description{
  List all clients currently connected to a given R socket server.
}

\usage{
getSocketClients(port = 8888)
}

\arguments{
  \item{port}{ The port of the R socket server. }
}

\value{
  A vector of character string with the address of clients in the form XXX.XXX.XXX.XXX:YYY
  where XXX.XXX.XXX.XXX is their ip address and YYY is thier port.
  The name of these items equals the corresponding Tcl socket name.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{getSocketServers}} }

\examples{
    # TO DO...
}
\keyword{ IO }
\keyword{ utilities }
