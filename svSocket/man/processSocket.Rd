\name{processSocket}
\alias{processSocket}

\title{ The function that processes a command from the socket. }

\description{
  This is the default R function called each time data is send by a client through a socket.
  It is possible to customize this function and to use customized versions for particular R socket servers.
}

\usage{
processSocket(msg)
}

\arguments{
  \item{msg}{ The message send by the client, to be processed. }
}

\value{
  Returns a string with the result of the process.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{startSocketServer}}, \code{\link{sendSocket}} }
\examples{
##TO DO...
}
\keyword{ IO }
