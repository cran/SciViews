\name{sendSocket}
\alias{sendSocket}

\title{ Send data to a client through a socket. }

\description{
  The text is send to one of the R socket server's clients currently connected.
}

\usage{
sendSocket(text, socket)
}

\arguments{
  \item{text}{ The text to send to the client. }
  \item{socket}{ The Tcl name of the client socket currently connected. }
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{processSocket}} }
\examples{
##TO DO...
}
\keyword{ IO }
