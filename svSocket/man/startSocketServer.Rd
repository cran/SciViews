\name{startSocketServer}
\alias{startSocketServer}
\alias{stopSocketServer}

\title{ Start and stop a R socket server. }

\description{
  A R socket server is listening for command send by clients to a TCP port.
  This server is implemented in Tcl/Tk, using the powerful 'socket' command.
  Since it runs in the separate tcltk event loop, it is not blocking R, and it
  runs in the background; the user can still enter commands at the R prompt while
  one or several R socket servers are running and even, possibly, processing
  socket clients requests.
}

\usage{
startSocketServer(port = 8888, server.name = "Rserver", procfun = processSocket)
stopSocketServer(port = 8888)
}

\arguments{
  \item{port}{ The TCP port of the R socket server }
  \item{server.name}{ The internal name of this server }
  \item{procfun}{ The function to use to process client's commands. By default, it is \code{processSocket()}. }
}

\details{
  One can write a different \code{procfun} function than the default one for special
  servers. That function must accept one argument (a string with the command send by the client)
  and it must return a character string containing the result of the computation.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\note{ This server is currently synchronous in the processing of the command. However,
       neither R, nor the client are blocked during exchange of data (communication is asynchronous). }

\seealso{ \code{\link{processSocket}} }

\examples{
##TO DO...
}
\keyword{ IO }
