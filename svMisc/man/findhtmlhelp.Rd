\name{findhtmlhelp}
\alias{findhtmlhelp}

\title{ Find the HTML help file associated with a topic }
\description{
  Get the path of the HTML help file associated with a topic, but do not display it. It can be used for contextual menu of objects in an object browser.
}
\usage{
findhtmlhelp(topic, package = .packages(), lib.loc = NULL, verbose = getOption("verbose"))
}

\arguments{
  \item{topic}{ The topic to search }
  \item{package}{ An optional package where to look at }
  \item{lib.loc}{ The path of the local library }
  \item{verbose}{ Verbose output }
}
\value{
  A character string containing the path of the HTML file, or "" if this file does not exist.
}
\author{ Eric Lecoutre <lecoutre@stat.ucl.ac.be> }

\seealso{ \code{\link{listMethods}} }
\examples{
	findhtmlhelp("plot")
	findhtmlhelp("ls")
}
\keyword{ utilities }
