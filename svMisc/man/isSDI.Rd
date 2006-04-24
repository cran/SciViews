\name{isSDI}
\alias{isSDI}

\title{ Is Rgui started in SDI mode under Windows? }
\description{
  Determine if R is running in Rgui under Windows and in SDI mode.
}
\usage{
isSDI()
}
\value{
  TRUE if it is Rgui under Windows, and it is stared in SDI mode (Single-Document Interface), FALSE otherwise. 
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org>  & Eric Lecoutre <lecoutre@stat.ucl.ac.be>}
\note{ See the menu entry 'Edit' -> 'GUI preferences' to change it, or start Rgui with the '--SDI' argument line parameter. Under another platform than Windows or if it is not Rgui, then isSDI always returns FALSE }

\seealso{ \code{\link{isWin}} }
\examples{
	isSDI()
}
\keyword{ utilities }
