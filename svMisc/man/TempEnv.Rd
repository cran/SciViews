\name{TempEnv}
\alias{TempEnv}

\title{ Get (and create it if needed) a temporary environment }
\description{
  This function creates and returns the reference to a temporary environment called TempEnv.
}
\usage{
    TempEnv()
}

\value{
  TempEnv (an environment object)
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{assignTemp}}, \code{\link{getTemp}}, \code{\link{rmTemp}}, \code{\link{existsTemp}} }
\examples{
	TempEnv()
}
\keyword{ utilities }
