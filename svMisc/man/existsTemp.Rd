\name{existsTemp}
\alias{existsTemp}

\title{ Determine if a variable exists in TempEnv }
\description{
  Does a variable exists in the temporary environment, TempEnv?
}
\usage{
    existsTemp(x, mode = "any")
}

\arguments{
    \item{x}{ The name of the variable (string) }
    \item{mode}{ The mode of the seeked variable }
}

\value{
    TRUE if the variable exists in TempEnv (and is of the correct mode), FALSE otherwise.
}

\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{TempEnv}}, \code{\link{assignTemp}}, \code{\link{getTemp}}, \code{\link{rmTemp}} }
\examples{
	assignTemp("test", TRUE)
	# Retrieve this variable
	existsTemp("test")
	# Remove it
	rmTemp("test")
	# Does it exists?
	existsTemp("test")
}
\keyword{ utilities }
