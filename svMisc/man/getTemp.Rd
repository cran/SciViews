\name{getTemp}
\alias{getTemp}

\title{ Get a temporary variable from the TempEnv environment }
\description{
  The function gets a variable from TempEnv, an environment dedicated to temporary variables.
}
\usage{
    getTemp(x, default = NULL, mode = "any")
}

\arguments{
    \item{x}{ The name of the variable }
    \item{default}{ The default value to return, in case the variable does not exist }
    \item{mode}{ The mode of the variable (if the variable exists, but is not of correct mode, nothing is retrieved).
      Use \code{mode = "any"} (default value) to retrieve the variable whatever its mode. }
}

\value{
    The content of the variable, or the default value if the variable is not found in TempEnv.
}

\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{TempEnv}}, \code{\link{assignTemp}}, \code{\link{rmTemp}}, \code{\link{existsTemp}} }
\examples{
	assignTemp("test", TRUE)
	# Retrieve this variable
	getTemp("test")
	# Retrieve a non existing variable (returns default value)
	getTemp("nonexistant", default = "default value")
}
\keyword{ utilities }
