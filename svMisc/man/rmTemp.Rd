\name{rmTemp}
\alias{rmTemp}

\title{ Remove one or several temporary variable(s) from the TempEnv environment }
\description{
  The function removes one or more variable(s) from TempEnv.
}
\usage{
    rmTemp(x)
}

\arguments{
    \item{x}{ The name of the variable (string), or a vector of characters with the name of all variables to remove from TempEnv }
}

\value{
    Nothing (or a try-error object if it fails).
}

\section{Warning}{
This command issues no error message if variable(s) do not exist in TempEnv!
}

\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{TempEnv}}, \code{\link{assignTemp}}, \code{\link{getTemp}}, \code{\link{existsTemp}} }
\examples{
	assignTemp("test", TRUE)
	# Retrieve this variable
	getTemp("test")
	# Remove it
	rmTemp("test")
	# Try to retrieve it again
	getTemp("test")
}
\keyword{ utilities }
