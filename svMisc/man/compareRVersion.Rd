\name{compareRVersion}
\alias{compareRVersion}

\title{ Compare current R version with a specified one }
\description{
  Determine if R is older (return -1), or not (return 1) than a given version number.
}
\usage{
    compareRVersion(version)
}

\arguments{
    \item{version}{ A string defining the version, like '2.0' or '1.9.1' }
}

\value{
  -1 if R is older, 1 otherwise.
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{isSDI}}, \code{\link{isWin}} }
\examples{
	compareRVersion("2.0")
}
\keyword{ utilities }
