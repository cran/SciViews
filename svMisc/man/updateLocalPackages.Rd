\name{updateLocalPackages}
\alias{updateLocalPackages}

\title{ Update packages from a local source easily under Windows }
\description{
  Update packages from a local source like a CD-Rom. These packages must be windows binaries (.zip) and a 'package' file must describe them in the same directory as it is done on CRAN.
}
\usage{
updateLocalPackages(basedir = "d:/R", url = paste("file:", basedir, "/bin/windows/contrib", sep = ""), ask = TRUE)
}

\arguments{
  \item{basedir}{ The base directory of the local installation files }
  \item{url}{ The whole path, or url, to use for installation. If provided, it overrides \code{basedir} }
  \item{ask}{ Should we ask for each package if it should be updated? }
}
\value{
  This function returns nothing. It is invoked for its side effects.
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{localPackages}}, \code{\link{installLocalPackages}} }
\keyword{ utilities }
