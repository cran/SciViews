\name{localPackages}
\alias{localPackages}

\title{ Install and update packages from a local source easily under Windows }
\description{
  Install and update packages from a local source like a CD-Rom. These packages must be windows binaries (.zip) and a 'package' file must describe them in the same directory as it is done on CRAN.
}
\usage{
localPackages(basedir = "d:/R", url = paste("file:", basedir, "/bin/windows/contrib", sep = ""), ask.update = TRUE)
}

\arguments{
  \item{basedir}{ The base directory of the local installation files }
  \item{url}{ The whole path, or url, to use for installation. If provided, it overrides \code{basedir} }
  \item{ask.update}{ If \code{ask.update} is TRUE, the function also update old packages. Otherwise, it only installs new ones. }
}
\value{
  This function returns nothing. It is invoked for its side effects.
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{installLocalPackages}}, \code{\link{updateLocalPackages}} }
\keyword{ utilities }
