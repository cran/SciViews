\name{installLocalPackages}
\alias{installLocalPackages}

\title{ Install packages from a local source under Windows }
\description{
  Install one or several local packages, for instance from a CD-rom. These packages must be Windows binaries (.zip), and a 'package' in the same directory must describe them the same way it is done in CRAN.
}
\usage{
installLocalPackages(basedir = "d:/R", url = paste("file:", basedir, "/bin/windows/contrib", sep = ""))
}

\arguments{
  \item{basedir}{ The base directory of the local installation files }
  \item{url}{ The whole path, or url, to use for installation. If provided, it overrides \code{basedir} }
}
\value{
  This function returns nothing. It is invoked for its side effects.
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{localPackages}}, \code{\link{updateLocalPackages}} }
\keyword{ utilities }
