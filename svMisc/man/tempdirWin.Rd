\name{tempdirWin}
\alias{tempdirWin}

\title{ Get the Windows temporary directory (only under Windows) }
\description{
  \code{tempdirWin()} retrieves the Windows temporary directory, which is simply the root of \code{tempdir()} under Windows.
}
\usage{
    tempdirWin()
}

\value{
  A string with the path to the temporary directory
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{userdir}} }
#ifdef windows
\examples{
	tempdirWin()
}
#endif
\keyword{ utilities }
