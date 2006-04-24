\name{guiInfo}
\alias{guiInfo}

\title{ Get an info tip or a completion list about an object }
\description{
  This function provides call tip, or completion list to GUI clients.
}
\usage{
guiInfo(fname, firstarg = NULL, type = "tip")
}

\arguments{
  \item{fname}{ The name of the function or of the object }
  \item{firstarg}{ The first argument (to determine which method is called, not used yet) }
  \item{type}{ The type of info required. Currently, 'tip', 'list' or 'listitems' }
}

\value{
  The content of the tip, or of the list.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiObjList}} }
\examples{
    #### TO DO...
}
\keyword{misc}
