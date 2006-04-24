\name{guiRemove}
\alias{guiRemove}

\title{ Remove one or several objects (and prompt for confirmation) }
\description{
  This function is intended to be called by an object browser context menu. It deletes variable(s) after prompting for confirmation.
}
\usage{
guiRemove(list = character(0), inherits = FALSE, ask = TRUE)
}

\arguments{
  \item{list}{ A list of variables to delete }
  \item{inherits}{ Do the variables inherit from lower environments in the search path? }
  \item{ask}{ Do we ask for confirmation to the user? }
}

\value{
  Nothing if the function succeed.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiObjBrowse}} }
\examples{
    #### TO DO...
}
\keyword{misc}
