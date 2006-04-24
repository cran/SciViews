\name{guiSave}
\alias{guiSave}

\title{ Save one or several objects to a .Rdata file }
\description{
  This function is intended to be called by an object explorer context menu. It saves one or several objects to a .Rdata file.
}
\usage{
guiSave(..., list = character(0), ascii = FALSE, version = NULL, envir = parent.frame(), compress = TRUE)
}

\arguments{
  \item{\dots}{ The list of variables to save }
  \item{list}{ Idem, but provided in a list }
  \item{ascii}{ Should the data be written in ASCII or binary mode? }
  \item{version}{ Which version of .Rdata to use }
  \item{envir}{ From which environment objects should be retrieved? }
  \item{compress}{ Do we compress the file? }
}

\value{
  The path of the .Rdata file where variables are written.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiObjBrowse}} }
\examples{
    #### TO DO...
}
\keyword{misc}
