\name{listMethods}
\alias{listMethods}

\title{ List all methods associated with a generic function }
\description{
  List all S3 and/or S4 methods for a generic function.
}
\usage{
listMethods(f, S3 = TRUE, S4 = TRUE)
}

\arguments{
  \item{f}{ The name of the generic function }
  \item{S3}{ If TRUE, list of S3 methods }
  \item{S4}{ If TRUE, list of S4 methods }
}
\value{
  A list with components:
  \item{S3}{The S3 methods for this generic function, or \code{character(0)} if none}
  \item{S4}{The S4 methods for this generic function, or \code{character(0)} if none}
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{listCustoms}} }
\examples{
	listMethods("t.test") # S3
	listMethods("show")	# S4
	listMethods("ls") # None, not a generic function!
}
\keyword{ utilities }
