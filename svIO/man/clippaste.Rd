\name{clippaste}
\alias{clippaste}

\title{ Paste the content of the clipboard into an variable }
\description{
  Create a new object by pasting the content of the clipboard in it. Contents generated with \code{copy} are recognized.
}

\usage{
clippaste(name = "newobj", type = "ascii", objclass = "data.frame", pos = 1, ...)
}

\arguments{
  \item{name}{ The name of the variable to create }
  \item{type}{ Specify the type of content (the format of the object currently in the clipboard) }
  \item{objclass}{ The class of the object to create }
  \item{pos}{ Where to place the new object (by default, in the global workspace) }
  \item{\dots}{ Further arguments passed to the specific paste method }
}

\value{
  The result returned by the specific paste method.
}

\author{ Eric Lecoutre }

\seealso{ \code{\link{copy}} }

\examples{
    \dontrun{
        # A trivial example... but that becomes more interesting if the copy
        # and paste operations are made in different R instances
        data(iris)
        copy(iris, type = "ascii")
        clippaste(iris2)
    }
}

\keyword{IO}
\keyword{file}
