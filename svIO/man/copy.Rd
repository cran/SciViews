\name{copy}
\alias{copy}

\title{ Convert an R object and copy it to the clipboard }

\description{
  This is a wrapper around \code{export}, using \code{file = "clipboard"}
}

\usage{
    copy(x, type = "raw", objname = deparse(substitute(x)), ...)
}

\arguments{
  \item{x}{ The object to copy }
  \item{type}{ \code{type} gives the format in which the object should be converted. The list of recognized formats for this object is obtained with \code{type = "typelist"} }
  \item{objname}{ The name of the copie object (by default, it is the name of the variable that holds it) }
  \item{\dots}{ Further arguments to use with the corresponding \code{export} method }
}
\value{
  The result returned by the corresponding \code{export} method
}

\author{ Philippe Grosjean & Eric Lecoutre }

\seealso{ \code{\link{export}}, \code{\link{clippaste}} }

\examples{
    \dontrun{
        data(iris)
        copy(iris, type = "ascii")
    }
}

\keyword{IO}
\keyword{file}
