\name{export}
\alias{export}
\alias{export.default}
\alias{export.data.frame}
\alias{export.matrix}

\title{ Export the content of an object in a foreign format }

\description{
  \code{export} is a generic function to convert objects into various foreign formats and write it to a file or a connection.
}

\synopsis{
    export(x, type = "raw", file, append = FALSE, objname = deparse(substitute(x)), ...)
    export.default(x, type = "raw", file, append = FALSE, objname = deparse(substitute(x)), ...)
    export.data.frame(x, type = "raw", file = NULL, append = FALSE, objname = deparse(substitute(x)), saslib = "work", sasmember = "", writeit = TRUE, ...)
    export.matrix(x, type = "raw", file, append = FALSE, objname = deparse(substitute(x)), ...)
}

\usage{
    export(x, type = "raw", file, append = FALSE, objname = deparse(substitute(x)), ...)
}

\arguments{
  \item{x}{ The object to export }
  \item{type}{ \code{type} gives the format in which the object should be converted. The list of recognized formats for this object is obtained with \code{type = "typelist"} }
  \item{file}{ The path to the file where the converted object should be extracted }
  \item{append}{ Do we append this object to this file? }
  \item{objname}{ The name of the object (by default, it is the name of the variable that holds it) }
  \item{\dots}{ Further argument passed to specific export methods }
}

\value{
  Return the result from specif exportation methods (invisibly if the file is written)
}

\author{ Philippe Grosjean & Eric Lecoutre }

\seealso{ \code{\link{copy}} }
\examples{
    \dontrun{
        data(iris)
        export(iris, type = "ascii", file = "iris.txt")
        # Inspect the file 'iris.txt', and then...
        unlink("iris.txt")
    }
}

\keyword{IO}
\keyword{file}
