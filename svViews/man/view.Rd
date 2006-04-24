\name{view}
\alias{view}
\alias{view.data.frame}
\alias{view.default}
\alias{view.function}
\alias{view.matrix}
\alias{view.princomp}
\alias{view.trellis}
\alias{view.ts}

\title{ View the content of an object in a HTML document }
\description{
  Views are rich-formatted presentations of the content of \R objects. They provide
  an easy way to inspect them, combining formatted text, tables and graphs.
}
\synopsis{
    view(x, type = "summary", objname = deparse(substitute(x)), ...)
    view.data.frame(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...)
    view.default(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...)
    view.function(x, type = "print", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...)
    view.matrix(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...)
    view.princomp(x, type = "print", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...)
    view.trellis(x, type = "print", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...)
    view.ts(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...)
}

\usage{
    view(x, type = "summary", objname = deparse(substitute(x)), \dots)
}

\arguments{
  \item{x}{ An R object }
  \item{type}{ The type of view to display. use \code{type = "typelist"} to get the list
    of views available for a given object }
  \item{objname}{ The name to use in the title of the view. By default, it is the actual name
    of the object }
  \item{\dots}{ Further arguments (depending on the object and on \code{type} }
}

\value{
  The name of a temporary file containing the generated view.
}

\author{ Eric Lecoutre & Philippe Grosjean }

\seealso{ \code{\link{report}}, \code{\link{viewHTMLinit}} }
\examples{
    \dontrun{
        data(iris)
        view(iris)
        iris.pc <- princomp(iris[, 1:4])
        view(iris.pc)
        view(ls)
    }
}
\keyword{print}
\keyword{interface}
