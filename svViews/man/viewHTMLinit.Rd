\name{viewHTMLinit}
\alias{viewHTMLinit}

\title{ Initialize a View }
\description{
  Create a HTML file and place a correct header in it.
}
\usage{
    viewHTMLinit(objname, type, file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", view.title = getOption("view.title"))
}

\arguments{
  \item{objname}{ Name of the object }
  \item{type}{ Type of View }
  \item{file}{ Name of the file where the View is created }
  \item{CSSFile}{ Name of the CSS (Cascading Style Sheet) file used }
  \item{command}{ The command to use for switching to a different view (internally used by SciViews-compatible applications) }
  \item{view.title}{ If \code{TRUE}, a title is added, otherwise no }
}

\value{
    Returns the file name invisibly.
}
  
}

\author{ Eric Lecoutre & Philippe Grosjean }

\seealso{ \code{\link{view}} }
\keyword{print}
\keyword{interface}
