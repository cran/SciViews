\name{guiViews}
\alias{guiViewsCmd}
\alias{guiViewsCSS}
\alias{guiViewsCSSChange}
\alias{guiViewsDir}
\alias{guiViewsDisplay}
\alias{guiViewsFile}

\title{ GUI functions to manipulate and generate Views }
\description{
  These function are internally used, and are not intended for direct manipulation.
}
\usage{
    guiViewsCmd(type, typelist, command, file, ...)
    guiViewsCSS()
    guiViewsCSSChange(newCSS = "")
    guiViewsDir()
    guiViewsDisplay(viewfile, viewer = getOption("viewer"), ...)
    guiViewsFile()
}

\arguments{
  \item{type}{ The type of View }
  \item{typelist}{ The list of all existing View types for this object }
  \item{command}{ The code interpreted by the Viewer to switch to a different View }
  \item{file}{ The file containing the View }
  \item{newCSS}{ The path to the new CSS (Cascading Stype Sheet) file to use }
  \item{viewfile}{ The file containing the View }
  \item{viewer}{ The viewer application used to display the View }
  \item{\dots}{ Further arguments passed to functions that generate the View }
}

\value{
  \code{guiViewsCSS()}, \code{guiViewsDir()}, and \code{guiViewsFile()} return the corresponding file or directory.
  \code{guiViewsCSSChange()} returns either \code{TRUE} or \code{FALSE}.
  \code{guiViewsCmd()} returns a string with the command.
  \code{guiViewsDisplay()} return the result returned by a call to the viewer.
}

\author{ Philippe Grosjean & Eric Lecoutre }

\seealso{ \code{\link{view}} }
\examples{
    guiViewsFile() # The name of temporary file suitable to hold a new View
}
\keyword{print}
\keyword{interface}
