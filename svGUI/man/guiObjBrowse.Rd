\name{guiObjBrowse}
\alias{guiObjBrowse}
\alias{guiObjClear}
\alias{guiObjDir}
\alias{guiObjList}
\alias{guiObjMenu}
\alias{guiObjSearch}

\title{ Function to implement an object browser }
\description{
  These functions provide features required to implement a complete object browser in the GUI client.
}
\usage{
guiObjBrowse(id = "default", env.name = NULL, pos = NULL, all.names = NULL,
    pattern = NULL, group = NULL, regenerate = FALSE)
guiObjClear(id)
guiObjDir()
guiObjList(id = "default", env.name = NULL, pos = 1, all.names = FALSE,
    pattern = "", group = "", path = NULL, compare = TRUE)
guiObjMenu(id = "default", selobject, objects, envir, path = NULL)
guiObjSearch(path = NULL, compare = TRUE)
}

\arguments{
  \item{id}{ The id of the object browser (you can run several ones concurrently) }
  \item{env.name}{ The name of the environment }
  \item{pos}{ The position in the search path }
  \item{all.names}{ Do we display all names (including hidden variables starting with '.')? }
  \item{pattern}{ A pattern to match for selecting variables }
  \item{group}{ A group to filter }
  \item{regenerate}{ Do we force to regenerate the information? }
  \item{path}{ The path where to write a temporary file with the requested information }
  \item{compare}{ If \code{compare == TRUE}, result is compared with last cached value and the client is updated only if something changed }
  \item{selobject}{ The current selection in the object browser }
  \item{objects}{ A list with selected items in the object browser }
  \item{envir}{ The currently selected environment in the object browser }
}
\details{
  \code{guiObjBrowse} do the horsework. \code{guiObjDir} gets the temporary directory where exchange files are stored.
  \code{guiObjList} lists objects in a given workspace. \code{guiObjSearch} lists the search path.
  \code{guiObjClear} clears any reference to a given object browser. \code{guiObjMenu} computes a context menu for a given object.
}
\value{
  Depending on the function, a list, a string, or a reference to an external, temporary file where data is written.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiRemove}}, \code{\link{guiSave}} }
\examples{
    #### TO DO...
}
\keyword{misc}
