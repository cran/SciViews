\name{winMenu}
\alias{winMenuChangeItem}
\alias{winMenuStateItem}

\title{ Additional winMenuXXX() functions }
\description{
  A couple of winMenuXXX() functions are defined in the package 'utils'
  to manipulate custom menus of Rgui (under windows only). Here are some
  additional ones. note that you should preferrably use the corresponding
  MenuXXX() function defined in this package (they work with windows menus
  as well as other menus, like Tk.
}
\usage{
  winMenuChangeItem(menu, item, action, options = "")
  winMenuStateItem(menu, item, active = TRUE)
}

\arguments{
  \item{menu}{ Name of a menu }
  \item{item}{ Name of a menu item }
  \item{action}{ Action the menu triggers (R code) }
  \item{options}{ Additional options, for instance 'state = "disable"' to
	disable the menu at creation. }
  \item{active}{ Do we enable or disable the menu item? }
}

\details{
  These functions complement the winMenuXXX() functions in package 'utils'.
  Do prefer to use the corresponding MenuXXX() functions that work with all
  types of menus (currently, only Windows and Tk, but more will be added in the future).
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{MenuChangeItem}}, \code{\link{MenuStateItem}} }

\examples{
   ## See ?MenuNames for extensive examples
}
\keyword{ utilities }
