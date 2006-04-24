\name{tkMenu}
\alias{tkMenuAdd}
\alias{tkMenuAddItem}
\alias{tkMenuDel}
\alias{tkMenuDelItem}
\alias{tkMenuItems}
\alias{tkMenuChangeItem}
\alias{tkMenuStateItem}
\alias{tkMenuInvoke}

\title{ Conveniently manipulate Tk menus }
\description{
  These functions provide an easier way to create and manipulate Tk menus
  under R. Note that the corresponding MenuXXX() function also manipulate
  Tk menus the same way, but are capable of manipulating other menus as well.
  One should, thus, preferably use MenuXXX() that tkMenuXXX()!
}
\usage{
  tkMenuAdd(menu, tearoff = FALSE)    
  tkMenuAddItem(menu, item, action, image = "", accel = "", options = "")
  tkMenuDel(menu)
  tkMenuDelItem(menu, item)
  tkMenuItems(menu)
  tkMenuChangeItem(menu, item, action = "", options = "")
  tkMenuStateItem(menu, item, active = TRUE)
  tkMenuInvoke(menu, item)  
}

\arguments{
  \item{menu}{ Name of a menu }
  \item{tearoff}{ Should the menu be detachable? }
  \item{item}{ Name of a menu item }
  \item{action}{ Action the menu triggers (R code) }
  \item{image}{ Name of an image to display at left of the menu item }
  \item{accel}{ Accelerator (keystroke) to use to trigger this menu item }
  \item{options}{ Additional options, for instance 'state = "disable"' to
	disable the menu at creation. }
  \item{active}{ Do we enable or disable the menu item? }
}

\value{
  \code{tkMenuAdd()} return the handle to the newly created menu.
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{MenuNames}} }

\examples{
  ## Look at the extensive example in ?MenuNames
}
\keyword{ utilities }
