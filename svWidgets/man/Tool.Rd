\name{Tool}
\alias{ToolbarAdd}
\alias{ToolbarDel}
\alias{ToolbarType}
\alias{ToolAdd}
\alias{ToolDel}
\alias{ToolNames}
\alias{ToolItems}
\alias{ToolType}
\alias{ToolChange}
\alias{ToolState}
\alias{ToolRead}
\alias{ToolReadPackage}

\title{ Conveniently manipulate tools and toolbars, whatever the widgets used }
\description{
  These functions provide an unifying way of dealing with tools and toolbars in R.
  Currently, they support a little bit a functionnalities for tcltk2 widgets (tile)
  and most functions, except \code{ToolRead} are not finished.
}
\usage{
  ToolbarAdd(toolbar, ...)
  ToolbarDel(toolbar)
  ToolbarType(toolbar, warn = TRUE)
  ToolAdd(tool, ...)
  ToolDel(tool)
  ToolNames()
  ToolItems(toolbar)
  ToolType(tool, warn = TRUE)
  ToolChange(tool, action = "", image = "", accel = "", options = "")
  ToolState(tool, active = TRUE)
  ToolRead(file = "Tools.txt")
  ToolReadPackage(package, subdir = "gui", file = "Tools.txt")
}

\arguments{
  \item{toolbar}{ Name of a toolbar }
  \item{\dots}{ Further options }
  \item{warn}{ Do we issue a warning if the type of tool or toolbar is not recognized? }
  \item{tool}{ The name of a tool }
  \item{action}{ Action the tool triggers (R code) }
  \item{image}{ Name of an image to display at left of the menu item }
  \item{accel}{ Accelerator (keystroke) to use to trigger this menu item }
  \item{options}{ Additional options, for instance 'state = "disable"' to
	disable the tool at creation. }
  \item{active}{ Do we enable or disable the tool? }
  \item{file}{ A file containing menu specifications to read }
  \item{package}{ Name of a package from where to load tools and toolbars specifications }
  \item{subdir}{ Subdirectory in the package where the tools and toolbars specifications
    are stored. By default, it is the "gui" subdirectory. }
}

\details{
  These functions care about creating, deleting and managing custom tools and toolbars.
  Informations and handles to the various tools created with these functions
  are stored in the .gui.Tools variable, located in the TempEnv environment.
}

\value{
  \code{ToolAdd()} return the handle to the newly created tool.
  \code{ToolNames()} return the list of all tools registered in .gui.Tools.
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{MenuAdd}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk or tcltk2 package loaded
	## Run these commands one at a time and look at the toolbars being created...
	
    ### TODO: examples here!
  }
}
\keyword{ utilities }
