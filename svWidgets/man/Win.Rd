\name{Win}
\alias{tkWinAdd}
\alias{tkWinDelete}
\alias{WinGet}
\alias{WinNames}

\title{ Manipulate Windows }
\description{
  R can combine various windows (native, Tk, Gtk, etc.). There could be problems
  when a GUI uses various kinds of windows together. For instance, it is very
  difficult to define a modal window that is modal for the whole application. 
	These functions manage windows and ease their clean creation an destruction.
}
\usage{
  tkWinAdd(name = "tkwin1", parent = .TkRoot, title = NULL, pos = NULL, 
		bind.delete = TRUE, ...)
  tkWinDelete(name)
  WinGet(name)
  WinNames()
}

\arguments{
  \item{name}{ Name of a window }
  \item{parent}{ Parent of this window }
  \item{title}{ Title of the window }
  \item{pos}{ Where to place the window. A string like '+XX+YY' where XX is the 
	horizontal position in pixels, and YY is the vertical position. Using negative
	values place the window relative to the right or bottom side of the screen.
	Specifying \code{NULL} (by default) allows for automatic placement of the window. }
  \item{bind.delete}{ Do we automatically bind tkWinDelete to the windows delete envent? }
  \item{\dots}{ Additional options to pass to the window creator }
}

\details{
  The list of windows and pointers to their handles are stored in '.gui.Wins'
  in the 'TempEnv' environnement.
}

\value{
  \code{tkWinAdd()} and \code{WinGet()} return the handle to the newly created window.
  \code{WinNames()} return the list of all windows registered in .gui.Wins.
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{MenuReadPackage}}, \code{\link{tkImgReadPackage}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	# Creating and destroying a Tk window and inspecting the list
	WinNames()
	tkWinAdd("tt", title = "My win", pos ="-40+20")
	WinNames()
	tkwm.deiconify(WinGet("tt")) # Standard tcltk functions on the window
	tkWinDelete("tt")
	WinNames()
  }
}
\keyword{ utilities }
