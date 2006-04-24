\name{Menu}
\alias{MenuAdd}
\alias{MenuAddItem}
\alias{MenuDel}
\alias{MenuDelItem}
\alias{MenuNames}
\alias{MenuItems}
\alias{MenuType}
\alias{MenuChangeItem}
\alias{MenuStateItem}
\alias{MenuRead}
\alias{MenuReadPackage}

\title{ Conveniently manipulate menus, whatever the window }
\description{
  These functions provide an unifying way of dealing with menus in R.
  It is ispired from the winMenuXXX() functions under Windows that allow
  easy manipulation of custom menus in Rgui.exe. Currently, they support all
  the functionnalities of winMenuXXX() functions and they bring a convenient
  similar layer on top of Tk menus to manipulate them in a similar way.
}
\usage{
  MenuAdd(menu, ...)  
  MenuAddItem(menu, item, action, image = "", accel = "", options = "")
  MenuDel(menu)
  MenuDelItem(menu, item)
  MenuNames()
  MenuItems(menu)
  MenuType(menu, warn = TRUE)
  MenuChangeItem(menu, item, action = "", options = "")
  MenuStateItem(menu, item, active = TRUE)
  MenuRead(file = "Menus.txt")
  MenuReadPackage(package, subdir = "gui", file = "Menus.txt")
}

\arguments{
  \item{menu}{ Name of a menu }
  \item{item}{ Name of a menu item }
  \item{action}{ Action the menu triggers (R code) }
  \item{image}{ Name of an image to display at left of the menu item }
  \item{accel}{ Accelerator (keystroke) to use to trigger this menu item }
  \item{options}{ Additional options, for instance 'state = "disable"' to
	disable the menu at creation. }
  \item{\dots}{ Further options }
  \item{warn}{ Do we issue a warning if the type of menu is not recognized? }
  \item{active}{ Do we enable or disable the menu item? }
  \item{file}{ A file containing menu specifications to read }
  \item{package}{ Name of a package from where to load menu specifications }
  \item{subdir}{ Subdirectory in the package where the menu specifications are stored.
	By default, it is the "gui" subdirectory. }
}

\details{
  These functions care about creating, deleting and managing custom menus.
  Informations and handles to the various menus created with these functions
  are stored in the .gui.Menus variable, located in the TempEnv environment.
}

\value{
  \code{MenuAdd()} return the handle to the newly created menu.
  \code{MenuNames()} return the list of all menus registered in .gui.Menus.
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{MenuNames}}, \code{\link{winMenuChangeItem}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded
	## Run these commands one at a time and look at menus...
	
	# Create menus in Rgui, using a specification file
	MenuReadPackage("svWidgets")
	MenuNames()
	(MenuItems("$ConsoleMain/Testit"))
	
	# Create menus manually in Rgui
    MenuAdd("$ConsoleMain/Testit2")
	MenuAddItem("$ConsoleMain/Testit2", "Trial", "ls()")
	MenuNames()
	(MenuItems("$ConsoleMain/Testit2"))
	MenuStateItem("$ConsoleMain/Testit2", "Trial", FALSE)
	MenuStateItem("$ConsoleMain/Testit2", "Trial", TRUE)
	# This is buggy! -> MenuChangeItem("$ConsoleMain/Testit2", "Trial", "search()")
	(MenuItems("$ConsoleMain/Testit2"))
	
	# Create and manipulate Tk menus
	tkWinAdd("tt", title = "A Tk window with menus", pos ="-40+20")
	MenuAdd("$Tk.tt/Misc")
	MenuNames()
	(MenuItems("$Tk.tt/Misc"))   # Still nothing in it
	MenuAddItem("$Tk.tt/Misc", "List &variables", "print(ls(envir = .GlobalEnv))")
	MenuAddItem("$Tk.tt/Misc", "Say &yo!", "cat('yo!\n')")

	MenuDelItem("$Tk.tt/Misc", "Say &yo!")
	MenuAddItem("$Tk.tt/Misc", "-")
	MenuAddItem("$Tk.tt/Misc", "&Say yo! twice", "cat('yo! yo!\n')")
	(MenuItems("$Tk.tt/Misc"))

	tkImgReadPackage("svWidgets")	# Make sure images are loaded
	MenuAdd("$Tk.tt/Misc/Sub&Menu")
	MenuAddItem("$Tk.tt/Misc/Sub&Menu", "T&rial", "cat('Trial trigerred!\n')")
	MenuAddItem("$Tk.tt/Misc", "Tria&l2", "cat('Trail with image and accel!\n')",
		image = "$Tk.butOpen", accel = "Ctrl+T")
	MenuNames()
	(MenuItems("$Tk.tt/Misc"))
	MenuStateItem("$Tk.tt/Misc", "Tria&l2", FALSE)
	MenuStateItem("$Tk.tt/Misc", "Tria&l2", TRUE)
	MenuStateItem("$Tk.tt/Misc", "Sub&Menu", FALSE)
	MenuStateItem("$Tk.tt/Misc", "Sub&Menu", TRUE)
	MenuChangeItem("$Tk.tt/Misc", "Tria&l2", options = "underline = 1")
	# This is the way to change binding
	tkbind(WinGet("tt"), "<Control-r>", function() tkMenuInvoke("$Tk.tt/Misc", "Tria&l2"))
	MenuChangeItem("$Tk.tt/Misc", "Tria&l2", action = 'cat("new action for Tria&l2!\n")')
	tkMenuInvoke("$Tk.tt/Misc", "Tria&l2")
	(MenuItems("$Tk.tt/Misc"))
	MenuDelItem("$Tk.tt/Misc", "Tria&l2")
	MenuDel("$Tk.tt/Misc")
	MenuNames()
	(MenuItems("$Tk.tt/Misc"))
	tkWinDelete("tt")
  }
}
\keyword{ utilities }
