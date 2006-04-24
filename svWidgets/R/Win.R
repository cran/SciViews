"WinGet" <-
function(name) {
	# Retrieve a "tkwin" object from .gui.Wins, given its name
	return(getTemp(".gui.Wins")[[name]])
}

"WinNames" <-
function() {
	# List all recorded tk windows in .gui.Wins
	### TODO: if Rgui, list also console, graph, editors and pagers!
	return(names(getTemp(".gui.Wins")))
}

"tkWinAdd" <-
function(name = "tkwin1", parent = .TkRoot, title = NULL, pos = NULL,
	bind.delete = TRUE, ...) {
	# Verify the name is valid and not yet in use
	if (is.null(name) || !inherits(name, "character"))
		stop("'name' must be a character string!")
	name <- name[1]
	.gui.Wins <- getTemp(".gui.Wins")
	if (!is.null(.gui.Wins[[name]]))
		stop("name ", name, " is already used!")
	# It is fine. We can create the tktoplevel window
	win <- tktoplevel(parent = parent, ...)
	if (!inherits(win, "tkwin")) {
		tkdestroy(win)
		stop("A problem occured while creating the window!")
	}
	if (!is.null(title)) tktitle(win) <- title[1]
	# Possibly position the window
	if (!is.null(pos)) tkwm.geometry(win, pos)
    if (bind.delete) {
		# Define action when clicking on 'X'
		#tkbind(win "<Destroy>", function() {tkWinDelete(name); tkdestroy(win)})
		tkwm.protocol(win, "WM_DELETE_WINDOW", function() tkWinDelete(name))
	}
	### TODO: change other characteristics of the window here...
	# Record the window in the list
	.gui.Wins[[name]] <- win
	assignTemp(".gui.Wins", .gui.Wins)
	return(win)
}

"tkWinDelete" <-
function(name) {
	# Same action as tkdestroy(), but cares about deleting relating resources
	# (.gui.XXX) from TempEnv
    win <- WinGet(name)
    # Delete the name from the windows list
	.gui.Wins <- getTemp(".gui.Wins")
	.gui.Wins[[name]] <- NULL
	# Delete menu resources
	.gui.Menus <- getTemp(".gui.Menus")
	Menus <- names(.gui.Menus)
	# Eliminate all menus starting with "$Tk.[name]"
	tkMenuDel(paste("$Tk.", name, sep = ""))
	### TODO: delete other resources as toolbars, etc...
	assignTemp(".gui.Wins", .gui.Wins)
	if (!is.null(win)) tkdestroy(win)
}
