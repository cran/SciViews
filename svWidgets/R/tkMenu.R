"tkMenuItems" <-
function(menu) {
	M <- getTemp(".gui.Menus")[[menu]]
	if (is.null(M))
		stop("unable to retrieve items for ", menu, "\n(menu ", menu, " does not exist)")
	Mitems <- M$Items
	if (is.null(Mitems)) res <- character(0) else res <- as.character(Mitems$action)
	if (length(res) > 0) names(res) <- Mitems$name
	return(invisible(res))
}

"tkMenuAdd" <-
function(menu, tearoff = FALSE) {
	require(tcltk)
	require(svMisc)
	# Get the menu name
	Mname <- basename(menu)
	# Get the name of the parent
	Pname <- dirname(menu)
	# Look if the parent exists (must be in .gui.Menus in TempEnv)
	.gui.Menus <- getTemp(".gui.Menus")
	# Do not create the menu if it already exists
	if (menu %in% names(.gui.Menus))
		stop("menu ", menu, " already exists!")
	Parent <- .gui.Menus[[Pname]]
	if (is.null(Parent)) {
        # If base menu is a "root", try to create the corresponding Tk top menu
        if (regexpr("/", Pname) < 0) {
			# Get the name of the Tk window that hosts the menu
			TkWinName <- sub("^[$]Tk[.]", "", Pname)
			# Look if such a Tk window is recorded in .gui.Wins
			TkWin <- WinGet(TkWinName)
			if (is.null(TkWin))
				stop("menu does not exist, and the parent Tk window cannot be found!")
			# Create the menu
			Parent <- tkmenu(TkWin)
			tkconfigure(TkWin, menu = Parent)
			# Add an entry for this top menu in .gui.Menus
			.gui.Menus[[Pname]] <- Parent
		} else stop("unable to add menu\n(base menu does not exists)")
	}
	# Check that the name is not already used (for a menu entry, for instance)
    items <- Parent$Items
    if (!is.null(items) && Mname %in% items$name)
		stop("the menu name is already used in this menu!")
	# Now that the parent menu exists, we can create this menu
	# Look where to place the underline (menu shortcut)
    Under <- regexpr("&", Mname)
	if (Under < 0) {    # No '&', place the underline at first position
		### TODO: calculate best position according to entries already in the menu
		Under <- 0
		item <- Mname
	} else {
		Under <- Under - 1 # Because Tk starts numbering at 0
		item <- sub("&", "", Mname)
	}
	Child <- tkmenu(Parent, tearoff = tearoff)
    tkadd(Parent, "cascade", label = item, menu = Child, underline = Under)
 	# Add an entry for this child menu in .gui.Menus
	.gui.Menus[[menu]] <- Child
    # ... and register it in the items of parent menu in .gui.Menus
	if (tearoff) options <- "tearoff = TRUE" else options <- "tearoff = FALSE"
	entry <- data.frame(name = I(Mname), action = I("[menu]"), image = (""),
		accel = I(""), options = I(options))
	if (is.null(items))
		.gui.Menus[[Pname]]$Items <- entry
	else
        .gui.Menus[[Pname]]$Items <- rbind(items, entry)
	# Update the TempEnv version of .gui.Menus
	assignTemp(".gui.Menus", .gui.Menus)
}

"tkMenuAddItem" <-
function(menu, item, action, image = "", accel = "", options = "") {
	require(tcltk)
	require(svMisc)
	# Look if the menu exists (must be in .gui.Menus in TempEnv)
	.gui.Menus <- getTemp(".gui.Menus")
	M <- .gui.Menus[[menu]]
	if (is.null(M)) {
		# On the contrary to winMenuAddItem(), if the menu does not exist yet
		# we generate an error (but we can later change this behaviour, of course!)
		stop("menu does not exist!")
	}
	# Look if the menu already exists
	Items <- M$Items
	if (!is.null(Items) && item %in% Items$name) {
        # On the contrary to winMenuAddItem(), it is not allowed to add twice the same
		# menu (would change value, but in Tk, it would add a second time the same menu)
		stop("the menu item ", item, " already exists!")
	}
	# Add the entry at the end of the Tk menu (### TODO: allow other positions)
	# First look if it is a command or a separator
	if (regexpr("^-+$", item) > 0) { #This must be a command
		tkadd(M, "separator")
		action <- "[separator]"
		options <- ""
	} else { # This is a menu command
		# Look for the '&', indicating where the underline should be located
		Under <- regexpr("&", item)
		if (Under < 0) {    # No '&', place the underline at first position
			### TODO: calculate best position according to entries already in the menu
			Uopt <- ", underline = 0"
			lbl <- item
		} else {
			Uopt <- paste(", underline =", Under - 1) # Because Tk starts numbering at 0
			lbl <- sub("&", "", item)
		}
		# Do we have to add an image to the menu?
		if (image != "") {
			# Look if the image resource is available
			Img <- ImgGet(image)
			if (!is.null(Img)) {
				Iopt <- paste(", image = '", as.character(Img), "'", sep ="")
			} else Iopt <- ""
		} else Iopt <- ""
        # Do we have an accelerator defined for this menu?
		if (accel != "") {
			Aopt <- paste(', accelerator = "', accel, '"', sep = "")
			# Compute the Tk accelerator and make corresponding binding
			tkAccel <- paste("<", tolower(accel), ">", sep ="")
			# 'ctrl+' becomes 'Control-'
			tkAccel <- sub("ctrl[+]", "Control-", tkAccel)
			# 'shift+' becomes 'Shift-'
        	tkAccel <- sub("shift[+]", "Shift-", tkAccel)
        	# 'alt+' becomes 'Alt-'
        	tkAccel <- sub("alt[+]", "Alt-", tkAccel)
			# Get parent window name
			pWin <- sub("^[$]Tk[.]([a-zA-Z0-9 _.-]+)/.*$", "\\1", menu)
			# Create the binding
			cmd <- paste('tkbind(WinGet("', pWin, '"), "', tkAccel,
				'", function() tkMenuInvoke("', menu, '", "', item, '"))' , sep = "")
			eval(parse(text = cmd))
		} else Aopt <- ""
		# Rework options
		if (options == "") opts <- "" else opts <- paste(",", options)
		cmd <- paste('tkadd(M, "command", label = "', lbl,
			'", command = function() ', action, ', compound = "left"', Iopt, Aopt, Uopt, opts, ')', sep = "")
		eval(parse(text = cmd))
	}
	# Register this menu entry in .gui.Menus
	entry <- data.frame(name = I(item), action = I(action), image = I(image),
		accel = I(accel), options = I(options))
	items <- .gui.Menus[[menu]]$Items
	if (is.null(items))
		.gui.Menus[[menu]]$Items <- entry
	else
        .gui.Menus[[menu]]$Items <- rbind(items, entry)
	# Update the TempEnv version of .gui.Menus
	assignTemp(".gui.Menus", .gui.Menus)
}

"tkMenuDelItem" <-
function(menu, item) {
	require(tcltk)
	require(svMisc)
	# Look if the menu exists (must be in .gui.Menus in TempEnv)
	.gui.Menus <- getTemp(".gui.Menus")
	M <- .gui.Menus[[menu]]
	if (is.null(M)) return(invisible())
	# Look if the item exists
	Items <- M$Items
	Pos <- which(Items$name == item) - 1 # Because Tk menu indices start at 0
	if (length(Pos) == 0) return(invisible())
	# Check that this item is not a submenu (must use tkMenuDel() instead)
	if (Items$action[Pos + 1] == "[menu]")
		stop("item ", item, " is a submenu. Use tkMenuDel() instead!")
	# Delete that entry
	tkdelete(M, Pos)
	# Eliminate that entry from .gui.Menus
	Items <- Items[Items$name != item, ]
	.gui.Menus[[menu]]$Items <- Items
	# Update the TempEnv version of .gui.Menus
	assignTemp(".gui.Menus", .gui.Menus)
}

"tkMenuDel" <-
function(menu) {
	# Delete a whole menu and all submenus
	require(tcltk)
	require(svMisc)
	# Look if the menu exists (must be in .gui.Menus in TempEnv)
	.gui.Menus <- getTemp(".gui.Menus")
	M <- .gui.Menus[[menu]]
	if (is.null(M)) return(invisible())
	# Look at all submenus to delete
	Menus <- names(.gui.Menus)
	Mmatch <- (substr(Menus, 1, nchar(menu)) == menu)
	dMenus <- sort(Menus[Mmatch], decreasing = TRUE) # Sort menus from bottom to top
	# Delete each menu in turn
	for (i in 1:length(dMenus)) {
		Pname <- dirname(dMenus[i])
		P <- .gui.Menus[[Pname]]
		N <- basename(dMenus[i])
		Items <- P$Items
		Pos <- which(P$Items$name == N)
		# If this is not a toplevel menu, Pos is Pos - 1
		### TODO: consider also tearoff menus that way!
		if (regexpr("/", Pname) > 0) Pos <- Pos - 1
		if (length(Pos) > 0) {
			tkdelete(P, Pos)
			.gui.Menus[[Pname]]$Items <- Items[Items$name != N, ]
		}
		.gui.Menus[[dMenus[i]]] <- NULL     # Eliminate the entry
	}
	# Update the TempEnv version of .gui.Menus
	assignTemp(".gui.Menus", .gui.Menus)
}

"tkMenuChangeItem" <-
function(menu, item, action = "", options = "") {
	# The Tk version of MenuChangeItem()
	require(tcltk)
	require(svMisc)
	# Look if the menu exists (must be in .gui.Menus in TempEnv)
	.gui.Menus <- getTemp(".gui.Menus")
	M <- .gui.Menus[[menu]]
	if (is.null(M)) return(invisible())
	# Look if the item exists
	Items <- M$Items
	Pos <- which(Items$name == item) - 1 # Because Tk menu indices start at 0
	if (length(Pos) == 0) return(invisible())
	# Check that this item is not a submenu or a separator
	Act <- Items$action[Pos + 1]
	if (Act == "[separator]")
		stop("item ", item, " is a separator; its state cannot be changed!")
    if (Act == "[menu]")
		stop("item ", item, " is a menu; its state cannot be changed!")
	if (options != "") {
		# Change the configuration of that entry
		eval(parse(text = paste("tkentryconfigure(M, Pos, ", options, ")", sep = "")))
	}
	# Do we need to change the action?
	if (action != "" && action != Act) {
		# Change the action
        cmd <- paste('tkentryconfigure(M, Pos, command = function() ', action, ')', sep = "")
		eval(parse(text = cmd))
		# Update .gui.Menus
		Items$action[Pos + 1] <- action
		.gui.Menus[[menu]]$Items <- Items
		# Update the TempEnv version of .gui.Menus
		assignTemp(".gui.Menus", .gui.Menus)
	}
}

"tkMenuStateItem" <-
function(menu, item, active = TRUE) {
	# The Tk version of MenuStateItem()
	require(tcltk)
	require(svMisc)
	# Look if the menu exists (must be in .gui.Menus in TempEnv)
	.gui.Menus <- getTemp(".gui.Menus")
	M <- .gui.Menus[[menu]]
	if (is.null(M)) return(invisible())
	# Look if the item exists
	Items <- M$Items
	Pos <- which(Items$name == item) - 1 # Because Tk menu indices start at 0
	if (length(Pos) == 0) return(invisible())
	# Check that this item is not a separator
	if (Items$action[Pos + 1] == "[separator]")
		stop("item ", item, " is a separator; its state cannot be changed!")
	# Set state for that entry
	State <- if (active) "normal" else "disabled"
	tkentryconfigure(M, Pos, state = State)
}

"tkMenuInvoke" <-
function(menu, item) {
	# Given a menu and an item in this menu, trigger the item action
	require(tcltk)
	require(svMisc)
	# Look if the menu exists (must be in .gui.Menus in TempEnv)
	.gui.Menus <- getTemp(".gui.Menus")
	M <- .gui.Menus[[menu]]
	if (is.null(M)) return(invisible())
	# Look if the item exists
	Items <- M$Items
	Pos <- which(Items$name == item) - 1 # Because Tk menu indices start at 0
	if (length(Pos) == 0) return(invisible())
	# Invoke this menu entry
	tcl(M, "invoke", Pos)
}
