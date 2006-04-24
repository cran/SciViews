"ToolbarAdd" <-
function(toolbar, ...) {
	res <- switch(ToolbarType(toolbar),
		tkToolbar = stop("Not implemented yet!"),
		tk2Toolbar = stop("Not implemented yet!"))
}

"ToolbarDel" <-
function(toolbar) {
	res <- switch(ToolbarType(toolbar),
		tkToolbar = stop("Not implemented yet!"),
		tk2Toolbar = stop("Not implemented yet!"))
}

"ToolbarType" <-
function(toolbar, warn = TRUE) {
	# Given a toolbar, return its type ("tkToolbar", "tk2Toolbar", NA)
	if (regexpr("^[$]Tk[.].+/", toolbar) > 0) return("tkToolbar") else
	if (regexpr("^[$]Tk2[.].+/", toolbar) > 0) return("tk2Toolbar") else {
        if (warn) warning("Unrecognized toolbar type for ", tool)
		return(NA)
	}
}

"ToolAdd" <-
function(tool, ...) {
	res <- switch(ToolType(tool),
		tkTool = stop("Not implemented yet!"),
		tk2Tool = stop("Not implemented yet!"))
}

"ToolDel" <-
function(tool) {
    res <- switch(ToolType(tool),
		tkTool = stop("Not implemented yet!"),
		tk2Tool = stop("Not implemented yet!"))
}

"ToolNames" <-
function() {
	res <- character(0)
	# Retrieve tool names
	res <- c(res, names(getTemp(".gui.Tools")))
	# eliminate toplevel entries
    if (length(res) > 0) res <- res[regexpr("/", res) > 0]
	return(res)
}

"ToolItems" <-
function(toolbar) {
    res <- switch(ToolbarType(toolbar),
		tkToolbar = stop("Not implemented yet!"),
		tk2Toolbar = stop("Not implemented yet!"))
}

"ToolType" <-
function(tool, warn = TRUE) {
	# Given a tool, return its type ("tkTool", "tk2Tool", NA)
	if (regexpr("^[$]Tk[.].+/", tool) > 0) return("tkTool") else
	if (regexpr("^[$]Tk2[.].+/", tool) > 0) return("tk2Tool") else {
        if (warn) warning("Unrecognized tool type for ", tool)
		return(NA)
	}
}

"ToolChange" <-
function(tool, action = "", image = "", accel = "", options = "") {
	# Change action, image, accel or options for tools
	res <- switch(ToolType(tool),
		tkTool = stop("Not implemented yet!"),
		tk2Tool = stop("Not implemented yet!"))
}

"ToolState" <-
function(tool, active = TRUE) {
	# Activate/inactivate tools
	res <- switch(ToolType(tool),
		tkTool = stop("Not implemented yet!"),
		tk2Tool = stop("Not implemented yet!"))
}

"ToolRead" <-
function(file = "Tools.txt") {	
	### TODO: allows to define a toolbar in plain tk and in tk2 (using tile)
	### TODO: create a .gui.Tools entry in TempEnv to store current tools config 
	# Read toolbars from a file
	T <- scan(file, character(0), sep = "\n", comment.char = "#", quiet = TRUE)
	# Split the lines into item, command, options
	T <- strsplit(T, "~~")
	# Strip leading and trailing spaces or tabs (used to align items in the file)
	T <- lapply(T, function(x) sub("[ \t]+$", "", sub("^[ \t]+", "", x)))
	# Move line after line and replace '|' by values
	N <- length(T)
	# Must have at least two entries
	if (N < 2) return(invisible())
	# First entry must be a toplevel, thus, it must start with '$'
	if (regexpr("^[$]", T[[1]][1]) < 0)
		stop("first entry is not a toplevel window!")
	toolLevels <- T[[1]][1]
	# Initialize a data frame to contain decrypted info
	dat <- rep("", N)
	L <- data.frame(tool = I(dat), item = I(dat), image = I(dat),
		action = I(dat), options = I(dat))
	Litem <- data.frame(tool = I(toolLevels), item = I(""), image = I(""),
		action = I("[top]"), options = I(""))
	L[1, ] <- Litem
	for (i in 2:N) {
		entry <- T[[i]][1]
		# Split on '|'
		split <- strsplit(entry, "[|]")[[1]]
		# Combine toolLevels & split
		last <- length(split)
		toolLevels[last] <- split[last]
		toolLevels <- toolLevels[1:last]
		# Recombine toolLevels for getting recalculated entry
		entry <- paste(toolLevels, collapse = "/")
		# Is this just a tool button, or a menu tool button/menu item?
		lastentry <- basename(entry)
		if (regexpr("^[$]", lastentry) > 0) { # This is a toolbar
			# Remove '$' before tool button entries
			tool <- gsub("/[$]", "/", entry)
			item <- ""      	# No item
			image <- ""     	# No image
			action <- if (last == 1) "[top]" else "[toolbar]"
			options <- ""       # No options (currently)
		} else {    # This is an menu entry in a tool button menu
			# tool = entry minus last item (remove '$' before tool entries)
			tool <- gsub("/[$]", "/", dirname(entry))
			# Decrypt lastentry to get image & item ([image]item)
			item <- sub("^[[][a-zA-Z0-9 ._-]+[]]", "", lastentry)
			if (item == lastentry) image <- "" else {
				image <- sub("^[[]([a-zA-Z0-9 ._-]+)[]].+$", "\\1", lastentry)
				# Since these are Tk images resources, I have to prefix '$Tk.'
				image <- paste("$Tk.", image, sep = "")
			}
			action <- T[[i]][2]
			if (is.na(action)) action <- ""
			options <- T[[i]][3]
        	if (is.na(options)) options <- ""
		}
    	Litem <- data.frame(tool = I(tool), item = I(item), image = I(image),
			action = I(action), options = I(options))
		# add it to the data.frame
		L[i, ] <- Litem
	}
	# The [top] entries are not needed
	L <- L[L$action != "[top]", ]

	# Execute this line-by-line to create the various tools
	N <- nrow(L)
	Toolbar <- NULL
	for (i in 1:N) {
		action <- L$action[i]
    	if (action == "[toolbar]") { 	# Create a toolbar
        	# Pack previous toolbar, if exists
			if (!is.null(Toolbar)) tkpack(Toolbar, side = "top", anchor = "w")
			Toolbar <- tk2frame(ZIDlgWin)
			#tkToolAdd(toolbar = L$menu[i])
		} else { # Create a tool in the toolbar
        	# Is it a separator?
			if (L$item[i] == "-") {
				sep <- tk2separator(Toolbar, orient = "vertical")
				tkgrid(sep, row = 0, column = i - 1, sticky = "nsew")
			} else {	# It is a tool button
				but <- tk2button(Toolbar, text = L$tool[i], image = ImgGet(L$image[i]),
					compound = "image", style = "Toolbutton",
					command = eval(parse(text = paste("function()", L$action[i])))) # And options?
				tkgrid(but, row = 0, column = i - 1, sticky = "nsew")
				tk2tip(but, L$item[i])
			}
			#tkToolAddItem(tool = L$tool[i], item = L$item[i], action = L$action[i],
			#	image = L$image[i], options = L$options[i])
		}
	}
	# Pack last toolbar
	if (!is.null(Toolbar)) tkpack(Toolbar, side = "top", anchor = "w")
}

"ToolReadPackage" <-
function(package, subdir = "gui", file = "Tools.txt") {
	# Create tools and toolbars using a definition file located in a R package
	dir <- file.path(.path.package(package = package)[1], subdir)
	# Check that the dir exists
	if (!file.exists(dir) || !file.info(dir)$isdir)
		stop("'", dir, "' does not exist, or is not a directory!")
	# Check that the file exists
	File <- file.path(dir, file)
	if (!file.exists(File))
		stop("'", file, "' not found in '", dir, "'!")
	# Read the tools and toolbars definition file
	res <- ToolRead(File)
	return(invisible(res))
}
