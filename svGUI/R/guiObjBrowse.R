"guiObjBrowse" <-
function(id = "default", env.name = NULL, pos = NULL, all.names = NULL, pattern = NULL, group = NULL, regenerate = FALSE) {
    # A simple function that maintains files for remote Object Browser
    # If four first parameters are NULL, use cached version of these parameters, or default values

    # Done in NAMESPACE
    #require(svMisc)
    # Does the svObjBrowser subdirectory exists in R temp dir?
	Root <- guiObjDir()
	if (!file.exists(Root) || !file.info(Root)$isdir) {
		unlink(Root)       # Make sure to destroy it if it is a file!
		if (!dir.create(Root))
			stop("Impossible to create the Object Browser root directory!")
	}
	
	# Control that 'Search.txt' is up-to-date
	ChangedSearch <- guiObjSearch(path = Root, compare = !regenerate)
	
	# Make sure id is character
	id <- as.character(id)
	if (id == "") id <- "default"
	
	# Get pos if env.name is provided
	if (!is.null(env.name)) {
		pos <- match(env.name, search())
		if (is.na(pos)) pos <- 1	# Default value when no match (go back to .GlobalEnv)
	}
	if (!is.null(pos)) {
		# Make sure that pos and env.name are correct and consistent
		env.name <- search()[pos]
		if (is.na(env.name)) {
			pos <- 1
			env.name <- ".GlobalEnv"
		}
	}
	
	# Get the five parameters pos, envir, all.names, pattern & group
    allPars <- getTemp(".guiObjParsCache", default = NULL)
    if (!is.null(allPars)) {
    	Pars <- allPars[[id]]
    } else {
    	Pars <- list(pos = 1, envir=".GlobalEnv", all.names = FALSE, pattern = "", group = "")
    	assignTemp(".guiObjParsCache", list())	# Create the list
    }
    if (is.null(Pars)) Pars <- list(pos = 1, envir=".GlobalEnv", all.names = FALSE, pattern = "", group = "") 
    # Possibly change some parameters (and make sure they are valid!)
	ParsChanged <- FALSE
	if (!is.null(pos)) {
        ParsChanged <- TRUE
		Pars$pos <- as.integer(pos[1])
		Pars$envir <- env.name
		if (Pars$pos < 1) { ParsChanged <- TRUE; Pars$pos <- 1; Pars$envir <- ".GlobalEnv"}
	}
	if (Pars$pos > length(search())) { ParsChanged <- TRUE; Pars$pos <- 1; Pars$envir <- ".GlobalEnv"}
	# Track possible changes in the search path
	if (is.null(env.name) && is.null(pos)) {
		if (is.na(match(Pars$envir, search()))) { ParsChanged <- TRUE; Pars$pos <- 1; Pars$envir <- ".GlobalEnv"}
		if (match(Pars$envir, search()) != Pars$pos) { ParsChanged <- TRUE; Pars$pos <- match(Pars$envir, search())}
	}
	# Track changes in the options
	if (!is.null(all.names)) {ParsChanged <- TRUE; Pars$all.names <- as.logical(all.names[1])}
	if (!is.null(pattern)) {ParsChanged <- TRUE; Pars$pattern <- as.character(pattern[1])}
	if (!is.null(group)) {ParsChanged <- TRUE; Pars$group <- as.character(group[1])}
	 # Write a cached version of these parameters in TempEnv
	 allPars <- getTemp(".guiObjParsCache", default = list())
	 allPars[[id]] <- Pars
	 assignTemp(".guiObjParsCache", allPars)

    # Control that 'List_<id>.txt' is up-to-date, but only if pos == 1 or regenerate or Pars or Search have changed
    # to limit the work done on workspaces that are unlikely to have change (ex: in package:base)!
	if (Pars$pos == 1 || regenerate || ParsChanged || ChangedSearch) {
	    ChangedList <- guiObjList(id = id, pos = Pars$pos, all.names = Pars$all.names, pattern = Pars$pattern,
	    	group = Pars$group, path = Root, compare = !regenerate)
	} else ChangedList <- FALSE

	# If something changed, warn the GUI client he must update its object browser
    Cmd <- ""
	if (ChangedSearch) Cmd <- "<<<<Search"
	if (ChangedList) Cmd <- paste(Cmd, "<<<<List ", id, sep="")
    return(guiCmd(Cmd))
}

