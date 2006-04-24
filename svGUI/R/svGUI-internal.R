".onLoad" <-
function(lib, pkg) {
	# Check R version that has to be >= 2.0.0
	if (!(as.numeric(R.Version()$major) >= 2 || as.numeric(R.Version()$minor) >= 0))
		stop("The svGUI library requires at least R 2.0.0!")
    # We rely on these libraries:
    # Done in NAMESPACE
    #if (!require(svMisc))
	#    stop("svGUI library requires svMisc")
    #if (!Require(tcltk))
	#    stop("svGUI library requires tcltk")
    #if (!Require(R2HTML))
	#    stop("svGUI library requires R2HTML")

	## Store svGUI library path to know where are stored CSS files and others
    svPath <- Sys.getenv("SciViews_Home")
	## TO DO: use a different default value under Linux/Unix!
	if (svPath == "") svPath[1] <- "c:/progra~1/SciViews" # A default directory
    assignTemp(".svPath", svPath, replace.existing = FALSE)

    # Install GUI communication
    if (interactive()) guiInstall()
    if (.svPath != "" && .Platform$OS == "windows" && interactive()) {	# Automatically start SciViews R Console
		# If RConsole.exe does not exist where it should be, then, propose to start the installer!
		Rcons <- file.path(get(".svPath", pos = 1), "bin", "RConsole.exe")
		if (file.exists(Rcons)) { # OK, we can start it (with the -connect argument line)
			guiStart(paste("\"", Rcons, "\"",  " -connect", sep = ""))
		} else {	# Indicate that companion apps are not found!
			cat("Variable '.svPath' points to", svPath, "\n")
			cat("However, 'RConsole.exe' is not found in its ./bin subdirectory\n")
			cat("Make sure SciViews-R is installed and '.svPath' points to the right directory\n")
			cat("\nThe best way to tell where SciViews is installed is by setting the\n")
			cat("environment variable 'SciViews_Home' with a value corresponding to the actual\n")
			cat("directory where you have installed SciViews (in Unix notation, that is, using '/'.\n")
			cat("You can download setup of SciViews-R at http://www.sciviews.org/SciViews-R\n")
		}
	}
}

".onUnload" <-
function(libpath) {
	# Make sure the communication is stopped with the GUI client
    guiStop()
    guiUninstall()
}

".packageName" <- "svGUI"
