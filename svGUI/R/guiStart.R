"guiStart" <-
function(gui = "\"%SciViews_Home%/bin/RConsole.exe\" -connect") {
    # Start the GUI client
    # Done in NAMESPACE
    #require(svMisc)
    if (!.Platform$OS == "windows") { # non-Windows
        ### TO DO...
        
    } else { # Windows
		# Make sure everything is installed correctly!
	    if (is.null(getTemp(".guiLinkTk", default = NULL)))
	        stop("GUI communication feature does not seem to be installed. Run 'guiInstall()' first!")

	    # Indicate that this is a SciViews program
		options(SciViews.version = 0.7-5)

		# Indicate that the GUI supports also SciViews TclTk exchange protocol
	    .Platform <- .Platform
	    .Platform$GUI <- c(.Platform$GUI[1], "SciViews TclTk GUI")
	    assignTemp(".Platform", .Platform)

		# Indicate that we want to use GUI Require
		options(guiRequire = TRUE)

		# Start the GUI program, if provided...
		if (!is.null(gui)) {
	        # Make the replacement for %SciViews_Home%
	        gui <- sub("\%SciViews_Home\%", gsub("\\\\", "/", getTemp(".svPath")), gui)
	        system(gui, wait = FALSE)
	    }

		# Set graph device to our special one
		options(device = "Windows")

		# Use the RConsole viewer
		options(viewer = "RConsole")

		# Define the location of the Report editor
		options(reporter = file.path(getTemp(".svPath"), "bin", "RReport.exe"))

		#options(view.title = FALSE)		# Disable views titles
		#options(view.maxsize = c(50, 20)) 	# matrix/data.frame maximum size with print views

		#### TO DO: redefine editor, pager, etc...

		# Change the prompt to indicate we are "SciViews-ready"
		options(prompt = "» ")
		options(continue = "÷ ")
	}

    # Done!
    invisible(TRUE)
}

