"guiStop" <-
function() {
    # Stop the GUI client
    # Done in NAMESPACE
    #require(svMisc)
    if (!.Platform$OS == "windows") { # non-Windows
        # Nothing to do yet, since guiStart do not work, currently!

    } else { # Windows
		# Make sure all graph devices are closed
		graphics.off()

		# Reset graph device to the original one (to do: better to save it before and restore the saved version!)
		options(device = "windows")

		# Reset default viewer and reporter
		options(viewer = NULL)
		options(reporter = NULL)
		options(view.title = NULL)

	    # Indicate to the client that he must quit
	    guiCmd("<<<<Quit")

	    # Reset original prompt
		options(prompt="> ")
		options(continue="+ ")

		# Eliminate SciViews.version
		options(SciViews.version = NULL)

		# Keep only first indication for .Platform$GUI
	    .Platform <- .Platform
	    .Platform$GUI <- .Platform$GUI[1]
	    assignTemp(".Platform", .Platform)
	}
	invisible(TRUE)
}

