"guiUninstall" <-
function() {
    # Done in NAMESPACE
    #require(svMisc)

    if (!.Platform$OS == "windows") { # non-Windows
        ### TO DO...
#        # Eliminate SciViews.TclTk
#		options(SciViews.socks = NULL)
#
#		# Unregister the TaskCallback
#	    # Use getTaskCallbackNames() to know if some tasks are registered
#	    Callback.Id <- getTemp(".guiObjCallbackId", default = NULL)
#	    if (!is.null(Callback.Id)) {
#	        if (removeTaskCallback(Callback.Id)) assignTemp(".guiObjCallbackId", NULL)
#	    }

    } else { # Windows
		# Eliminate SciViews.TclTk
		options(SciViews.TclTk = NULL)

		# Unregister the TaskCallback
	    # Use getTaskCallbackNames() to know if some tasks are registered
	    Callback.Id <- getTemp(".guiObjCallbackId", default = NULL)
	    if (!is.null(Callback.Id)) {
	        if (removeTaskCallback(Callback.Id)) assignTemp(".guiObjCallbackId", NULL)
	    }

		# Destroy the tcltk window used for communication
		e <- try(tkdestroy(getTemp(".guiLinkTk")), silent = TRUE)
		e <- rmTemp(".guiLinkTk")
	}
	invisible(TRUE)
}

