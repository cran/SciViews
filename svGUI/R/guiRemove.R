"guiRemove" <-
function(list = character(0), inherits = FALSE, ask =TRUE) {
	# Note: guiRemove works only under Windows... under otherf platforms, no confirmation yet!
 	if (!.Platform$OS == "windows") { # Never ask, currently
        remove(list = list, pos = 1, inherits = inherits)	# Only deal with objects from .GlobalEnv!
    } else { # This is Windows
		# Essentially asks for a confirmation
		if (ask == FALSE || winDialog("okcancel", paste("Are you sure you want to delete\n", paste(list, collapse = ", "), "?", sep="")) == "OK")
			remove(list = list, pos = 1, inherits = inherits)	# Only deal with objects from .GlobalEnv!
	}
	invisible(TRUE)
}

