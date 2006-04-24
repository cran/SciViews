"report" <-
function(x, objname = deparse(substitute(x)), reptype = getOption("report"),
	application = getOption("reporter"), bookmark = "<End>", ...) {
	objname <- objname
	# Currently support only HTML or DOC (WinWord)
	if (is.null(reptype)) reptype <- "html"     # Default value
	if (reptype != "html" && reptype != "doc")
		stop("Only 'html' or 'doc' reptypes are currently supported!")

    # Compute the expression
	if (is.null(x)) { # Just start/activate the app...
		if (reptype == "html") {
		    system(paste("\"", application, "\" --activate", sep = ""), wait = FALSE)
		} else { # reptype == 'doc'
      		if (!WordOpen())
            	stop("Error while trying to start or activate Microsoft Word!")
		}
		return(invisible(TRUE))
	} else {
		xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
		if (inherits(xexp, "try-error") || is.null(xexp)) {
			xexp <- substitute(x)
			if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
				xexp <- parse(text = paste("`", xexp, "`", sep = ""))
			xexp <- as.expression(xexp)
		}
		file <- view(x = xexp, objname = objname, browse = FALSE, ...)
	}
	
	if (reptype == "html") {
		if (is.null(application))
			stop("No compatible HTML reporting application supported.\n Use view() and then copy and paste!")

		if (!.Platform$OS == "windows") { # Currently, no reporting app!
			cat("No reporting app yet. Data is in:", file, "\n")
		} else {
			system(paste("\"", application, "\" --addHTML=", file, sep = ""), wait = FALSE)
		}
	} else { # reptype == 'doc'
		if (!is.null(bookmark) && bookmark == "<End>") {
			# Move to the end of the document
	    	WordGotoEnd()
	    	# Add the text...
	    	WordInsertFile(file, keep.bookmark = FALSE)
		} else {
	    	# Give a bookmark, or allow the user to possibly choose another position
	    	WordGoto(bookmark = bookmark)
	    	# Add the text...
	    	WordInsertFile(file, keep.bookmark = TRUE)
		}
	}
	invisible(TRUE)
}
