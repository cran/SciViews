"copy" <-
function(x, type = "raw", objname = deparse(substitute(x)), ...) {
	objname <- objname
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# The way to copy to the clipboard is platform-dependent
	if (.Platform$OS == "windows"){ # This is Windows
		export(x = xexp, type = type, file = "clipboard", append = FALSE, objname = objname, ...)
	} else { ### Rem: according to a suggestion by Ted Harding... not tested yet!
		File <- tempfile()
		export(x = xexp, type = type, file = File, append = FALSE, objname = objname, ...)
		system(paste("wxcopy <", File), wait = TRUE, invisible = TRUE) # TO DO: manage errors!
		unlink(File)
	}
}
