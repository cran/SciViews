"report" <-
function(x, objname = deparse(substitute(x)), reptype = "html", application = getOption("reporter"), ...) {
	objname <- objname
	# Currently support only HTML
	if (reptype != "html")
		stop("Only 'html' reptype currently supported!")
	if (is.null(application))
		stop("No compatible reporting application supported.\n Use view() and then copy and paste!")
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	file <- view(x = xexp, objname = objname, browse = FALSE, ...)
	if (!.Platform$OS == "windows") { # Currently, no reproting app!
		cat("No reporting app yet. Data is in:", file, "\n")
	} else {
		system(paste("\"", application, "\" --addHTML=", file, sep = ""), wait = FALSE)
	}
	invisible(TRUE)
}
