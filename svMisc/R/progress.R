"progress" <-
function(value, max.value = NULL) {
	# A progress indicator in the R console
	if (!is.numeric(value))
		stop("`value' must be numeric!")
	if (is.null(max.value)) {
		max.value <- 100
		percent <- TRUE
	} else percent <- FALSE
	if (!is.numeric(max.value))
		stop("`max.value' must be numeric or NULL!")
	# If value is higher than max.value, we erase the message
	erase.only <- (value > max.value)
	# Now that everything is OK, we can proceed
	# We work only with integer part of the values
	# and transform them into strings of same length
	max.value <- as.character(round(max.value))
	l <- nchar(max.value)
	value <- formatC(round(value), width = l)
	# Treatment is different if it is 'x%' or 'x on y' display type
	if (percent) {
		backspaces <- paste(rep("\8", l + 14), collapse = "")
		if (erase.only) message <- "" else
			message <- paste("Progress: ", value, "%  ", sep = "")
		cat(backspaces, message, sep = "")
	} else {
		backspaces <- paste(rep("\8", 2 * l + 16), collapse = "")
		if (erase.only) message <- "" else
			message <- paste("Progress: ", value, " on ", max.value, "  ", sep = "")
		cat(backspaces, message, sep = "")	
	}
	# Under Windows, make sure the message is actualized
	if (.Platform$OS.type == "windows") flush.console()	
	invisible(NULL)
}
