"export.matrix" <-
function(x, type = "raw", file, append = FALSE, objname = deparse(substitute(x)), ...) {
	if (is.null(file) || file == "clipboard") append = FALSE # Make sure we do not append to the clipboard!
	objname <- objname
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'export.matrix..type()'?
	custom <- paste("export.matrix..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, append = append, ...)
	} else {	
		# Essentially call export.data.frame to ensure columns have names
		res <- export.data.frame(x = xexp, type = type, file = file, append = append, objname = objname, ...)
	}
	if (type == "typelist") return(res) else invisible(res)
}
