"CallTip" <-
function(code, only.args = FALSE, location = FALSE) {
	# Get a call tip, given a part of the code
	# Extract the last variable name, given it is either at the end, or terminated by '('
	code <- sub(" *\\($", "", code[1])
	pos <- regexpr("[a-zA-Z0-9_\\.]+$", code, useBytes = TRUE)
	code <- substring(code, pos)

	# Get the corresponding Call Tip
	ctip <- "" # Default value, in case the function does not exist
	if (code != "" && exists(code, where = 1, mode = "function"))
        ctip <- Args(code, only.args = only.args)
	# Do we need to append an indication of where this function is located?
	if (location == TRUE) {
 		pkg <- sub("^package:", "", find(code, mode = "function"))
	    if (length(pkg) > 0 && pkg != ".GlobalEnv") ctip <- paste(ctip, " [", pkg, "]", sep = "")
	}
	return(ctip)
}
