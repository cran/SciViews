"CallTip" <-
function(code) {
	# Get a call tip, given a part of the code
	# Extract the last variable name, given it is either at the end, or terminated by '('
	code <- sub(" *\\($", "", code[1])
	pos <- regexpr("[a-zA-Z0-9_\\.]+$", code)
	code <- substring(code, pos)

	# Get the corresponding Call Tip
	ctip <- "" # Default value, in case the function does not exist
	if (code != "" && exists(code, where = 1, mode = "function")) ctip <- Args(code)
	#### TO DO: return this value to the calling app!
	return(ctip)
}
