"getEnvironment" <-
function(pos) {
	# Retrieve an environment from a position in search(), or from its name
	# if pos = -1, returns the parent environment of the calling function
	# if the environment is "package:base", return NULL
	envir <- if (pos == -1) parent.frame(2) else as.environment(pos)
	return(envir)
}
