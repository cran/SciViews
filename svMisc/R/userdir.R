"userdir" <-
function() {
	# Return the user directory ("My Documents" under Windows)
	return(Sys.getenv("R_User"))
}
