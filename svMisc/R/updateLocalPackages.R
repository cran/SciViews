"updateLocalPackages" <-
function(basedir = "d:/R", url = paste("file:",
	basedir, "/bin/windows/contrib", sep = ""), ask = TRUE) {
	if (!isWin()) stop("This function is for Windows only!")
	# Update packages from a local source
	update.packages(contriburl = url, ask = ask)
}
