"installLocalPackages" <-
function(basedir = "d:/R", url = paste("file:",
	basedir, "/bin/windows/contrib", sep = "")) {
	# Install one or several packages from a local source
	if (.Platform$OS.type != "windows")
		stop("This function is for Windows only! Use install.packages instead.")
	localurl <- (length(grep("^file:", url)) > 0)
	if (!localurl)
        stop("installLocalPackages can only install local packages. Use install.packages to install accross the internet")
	a <- CRAN.packages(contriburl = url)
	pkgs <- select.list(a[, 1], , TRUE)
	if (length(pkgs) == 0 || pkgs[1] == "")	# The user cancelled the list
		return(invisible(NULL))
	lib <- .libPaths()[1]
    # Install those selected packages
    install.packages(pkgs, lib, CRAN = NULL)
	invisible()
}
