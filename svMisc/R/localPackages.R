"localPackages" <-
function(basedir = "d:/R", url = paste("file:", basedir,
	"/bin/windows/contrib", sep = ""), ask.update = TRUE) {
	# Install and/or update packages from a local source, like a CD-ROM
	# This code is inspired from install.packages
	if (.Platform$OS.type != "windows")
		stop("This function is for Windows only! Use install.packages instead.")
	localurl <- (length(grep("^file:", url)) > 0)
	if (!localurl)
        stop("localPackages can only install local packages. Use install.packages to install accross the internet")
    
    # Are there new packages on the local source?
    cat("Checking for new packages...\n")
    flush.console()
	available <- CRAN.packages(contriburl = url)
	inst <- installed.packages()
	# Which packages are not installed yet?
	not.inst <- available[is.na(match(available[, 1], inst[, "Bundle"])), ]
	not.inst <- not.inst[is.na(match(not.inst[, 1], inst[, 1])), ]
	if (NROW(not.inst) > 0) {		# Some packages were not installed yet
		pkgs <- select.list(not.inst[, 1], , TRUE)
		if (length(pkgs) > 0 && pkgs[1] != "") {
            install.packages(pkgs, contriburl = url)
	    }
	
	} else {	# All packages are installed
		cat("all available packages are already installed\n")
	}
	
	if (ask.update) {	# Check for updated packages...
		cat("\nChecking for updated packages...\n")
		flush.console()
		old <- old.packages(CRAN = baseurl, available = available)
		if (NROW(old) > 0) {	# Not all packages are up to date
			update.packages(CRAN = baseurl)
		} else {
			cat("all installed packages are up to date\n")
		}
	}
}
