"guiViewsCSSChange" <-
function(newCSS = "") {
	# if newCSS is "", then use default CSS file, that is, the one from R2HTML
	if (newCSS == "") {
        # Done in NAMESPACE
        #require(svMisc)
        #if (!Require(R2HTML))
		#	stop("Package R2HTML is required!")
		# Get the path to R2HTML
		packagepath <- .find.package(package = "R2HTML")
		newCSS <- file.path(packagepath, "output", "R2HTML.css")
	}
	if (file.exists(newCSS)) {
		file.copy(newCSS, guiViewsCSS(), overwrite = TRUE)
		return(TRUE)
	} else return(FALSE)
	
}
