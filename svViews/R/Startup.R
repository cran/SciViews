".onLoad" <-
function(lib, pkg) {
	# Install dir and files needed for views
	ViewsRoot <- guiViewsDir()
	if (!file.exists(ViewsRoot) || !file.info(ViewsRoot)$isdir) {
		unlink(ViewsRoot)       # Make sure to destroy it if it is a file!
		if (!dir.create(ViewsRoot))
			stop("Impossible to create the Views root directory!")
	}
	if (!guiViewsCSSChange())
		stop("Impossible to install the default CSS file needed for views!")
}
