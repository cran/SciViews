"guiViewsDisplay" <-
function(viewfile, viewer = getOption("viewer"), ...) {
	# If viewer is null, use "internal" which is simply to display it in the internet browser
	if (is.null(viewer)) viewer <- "internal"

	"ViewInternal" <- function(viewfile) {
		browseURL(viewfile)
		return(TRUE)
	}
	
	"ViewRConsole" <- function(viewfile){
		# Warn the SciViews GUI client that the view details are ready by returning a command
		return(guiCmd(paste("«View", viewfile)))
	}
	
	"ViewOther" <- function(viewfile, viewer, ...) {
		# Try to run "viewer" with "viewfile" as argument
		res <- try(system(paste("\"", viewer, "\" \"", viewfile, "\"", sep = ""), wait = FALSE))
		if (inherits(res, "try-error")) return(FALSE) else return(TRUE)
	}
	
	res <- switch(viewer,
		"internal"= ViewInternal(viewfile),
		"RConsole"= ViewRConsole(viewfile),
		ViewOther(viewer = viewer, viewfile = viewfile, ...))
	invisible(res)
}
