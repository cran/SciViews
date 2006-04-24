"viewHTMLinit" <-
function(objname, type, file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", view.title = getOption("view.title")) {
	# Initialisation of HTML file
	HTMLfilecomplete <- rev(strsplit(file, "\\\\")[[1]])[1]
	tmp <- strsplit(HTMLfilecomplete, "\\.")[[1]]
	HTMLfilename <- tmp[1]
	HTMLfileextension <- tmp[2]
	cat(paste("<html>\n<head>\n\t<title>R View: ", objname, " [", type, "]</title>\n", sep = ""), file = file, append = FALSE)
	#### TO DO: use basename(CSSFile) only if it is on the same dir as file!!!
	cat(paste("\t<link rel=stylesheet href='", basename(CSSFile), "' type=text/css>", sep = ""), file = file, append = TRUE)
	# Add the script that returns view tabs information if the Viewer support them
	cat(paste("\n<script language=\"javascript\">\n", sep = ""), file = file, append = TRUE)
	# Info is passed through the status message as:
	# «View type|coma-separated list of views|command to change view, i.e., view(expr, type = "<type>")
	cat(paste("\twindow.status = '", command, "'\n", sep = ""), file = file, append = TRUE)
	# Once done, restore the default status message
	cat(paste("\twindow.status = \"\"\n", sep = ""), file = file, append = TRUE)
	cat(paste("</script>\n<body>\n", sep = ""), file = file, append = TRUE)
	# Title in the page
	if (is.null(view.title) || view.title == TRUE) {
		HTML(as.title(paste("&nbsp;View of <b>", objname, "</b> &nbsp; [", type ,"]", sep = "")), file = file)
		cat("\n", file = file, append = TRUE)
	}
	invisible(file)
}
