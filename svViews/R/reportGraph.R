"reportGraph" <-
function(device = dev.cur(), application = getOption("reporter"), multiformat = FALSE, dir = file.path(tempdir(), "svGraph"), width = 480, height = 480, pointsize = 12, bg = "transparent", ...) {
	#### TO DO: use random names for graphs (like graph36467.png)
	#### TO DO: manage different aspect ratios
	#### TO DO: manage printing only one graph if recording history
	if (is.null(application))
		stop("No compatible reporting application supported.\n Use view() and then copy and paste!")
	# Make sure dir exists and is empty
	if (!file.exists(dir)) { 
		dir.create(dir)
	} else { # eliminate all files in this dir
		filelist <- list.files(dir)
		if (length(filelist) > 0) unlink(file.path(dir, filelist))
	}
	# Save current active device and activate device
	currentdevice <- dev.cur()
	dev.set(device)
	# Make the png copy of this graph
	pngfile = file.path(dir, "graph.png")
	dev.copy(device = png, file = pngfile, width = width, height = height, pointsize = pointsize, bg = bg)
	dev.off()
	if (multiformat) {
		dev.copy(device = pdf, file = file.path(dir, "graph.pdf"), width = width/96, height = height/96, pointsize = pointsize, bg = bg, ...)
		dev.off()
		dev.copy(device = win.metafile, file = file.path(dir, "graph.emf"), width = width/96, height = height/96, pointsize = pointsize)
		dev.off()
	}
	# Reactivate previously active graph
	dev.set(currentdevice)
	# Send a command to the report editor, in order to include this graph in the report
    if (!.Platform$OS == "windows") { # Currently, no reproting app!
		cat("No reporting app yet. Data is in:", file, "\n")
	} else {
		system(paste("\"", application, "\" --addPict=", pngfile, sep = ""), wait = FALSE)
	}
	# Return the name of the png file created
	invisible(pngfile)
}
