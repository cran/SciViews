"reportGraph" <-
function(device = dev.cur(), reptype = getOption("report"), application = getOption("reporter"),
bookmark = "<End>", multiformat = FALSE, dir = file.path(tempdir(), "svGraph"),
width = 480, height = 480, pointsize = 12, bg = "transparent", ...) {
	#### TO DO: manage different aspect ratios
	#### TO DO: manage printing only one graph if recording history
    # Currently support only HTML or DOC (WinWord)
    if (is.null(reptype)) reptype <- "html"     # Default value
	if (reptype != "html" && reptype != "doc")
		stop("Only 'html' or 'doc' reptypes are currently supported!")

	if (reptype == "html") {
        if (is.null(application))
			stop("No compatible reporting application supported.\n Use view() and then copy and paste!")
    } else { # reptype = 'doc'
        if (!WordOpen())
            stop("Error while trying to start or activate Microsoft Word!")
    }

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

	if (reptype == "html") {
		# Make the png copy of this graph
        basefile <-  tempfile(pattern = "graph", tmpdir = "c:/temp")
        imgfile <- paste(basefile, ".png", sep = "")
		#imgfile = file.path(dir, "graph.png")
		dev.copy(device = png, file = imgfile, width = width, height = height, pointsize = pointsize, bg = bg)
		dev.off()
		if (multiformat) {
			dev.copy(device = pdf, file = paste(basefile, ".pdf", sep = ""), width = width/96, height = height/96, pointsize = pointsize, bg = bg, ...)
			dev.off()
			#dev.copy(device = win.metafile, file = paste(basefile, ".emf", sep = ""), width = width/96, height = height/96, pointsize = pointsize)
			dev.off()
		}
		# Reactivate previously active graph
		dev.set(currentdevice)
		# Send a command to the report editor, in order to include this graph in the report
    	if (!.Platform$OS == "windows") { # Currently, no reporting app!
			cat("No reporting app yet. Data is in:", imgfile, "\n")
		} else {
			system(paste("\"", application, "\" --addPict=", imgfile, sep = ""), wait = FALSE)
		}
    } else { # reptype = 'doc'
		imgfile <-  paste(tempfile(pattern = "graph", tmpdir = "c:/temp"), ".emf", sep = "")
		#imgfile <- file.path(dir, "graph.emf")
		dev.copy(device = win.metafile, file = imgfile, width = width/96, height = height/96, pointsize = pointsize)
		dev.off()
        # Reactivate previously active graph
		dev.set(currentdevice)
		# Add the graph in WinWord
		if (!is.null(bookmark) && bookmark == "<End>") {
			# Move to the end of the document
	    	WordGotoEnd()
	    	# Add the picture...
	    	WordInsertPictureFile(imgfile, keep.bookmark = FALSE)
		} else {
	    	# Give a bookmark, or allow the user to possibly choose another position
	    	WordGoto(bookmark = bookmark)
      		# Add the picture...
	    	WordInsertPictureFile(imgfile, keep.bookmark = TRUE)
		}
    }
	# Return the name of the png/emf file created
	invisible(imgfile)
}
