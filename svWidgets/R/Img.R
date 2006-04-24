"ImgGet" <-
function(image) {
	# Get the image
    return(getTemp(".gui.Imgs")[[image]])
}

"ImgType" <-
function(image, warn = TRUE) {
	# Get the type of image
	if (regexpr("^[$]Tk[.]", image) > 0) return("tkImage") else {
        if (warn) warning("Unrecognized image type for ", image)
		return(NA)
	}
}

"ImgNames" <-
function() {
	# List all available images
    return(names(getTemp(".gui.Imgs")))
}

"tkImgAdd" <-
function(file, type = "gif") {
	# Add a Tk image to the list (just GIF for the moment, but Img package allows for more!)
	if (type != "gif")
		stop("Only 'gif' images currently supported!")
	if (!file.exists(file))
		stop("File ", file, " not found!")
	require(tcltk)
	require(svMisc)
	# Load the image and assign it to an item to the .gui.Imgs object in TempEnv
	.gui.Imgs <- getTemp(".gui.Imgs")
	if (is.null(.gui.Imgs)) .gui.Imgs <- list()
	# Calculate image name as being the basename of the file without extension
	Iname <- sub("[.][^.]+$", "", basename(file))
	# In order to indicate it is a Tk resource, prepend '$Tk.'
	Iname <- paste("$Tk.", Iname, sep = "")
	# If that name already exists, do nothing
	### TODO: may be, update the image? Argument update = TRUE
	if (Iname %in% names(.gui.Imgs)) return(invisible())
	Image <- tclVar()
	tkcmd("image", "create", "photo", Image, file = file)
	.gui.Imgs[[Iname]] <- Image
	# Reassign .gui.Imgs to TempEnv
	assignTemp(".gui.Imgs", .gui.Imgs)
	return(invisible(Image))
}

"tkImgDel" <-
function(image) {
	# Delete an image ressource from the list
	.gui.Imgs <- getTemp(".gui.Imgs")
	# Is the image there?
	if (!image %in% names(.gui.Imgs)) return(invisible())
	# Delete the image
	Image <- .gui.Imgs[[image]]
	tkcmd("image", "delete", Image)
	# Eliminate it from the list in .gui.Imgs
	.gui.Imgs[[image]] <- NULL
	# Reassign .gui.Imgs to TempEnv
	assignTemp(".gui.Imgs", .gui.Imgs)
}

"tkImgReadPackage" <-
function(package, subdir = "gui", type = "gif") {
	# Read all gif images from a subdirectory of a package to Tk images resources
	dir <- file.path(.path.package(package = package)[1], subdir)
	# Check that the dir exists
	if (!file.exists(dir) || !file.info(dir)$isdir)
		stop("'", dir, "' does not exist, or is not a directory!")
	# List all file of 'type' in that directory
	if (type != "gif")
		stop("only type = 'gif' is currently supported")
	pattern <- "[.][gG][iI][fF]$"
	files <- list.files(dir, pattern = pattern, full.names = TRUE)
	if (length(files) == 0) return(invisible())
	for (i in 1:length(files)) tkImgAdd(files[i], type = type)
}
