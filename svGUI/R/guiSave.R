"guiSave" <-
function(..., list = character(0), ascii = FALSE, version = NULL, envir = parent.frame(), compress = TRUE) {
    if (!.Platform$OS == "windows") stop("Do not work yet outside of Windows!")
	# This is essentially a save() operation, but where the user is prompted for a file name
	File <- choose.files(caption = "Save multiples objects under...", filters = Filters[c("RData", "All"),], multi = FALSE)
	if (length(File) == 0) return(invisible(FALSE))	# The user cancelled the operation
	invisible(save(..., list = list, file = File, ascii = ascii, version = version, envir = envir, compress = compress))
}

