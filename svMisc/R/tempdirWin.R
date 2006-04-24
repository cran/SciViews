"tempdirWin" <-
function() {
	if (!isWin()) stop("This function is for Windows only!")
	### TO DO: a platform-independent version!
	
	# Get the Windows temp dir (useful to communicate with other programs)
	# It is assumed to be the parent of the R session temp dir
	return(sub("\\\\Rtmp[0-9]+$", "", tempdir()))
}
