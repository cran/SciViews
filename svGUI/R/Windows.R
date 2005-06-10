"Windows" <-
function(width = 7, height = 7, pointsize = 12, record = getOption("graphics.record"), 
   	rescale = c("R", "fit", "fixed"), xpinch, ypinch, bg = "transparent", canvas = "white",
    gamma = getOption("gamma"), xpos = NA, ypos = NA, buffered = getOption("windowsBuffered")) {
    # Done in NAMESPACE
    #require(svMisc)
    if (!.Platform$OS == "windows") stop("This function is Windows specific!")
	windows(width = width, height = height, pointsize = pointsize, record = record,
		rescale = rescale, xpinch = xpinch, ypinch = ypinch, bg = bg, canvas = canvas,
		gamma = gamma, xpos = xpos, ypos = ypos, buffered = buffered)
	Cmd <- "<<<<Graph"
	invisible(guiCmd(Cmd))
}

