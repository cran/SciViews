"guiInstall" <-
function(HideTkWindow = TRUE){
    # Done in NAMESPACE
    #require(svMisc)
    #Require(svIO)
    #Require(svViews)
    if (!capabilities("tcltk"))
		stop("Tcl/Tk is required but is not supported on this system!")
    if (!(as.numeric(R.Version()$major) >= 2 || as.numeric(R.Version()$minor) >= 1))
		warning("This feature was not tested on a lower version of R than 2.1.0. Please, upgrade R if you have problems.")

	# Here the code is different, depending on the platform
	if (!.Platform$OS == "windows") { # non-Windows
	    ### TO DO...
	    # TO DO Create .guiCmd
#	    # Register a TaskCallback to generate automatically informations for an object browser
#		# Use  getTaskCallbackNames() to know if some tasks are registered
#		assignTemp(".guiObjCallback", function(...) {
#	        ob <- names(getTemp(".guiObjListCache", default = NULL))
#	        if (!is.null(ob)) for (i in 1:length(ob)) guiObjBrowse(id = ob[i])
#		    return(TRUE)   # Required to keep it in the TaskCallback list
#	    })
#	    Callback <- getTemp(".guiObjCallbackId", default = NULL)
#	    if (is.null(Callback)) {
#	        n <- addTaskCallback(getTemp(".guiObjCallback"))
#	        if (!is.null(n)) assignTemp(".guiObjCallbackId", as.character(n))
#	    }

#	    # Indicate that this uses socks communication
#		options(SciViews.socks = TRUE)
	    
	} else { # This is Windows
		if (!.Platform$GUI[1] == "Rgui")
		    stop("Sorry, this GUI works only from within Rgui!")
		if (!is.null(getTemp(".guiLinkTk")))
			stop("Interface already operational. Use guiUninstall() first!")
		if (!isSDI())
		   warning("It seems Rgui is not in SDI mode. This inhibits some GUI programs like RConsole.exe to work. Go to Edit -> GUI preferences, check SDI, save the file in the proposed location and restart R to switch in SDI mode.")

		# OK, we can proceed!
	    # Done in NAMESPACE
	    #if (!Require(tcltk)) stop("library tcltk is required!")
	    # Define various commands
		assignTemp(".guiLinkFile", paste(tempdir(), "svLink.R", sep = .Platform$file.sep))
		assignTemp(".guiLinkOutFile", paste(tempdir(), "svLink.txt", sep = .Platform$file.sep))
		assignTemp(".guiCat", function(...) {
	        assignTemp(".guiLinkOutFile", paste(tempdir(), "svLink.txt", sep = .Platform$file.sep), replace.existing = FALSE)
		    cat(... , file = getTemp(".guiLinkOutFile"), append = TRUE)
		})
		assignTemp(".guiWrite", function(x) {
	        assignTemp(".guiLinkOutFile", paste(tempdir(), "svLink.txt", sep = .Platform$file.sep), replace.existing = FALSE)
	        write.table(x, file = getTemp(".guiLinkOutFile"), quote = FALSE, sep = "\t", row.names = FALSE)
		})
		assignTemp(".guiCmd", function(..., CmdFile = NULL) {
			if (is.null(CmdFile) || !is.character(CmdFile)) CmdFile <- paste(tempfile("svLink"), ".txt", sep = "") else CmdFile <- CmdFile[1]
			try(tktitle(getTemp(".guiLinkTk")) <- "CALC!", silent = TRUE)
			cat(..., file = CmdFile)
			try(tktitle(getTemp(".guiLinkTk")) <- getTemp(".guiLinkFile"), silent = TRUE)
		})
		# Create the window
		assignTemp(".guiLinkTk", tktoplevel())
	    .guiLinkTk <- getTemp(".guiLinkTk")
	    if (HideTkWindow) tkwm.withdraw(.guiLinkTk)
		tktitle(.guiLinkTk) <- getTemp(".guiLinkFile")
		topMenu <- tkmenu(.guiLinkTk)
		tkconfigure(.guiLinkTk, menu = topMenu)
		fileMenu <- tkmenu(topMenu, tearoff = FALSE)
		evalcmd <- function() {
	        .guiLinkFile <- getTemp(".guiLinkFile")
			if (file.exists(.guiLinkFile)) {
				file.remove(getTemp(".guiLinkOutFile"))	# Make sure to create a new output file
				tktitle(.guiLinkTk) <- "CALC!"	# This indicates I am processing a command
	            e <- try(source(.guiLinkFile))
				tktitle(.guiLinkTk) <- .guiLinkFile
				file.remove(.guiLinkFile)
			} else tktitle(.guiLinkTk) <- .guiLinkFile
		}
		tkadd(topMenu, "command", label = "Run", command = evalcmd)

		# Register a TaskCallback to generate automatically informations for an object browser
		# Use  getTaskCallbackNames() to know if some tasks are registered
		assignTemp(".guiObjCallback", function(...) {
	        ob <- names(getTemp(".guiObjListCache", default = NULL))
	        if (!is.null(ob)) for (i in 1:length(ob)) guiObjBrowse(id = ob[i])
		    return(TRUE)   # Required to keep it in the TaskCallback list
	    })
	    Callback <- getTemp(".guiObjCallbackId", default = NULL)
	    if (is.null(Callback)) {
	        n <- addTaskCallback(getTemp(".guiObjCallback"))
	        if (!is.null(n)) assignTemp(".guiObjCallbackId", as.character(n))
	    }

	    # Indicate that this uses TclTk communication
		options(SciViews.TclTk = TRUE)
	}

	# Done!
	invisible(TRUE)
}

