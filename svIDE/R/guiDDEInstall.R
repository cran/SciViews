"guiCallTip" <-
function(code, file = NULL, onlyargs = FALSE, maxwidth = 60, location = FALSE) {
    # This is an interface to CallTip for external programs
    # Clear ::SciViewsR_CallTip
    .Tcl("set ::SciViewsR_CallTip {}")

    # Using a callback, all args are strings => convert
    if (length(file) == 0 || file == "" || file == "NULL") file <- NULL
    only.args <- as.logical(onlyargs[1])
    max.width <- as.integer(maxwidth[1])
    
    # Get the call tip
	ctip <- CallTip(code, only.args = only.args, location = location)

    # Possibly break long lines at reasonables widths
    if (only.args) Exdent <- 0 else Exdent <- 4
    if (!is.null(max.width) && !max.width < 1)
	   ctip <- paste(strwrap(ctip, width = max.width, exdent = Exdent), collapse = "\n")

	# Copy the result to a Tcl variable
    .Tcl(paste("set ::SciViewsR_CallTip {", ctip, "}", sep = ""))

    if (!is.null(file)) { # Copy it also to the clipboard or a file
        # if file = clipboard and this is Windows, copy it to the clipboard
        if (file == "clipboard") {
            if (.Platform$OS.type == "windows") {
                writeClipboard(ctip)
            } else {
                stop("'clipboard' not supported yet on platforms different than Windows!")
            }
        } else { # copy the call tip to the file
            cat(ctip, file = file)
        }
    }
	invisible(ctip)
}

"guiComplete" <-
function(code, file = NULL, givetype = FALSE, fieldsep = "|") {
    # This is an interfacte to CallTip for external programs
    # Clear ::SciViewsR_Complete
    .Tcl("set ::SciViewsR_Complete {}")
    
    # Using a callback, all args are strings => convert
    if (length(file) == 0 || file == "" || file == "NULL") file <- NULL
    givetype <- as.logical(givetype[1])
    fieldsep = fieldsep[1]

    # Get the completion list
	clist <- Complete(code, givetype = givetype, fieldsep = fieldsep)

	# Copy the result to a Tcl variable
    .Tcl(paste("set ::SciViewsR_Complete {", clist, "}", sep = ""))

    if (!is.null(file)) { # Copy it also to the clipboard or a file
        # if file = clipboard and this is Windows, copy it to the clipboard
        if (file == "clipboard") {
            if (.Platform$OS.type == "windows") {
                writeClipboard(clist)
            } else {
                stop("'clipboard' not supported yet on platforms different than Windows!")
            }
        } else { # copy the completion list to the file
            cat(clist, file = file)
        }
    }
	invisible(clist)
}

"guiDDEInstall" <-
function() {
    # Register a dde server for R and install callbacks for serveur functions

    # Make sure tcl/tk dde is operational
    if (.Platform$OS.type != "windows") return("DDE not installed: this is not Windows!")
	if (!capabilities("tcltk")) return("DDE not installed: this version of R cannot use Tcl/Tk!")
    if (!require(tcltk)) return("DDE not installed: impossible to load tcltk package!")
	tclRequire("dde", warn = TRUE)	# Should be installed by default with the tcltk package under Windows

    # Register a "SciViewsR" server
    topic <- "SciViewsR"
    # Verify if I am not already registered under this topic
    if (!tclvalue(.Tcl("dde servername {}")) == topic) {
        # Check that this server name does not exist yet
        if (length(grep(paste("[{]TclEval ", topic, "[}]", sep = ""), tclvalue(.Tcl("dde services TclEval {}")), useBytes = TRUE)) > 0)
            invisible("DDE not installed: server name already in use (by another R instance?)!")
        # Register me as a dde server with this topic name
        .Tcl(paste("dde servername", topic))
        # Check that the server is set correctly (if not, return an error)
        if (!tclvalue(.Tcl("dde servername {}")) == topic)
            invisible("DDE not installed: an unknown error occurred while registering the server!")
    }

    # Install callbacks for guiXXXX functions, so that DDE clients can access them
    # guiCallTip()... Take care: this must be adapted if you change guiCallTip()!
    res <- .Tcl.callback(guiCallTip)
    .Tcl(paste("proc guiCallTip {code {file \"\"} {onlyargs FALSE} {maxwidth 60} {location FALSE} } {", gsub("%", "$", res), "}", sep = ""))
    
    # guiComplete()... Take care: this must be adapted if you change guiComplete()!
    res <- .Tcl.callback(guiComplete)
    .Tcl(paste("proc guiComplete {code {file \"\"} {givetype FALSE} {fieldsep |} } {", gsub("%", "$", res), "}", sep = ""))
    
    # Done
    invisible("") # OK!
}
