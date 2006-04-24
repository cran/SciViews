# Reporting functions for Microsoft Word through DDE

"WordOpen" <-
function(report = NULL, warn = TRUE) {
	# Make sure that Word is running
    # and get a list of available services for WinWord:
	### NOTE: for unknown reasons, path to documents with accents is not well handled yet!
	if (!require(tcltk2, quietly = TRUE)) {
	    if (warn) warning("The 'tcltk2' package is required!")
		return(invisible(FALSE))
	}
	wordDDE <-tk2dde.services("WinWord")
	if (length(wordDDE) == 0) {
		# Determine if Winword is installed on the machine
		# and get the path to Winword.exe
    	Rkey <- "HKEY_LOCAL_MACHINE\\Software\\Classes\\Applications\\WINWORD.EXE\\Shell\\Edit\\Command"
		wordexe <- try(tk2reg.get(Rkey, ""), silent = TRUE)
		if (inherits(wordexe, "try-error")) {
	    	if (warn) warning("Microsoft Word does not seem to be (correctly) installed on this machine!")
			return(invisible(FALSE))
		} else {
	    	# Rework the result to get only the path to Winword.exe
	    	wordexe <- sub("^(\".*\").*$", "\\1", wordexe)
		}
		# Try to start Word now
		wordstarted <- try(system(wordexe, wait = FALSE), silent = TRUE)
		if (inherits(wordstarted, "try-error")) {
	    	if (warn) warning("Microsoft Word does not seem to be (correctly) installed on this machine!")
			return(invisible(FALSE))
		}
		# Wait that Winword is ready, and make sure that a Winword DDE topic is available
		for (i in 1:50) { # Wait up to approx. 10 sec
		    Sys.sleep(0.2)
		    wordDDE <-tk2dde.services("WinWord")
		    if (length(wordDDE) > 0) break
		}
		if (length(wordDDE) == 0) {
	    	if (warn) warning("Impossible to get a communication with Microsoft Word. Please, retry, or restart Word manually.")
			return(invisible(FALSE))
		}
	}
	if (!is.null(report)) {
		# Open or activate the given doc
        if (!file.exists(report)) {
	    	if (warn) warning("The report '", report, "' does not exist!", sep = "")
			return(invisible(FALSE))
		}
		tk2dde.exec("Winword", "System",
			paste("{\[FileOpen .Name=\"", report, "\", .Revert=0\]}", sep =""))
	}
	return(invisible(TRUE))
}

"WordExec" <-
function(command, type = "DDE", async = FALSE) {
	# Execute a command in Winword
	# Currently, only DDE interface is implemented... but OLE will be in the future
	if (type != "DDE") stop("Only DDE communication type is currently supported")
	res <- try(tk2dde.exec("Winword", "System", command,
		async = async), silent = TRUE)
	return(invisible(!inherits(res, "try-error")))
}

"WordActivate" <-
function(async = FALSE) {
	# Activate Microsoft Word
	return(invisible(WordExec("{\[AppActivate \"Microsoft Word\"\]}", async = async)))
}

"WordExit" <-
function() {
	# Exit Microsoft Word, possibly prompting to save opened documents
	return(invisible(WordExec("{\[FileExit\]}", async = FALSE)))
}

"WordGoto" <-
function(bookmark = NULL) {
	if (is.null(bookmark)) {
		# Activate Microsoft Word and display the goto dialog box to select a bookmark
		return(invisible(WordExec("{\[Dim dlg As EditGoto\]\[x = Dialog(dlg)\]}", async = FALSE)))
	} else {
	    command <- paste("{\[EditGoto .Destination=\"", bookmark, "\"\]}", sep = "")
		return(invisible(WordExec(command, async = FALSE)))
	}
}

"WordGotoEnd" <-
function() {
	# Move to the end of the Word document
 	return(invisible(WordExec("{\[EndOfDocument\]}", async = FALSE)))
}

"WordInsertPara" <-
function() {
    # Make a new paragraph at the insertion point in a Word document
    return(invisible(WordExec("{\[InsertPara\]}", async = FALSE)))
}

"WordInsertPageBreak" <-
function() {
    # Start a new page in a Word document
    return(invisible(WordExec("{\[InsertPageBreak\]}", async = FALSE)))
}

"WordInsertText" <-
function(text) {
	### TODO: allow and interpret Wiki code for formatting!
	# Insert a text at the insertion point in a Word document
	# One can use:
	# \t to insert a tabulation
	# \v or \n to insert a line break in the same paragraph
	# (\n is converted into \v)
	# \r to insert a new paragraph
	# \f to insert a page break
	text <- gsub("\n", "\v", text)
	command <- paste("{\[Insert \"", text, "\"\]}", sep = "")
	return(invisible(WordExec(command, async = FALSE)))
}

"WordInsertFile" <-
function(file, keep.bookmark = FALSE) {
    # Insert a file (possibly in a foreign format) in a Word document
    # either at the local insertion point, or in a given bookmark
	# First check if the file exists
	if (!file.exists(file)) {
	    warning("File '", file, "' does not exist!")
	    return(invisible(FALSE))
	}
    if (keep.bookmark) {
		# If I replace all selection inside a bookmark, the bookmark is lost (bug?!)
		# This is a hack: I reduce selection by one char, and then delete this char
		# after inserting the file content
		# This implies that the current selection cannot be empty!
		command <- paste("{\[CharLeft 1, 1\]\[InsertFile .Name=\"", file,
			"\", .ConfirmConversions=0\][\CharRight 1, 1\]\[EditClear\]}", sep = "")
	} else {
	    # This is the stadard function to use in all other cases
     	command <- paste("{\[InsertFile .Name=\"", file,
			"\", .ConfirmConversions=0\]}", sep = "")
	}
	return(invisible(WordExec(command, async = FALSE)))
}

"WordInsertPictureFile" <-
function(file, keep.bookmark = FALSE) {
    # Insert a picture from a file into the current position
    # First check if the file exists
	if (!file.exists(file)) {
	    warning("Picture file '", file, "' does not exist!")
	    return(invisible(FALSE))
	}
	if (keep.bookmark) {
		# If I replace all selection inside a bookmark, the bookmark is lost (bug?!)
		# This is a hack: I reduce selection by one char, and then delete this char
		# after inserting the picture
		# This implies that the current selection cannot be empty!
		command <- paste("{\[CharLeft 1, 1\]\[InsertPicture .Name=\"", file,
			"\"\][\CharRight 1, 1\]\[EditClear\]}", sep = "")
	} else {
	    # This is the stadard function to use in all other cases
	    command <- paste("{\[InsertPicture .Name=\"", file, "\"\]}", sep = "")
	}
	return(invisible(WordExec(command, async = FALSE)))
}
