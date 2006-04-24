"Complete" <-
function(code, givetype = FALSE, fieldsep = "|") {
	# Get a completion list, given a part of the code

    # This code gets the (partial) name of a variable at the end of a string
    getName <- function(code) {
        if (length(grep("[a-zA-Z0-9_\\.]$", code)) == 0) return("")
        pos <- regexpr("[a-zA-Z0-9_\\.]+$", code, useBytes = TRUE)
	    name <- substring(code, pos)
	    return(name)
    }
    # get the type of object
    getType <- function(name, fieldsep = "|") {
		Obj <- get(name)
		ObjClass <- class(Obj)[1]
		if (ObjClass == "function") {
			if (class(getS3method(name, "default", optional = TRUE)) == "function")
				ObjClass <- "method"
		} else if (is.vector(Obj)) {
			ObjClass <- "vector"
		} else if (!(ObjClass %in%  c("data.frame", "matrix", "array", "table", "list"))) {
			ObjClass <- "other"
		}
		res <- paste(name, ObjClass, sep = fieldsep)
		return(res)
	}

    # Determine what is asked according to last chars in code
    name <- NULL

    # 1) If code ends by '$' or '[[', then try to subset a list or data.frame
    if (length(grep("\\$$", code)) != 0)
        name <- getName(sub("\\$$", "", code))
    if (length(grep("\\[\\[$", code)) != 0)
        name <- getName(sub("\\[\\[$", "", code))
    if (!is.null(name)) {
        if (!exists(name, where = 1)) return("")
		obj <- get(name, pos = 1)
		if (!(inherits(obj, "list") || inherits(obj, "data.frame"))) return("")
        # Get names of this list
        res <- names(obj)
        # Eliminate items without names
		res <- res[res != ""]
        if (length(res) == 0) return("")
        # possibly append the type "subset"
		if (givetype) res <- paste(res, "subset", sep = fieldsep)
		return(paste(res, collapse = "\n"))
    }
    
    # 2) If code ends by '@', then try to get slots of an S4 object
    if (length(grep("@$", code)) != 0)
        name <- getName(sub("@$", "", code))
    if (!is.null(name)) {
        if (!exists(name, where = 1)) return("")
		obj <- get(name, pos = 1)
        slots <- getSlots(class(obj))
        if (length(slots) == 0) return("")
        slotnames <- names(slots)
        if (givetype) {
            return(paste(slotnames, slots, sep = fieldsep, collapse = "\n"))
        } else {
            return(paste(slotnames, collapse = "\n"))
        }
    }
    
    # 3) If code ends by A-Za-z0-9._, we must be writing a keyword => look for a list of matching keywords
    #    else, just return the list of variables in .GlobalEnv
    name <- getName(code)
	if (name == "") { # Just list items in .GlobalEnv
		res <- ls(pos = 1)
		if (length(res) == 0) return("")
	} else {
		res <- apropos(paste("^", gsub("\\.", "\\\\.", name), sep = ""))
		if (length(res) == 0) return("")
	}
	# Get more info about these items
	if (givetype) res <- sapply(res, getType, fieldsep = fieldsep)
	names(res) <- NULL
    return(paste(res, collapse = "\n"))
}
