"guiInfo" <-
function(fname, firstarg = NULL, type = "tip") {
    # Done in NAMESPACE
    #require(svMisc)
    if (type != "tip" && type != "list" && type != "listitems")
		stop("No other type than 'tip', 'list' or 'listitems' currently supported")
	if (type == "tip") {
		# firstarg not used yet... but reserved for methods!
		# TO DO: cases of if, for, ...
		if (exists(fname) && is.function(get(fname))) {
			res <- sub("function ", fname, deparse(args(get(fname))))
			if (length(res) > 1)
				res <- paste(res[-length(res)], collapse = "")
				res <- gsub("    ", "", res)
		} else res <- ""
		if (length(res) == 1 && res == "")
			return(FALSE)
		# If something changed, warn the GUI client he must update its object browser
		Cmd <- paste(c("<<<<Tip", res), collapse = "\n")
		return(guiCmd(Cmd))
	 
	 } else if (type == "list") {
	 	"describe" <- function(name) {
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
			Res <- paste(name, ObjClass, sep = "|")
			return(Res)
		}
	 
		# firstarg is NULL for objects
		# or it is the name of a list if l$ or l[[
		# or it is the name of a S4 object if obj@
		if (is.null(firstarg)) {
			if (fname == "") { # Just list items in .GlobalEnv
				res <- ls(pos = 1)
				if (length(res) == 0)
					return(FALSE)
			} else {
				res <- apropos(paste("^", gsub("\\.", "\\\\.", fname), sep = ""))
				if (length(res) == 0)
					return(FALSE)
			}
			# Get more info about these items
			res <- sapply(res, describe)
			names(res) <- NULL
		} else {	# firstarg not null
			# TO DO...
			
		}
		# If there is a non empty list
		Cmd <- paste(c("<<<<CodeList", res), collapse = "\n")
        return(guiCmd(Cmd))
		
	} else if (type == "listitems") {
		# fname should contain a list or data.frame
		if (!exists(fname))
			return(FALSE)
		obj <- get(fname)
		if (!(inherits(obj, "list") | inherits(obj, "data.frame")))
			return(FALSE)
		res <- paste(names(obj), "subset", sep="|")
		# Eliminate items without names
		res <- res[res != "|subset"]
		if (length(res) == 0)
			return(FALSE)
		Cmd <- paste(c("<<<<CodeList", res), collapse = "\n")
        return(guiCmd(Cmd))
	}
}

