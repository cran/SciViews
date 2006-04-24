"guiViewsCmd" <-
function(type, typelist, command, file, ...) {
	#### TO DO: does not work if file or browser arguments are provided... and also if CSSFile is provided, then it is not changed in views!
	# Build a command line that can be interpreted by compatible viewers to manage tabs in the View window
	command <- paste(deparse(command), collapse = "")
    file <- gsub("[\\]", "/", file)
	# Command could be view.data.frame(....), but due to namespace, only view(....) is recognized => change it!
	command <- sub("view\.[A-Za-z0-9\._]+[(]", "view(", command)
    # Need to replace type = "curtype" by type = "<type>". If type argument not provided, need to add it
	typearg <- paste("type = \"", type, "\"", sep = "")
	# Is it in the command?
	if (length(grep(typearg, command)) > 0) { # Replace it with type = "<type>"
		command <- sub(typearg, "type = \"<type>\"", command)
	} else {	# Add it
		command <- sub("[(]", "(type = \"<type>\", ", command)
	}
	if (length(grep("browse = FALSE", command)) == 0)
		command <- sub("[(]", paste("(file = \"", file, "\", browse = FALSE, ", sep = ""), command)	
    return(paste("<<<<View ", type, "|", paste(typelist, collapse = ","), "|", command, sep = ""))
}
