"view.default" <-
function(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...) {
	
	"viewPrint" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTML(x, file = file)
		return(file)
	}

	"viewSummary" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		if (is.function(x)) {
			argums <- sub("function", objname, deparse(args(x)))
			HTML(paste(argums[1:(length(argums) - 1)], "\n"), file = file)
		} else {
			HTML(summary(x), file = file)
		}
		return(file)
	}

	objname <- objname
    # Done in NAMESPACE
    #require(svMisc)
    typelist <- unique(c("summary", "print", listCustoms("view", "default")))
	if (command == "") command <- guiViewsCmd(type = type, typelist = typelist, command = match.call(), file = file)
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'view.default..type()'?
	custom <- paste("view.default..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, CSSFile = CSSFile, command = command, browse = browse, ...)
	} else {	
		# Process the command in the standard function 
		x <- eval(xexp, envir = .GlobalEnv)
		res <- switch(type,
			"typelist"= typelist,
			"summary"= viewSummary(x, objname, type, file, CSSFile, command, ...),
			"help"   = viewHelp(objname, type, file, CSSFile, command, ...),
			viewPrint(x, objname, type, file, CSSFile, command, ...)) # If nomatch
		# PhG: with S4 objects, it is the 'show' method that is used by default, not print! => how to include it (could possibly return a graph!)
	}
	if (browse == TRUE && type != "typelist") guiViewsDisplay(res)
	if (type == "typelist" || browse == FALSE) return(res) else invisible(res)
}
