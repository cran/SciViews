"view.function" <-
function(x, type = "print", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...) {

	"viewMethods" <- function(objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		ll <- methods(objname)
		if (length(ll) > 0) {
			# There are some methods for the function
			# Just list objects class
			ll <- unlist(lapply(strsplit(ll, "\\."), FUN = function(vec) return(paste(vec[-1], collapse = "."))))
			cl <- cbind(classes = ll)
			HTML("Generic function could be applied on following classes<br>", file = file)
			HTML(cl, file = file)
		} else {
			# No method for function
			HTML("Function is not generic / no method for this function.<br>", file = file)
		}
		return(file)
	}

	objname <- objname
    # Done in NAMESPACE
    #require(svMisc)
    typelist <- unique(c("print", "methods", listCustoms("view", "function")))
	if (command == "") command <- guiViewsCmd(type = type, typelist = typelist, command = match.call(), file = file)
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'view.function..type()'?
	custom <- paste("view.function..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, CSSFile = CSSFile, command = command, browse = browse, ...)
	} else {	
		# Process the command in the standard function 
		x <- eval(xexp, envir = .GlobalEnv)
		res <- switch(type,
			"typelist"= typelist,
			"methods" = viewMethods(objname, type, file, CSSFile, command, ...),
		   	view.default(x = xexp, type = type, file = file, CSSFile = CSSFile, objname = objname, command = command, browse = browse, ...))
	}
	if (browse == TRUE  && type != "typelist") guiViewsDisplay(res)
	if (type == "typelist" || browse == FALSE) return(res) else invisible(res)
}
