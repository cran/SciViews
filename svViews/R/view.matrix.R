"view.matrix" <-
function(x, type = "summary", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "",  browse = TRUE, ...) {

	"viewPrint" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		objdim <- dim(x)
		# For data.frame / matrices, if object is too big, subset it according to option view.maxsize
		maxsize <- getOption("view.maxsize")
		if (is.null(maxsize) || !is.numeric(maxsize) || length(maxsize) < 2)	# Wrong value for view.maxsize => use default one
			maxsize <- c(50, 15)
		maxsize[maxsize < 1] <- 1
		maxsize <- as.integer(maxsize) # make sure it is postive integers
		if (any(objdim > maxsize[1:2])) {
			HTML(x[1:min(maxsize[1], objdim[1]), 1:min(maxsize[2], objdim[2])], file = file)
			HTML(paste("<i>Some rows/columns have been removed from preview / Object dimensions:", paste(objdim, collapse = "x"), "</i>", sep = ""), file = file)
		} else {
			HTML(x, file = file)
		}
		return(file)
	}

	"viewMissing" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		nmissing <- sum(is.na(x))
		pmissing <- round(100 * nmissing / length(unlist(x)))
		HTML(paste("Total number of missing values: ", nmissing, " (", pmissing, "%)", sep = ""), file = file)
		if (nmissing > 0) {
			# Some missing values / See in details
			names <- dimnames(x)[[2]]
			for (i in 1:dim(x)[2]) {
				HTML(paste("<li><b>", names[i], "</b><br>", sep = ""), file = file)
				xi <- x[, i]
				nmissing <- sum(is.na(xi))
				pmissing <- round(100 * nmissing / length(xi))
				HTML(paste("&nbsp;&nbsp;&nbsp;Missing values: ", nmissing, " (", pmissing, "%)", sep = ""), file = file)
			}
		}
		return(file)
	}

	objname <- objname
    # Done in NAMESPACE
    #require(svMisc)
    typelist <- unique(c("summary", "print", "missing", listCustoms("view", "matrix")))
	if (command == "") command <- guiViewsCmd(type = type, typelist = typelist, command = match.call(), file = file)
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'view.matrix..type()'?
	custom <- paste("view.matrix..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, CSSFile = CSSFile, command = command, browse = browse, ...)
	} else {	
		# Process the command in the standard function
		x <- eval(xexp, envir = .GlobalEnv)
		res <- switch(type,
			"typelist"= typelist,
			"print" = viewPrint(x, objname, type, file, CSSFile, command, ...),
			"missing" = viewMissing(x, objname, type, file, CSSFile, command, ...),
			view.default(x = xexp, type = type, file = file, CSSFile = CSSFile, init = FALSE, objname = objname, command = command, browse = browse, ...))
	}
	if (browse == TRUE && type != "typelist") guiViewsDisplay(res)
	if (type == "typelist" || browse == FALSE) return(res) else invisible(res)
}
