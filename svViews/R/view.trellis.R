"view.trellis" <-
function(x,  type = "print", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "", browse = TRUE, ...) {

	"viewPrint" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		print(x)
		HTML("Trellis graph plotted in graphical device / Parameters", file = file)
		HTMLhr(file = file)
		return(file)
	}

	"viewPNG" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		graphfic = paste(tempfile(), ".png", sep = "")
		png(graphfic, width = 700, height = 700)
        # lset from lattice is DEPRECATED!!!
        lset(list(background = list(col = "white")))
		print(x)
		dev.off()
		HTMLInsertGraph(graphfic, Width = "700", file = file)
		return(file)
	}
	### Details of the trellis object: / Could be passed to the GUI client to a Trellis parameter form.
	### cat(paste(capture.output(unclass(x)), collapse = "\n"), file = "")

	objname <- objname
    # Done in NAMESPACE
    #require(svMisc)
    typelist <- unique(c("print", "png", listCustoms("view", "trellis")))
	if (command == "") command <- guiViewsCmd(type = type, typelist = typelist, command = match.call(), file = file)
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'view.trellis..type()'?
	custom <- paste("view.trellis..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, CSSFile = CSSFile, command = command, browse = browse, ...)
	} else {	
		# Process the command in the standard function
		x <- eval(xexp, envir = .GlobalEnv)
		res <- switch(type,
			"typelist"= typelist,
			"png" = viewPNG(x, objname, type, file, CSSFile, command, ...),
			viewPrint(x, objname, type, file, CSSFile, command, ...)) # If "print" or nomatch / Default for trellis
	}
	if (browse == TRUE && type != "typelist") guiViewsDisplay(res)
	if (type == "typelist" || browse == FALSE) return(res) else invisible(res)
}
