"view.princomp" <-
function(x, type = "print", objname = deparse(substitute(x)), file = guiViewsFile(), CSSFile = guiViewsCSS(), command = "",  browse = TRUE, ...) {

    "viewLoadings" <- function(x, objname, type, file, CSSFile, command, Init = TRUE, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		if (Init) viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTMLli("<a name='loadings'>Loadings:</a> ", file = file)
		HTML(unclass(loadings(x)), file = file)
		return(file)
	}

    "viewScreeplot" <- function(x, objname, type, file, CSSFile, command, Init = TRUE, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		if (Init) viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTMLli("<a name='screeplot'>Screeplot:</a> ", file = file)
		graphfic <- paste(tempfile(), ".jpg", sep = "")
		jpeg(graphfic, width = 500, height = 500)
		plot(x, main = "Scree plot")
		dev.off()
		HTMLInsertGraph(graphfic, Caption = "Scree plot", Width = 500, file = file)
		return(file)
	}

	"viewScores"<- function(x, objname, type, file, CSSFile, command, Init = TRUE, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		if (Init) viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTMLli("<a name='scores'>Scores</a>", file = file)
		HTML(x$scores, file = file)
		return(file)
	}

	"viewBiplot" <- function(x, objname, type, file, CSSFile, command, Init = TRUE, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		if (Init) viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTMLli("<a name='biplot'>Biplot:</a>", file = file)
		graphfic <- paste(tempfile(), ".jpg", sep = "")
		jpeg(graphfic, width = 500, height = 500)
		biplot(x, main = "", pch = 0.7)
		dev.off()
		HTMLInsertGraph(graphfic, Caption="Biplot - Factors (1,2)", Width = 500, file = file)
		return(file)
	}

	"viewAll" <- function(x, objname, type, file, CSSFile, command, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		viewHTMLinit(objname = objname, type = type, file = file, CSSFile = CSSFile, command = command)
		HTML("<p align=right><a href='#call'>Call</a> | <a href='#loadings'>Loadings</a> | <a href='#screeplot'>screeplot</a> | <a href='#scores'>Scores</a> | <a href='#biplot'>Biplot</a>&nbsp;</p>", file = file)
		HTMLhr(file = file)
		HTMLli("<a name='call'>Call: </a>", file = file)
		HTML(x$call, file = file)
		viewLoadings(x, objname, type, file, CSSFile, command, Init = FALSE, ...)
		viewScreeplot(x, objname, type, file, CSSFile, command, Init = FALSE, ...)
		viewScores(x, objname, type, file, CSSFile, command, Init = FALSE, ...)
		viewBiplot(x, objname, type, file, CSSFile, command, Init = FALSE, ...)
	}

    objname <- objname
    # Done in NAMESPACE
    #require(svMisc)
    typelist <- unique(c("summary", "print", "loadings", "screeplot", "scores", "biplot", "all", listCustoms("view", "princomp")))
	if (command == "") command <- guiViewsCmd(type = type, typelist = typelist, command = match.call(), file = file)
   	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
    # Do we have to pass this to a custom function as 'view.princomp..type()'?
	custom <- paste("view.princomp..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, CSSFile = CSSFile, command = command, browse = browse, ...)
	} else {	
		# Process the command in the standard function 
    	x <- eval(xexp, envir = .GlobalEnv)
    	res <- switch(type,
			"typelist"= typelist,
			"loadings"  = viewLoadings(x, objname, type, file, CSSFile, command, ...),
			"screeplot" = viewScreeplot(x, objname, type, file, CSSFile, command, ...),
			"scores"    = viewScores(x, objname, type, file, CSSFile, command, ...),
			"biplot"    = viewBiplot(x, objname, type, file, CSSFile, command, ...),
			"all"    = viewAll(x, objname, type, file, CSSFile, command, ...),
		   	view.default(x = xexp, type = type, file = file, CSSFile = CSSFile, init = FALSE, objname = objname, command = command, browse = browse, ...))
	}
	if (browse == TRUE && type != "typelist") guiViewsDisplay(res)
	if (type == "typelist" || browse == FALSE) return(res) else invisible(res)
}
