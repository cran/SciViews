"export.default" <-
function(x, type = "raw", file, append = FALSE, objname = deparse(substitute(x)), ...) {

	"exportHTML" <- function(x, file, append, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        #Require(R2HTML)
		if (file != "") tmpfile <- file(file, c("w", "a")[append + 1]) else tmpfile <- file
		HTML(x, file = tmpfile, ...)
		if (file != "") close(tmpfile)
		invisible(return(TRUE))
	}

	"exportLaTeX" <- function(x, file, append, ...) {
        # Done in NAMESPACE
        #require(svMisc)
        if (!(Require(Hmisc)))
			stop("¨Package HMisc is required for LaTeX exportation!")
		if (file != "") tmpfile <- file(file, c("w", "a")[append+1]) else tmpfile <- file
		latex(x, file = tmpfile, ...)
		if (file != "") close(tmpfile)
		invisible(return(TRUE))
	}

	"exportASCII" <- function(x, file, append, ...) {
		treated <- FALSE
		if (is.vector(x)) {
			txt <- paste(x, collapse = "\t")
			treated <- TRUE
		}
		if (is.matrix(x)) {
			txt <- as.character(x)
			txt <- paste(apply(x, 1, FUN = paste, collapse = "\t"), collapse = "\n")
			treated <- TRUE
		}
		if (!treated) {
			tmpfile <- tempfile()
			sink(tmpfile)
			print(x)
			sink()
			txt <- readLines(tmpfile)
			txt <- paste(txt, collapse = "\n")
		}

		if (file != "") tmpfile <- file(file, c("w", "a")[append+1]) else tmpfile <- file
		cat(txt, file = tmpfile, ...)
		if (file != "") close(tmpfile)
		invisible(return(TRUE))
	}

	"exportRaw" <- function(x, file, ...) {
		if (file != "") tmpfile <- file(file, c("w", "a")[append+1]) else tmpfile <- file
		dput(x, file = tmpfile)
		if (file != "") close(tmpfile)
		invisible(return(TRUE))
	}

	if (is.null(file) || file == "clipboard") append = FALSE # Make sure we do not append to the clipboard!
	objname <- objname
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text = paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'export.default..type()'?
	custom <- paste("export.default..", type, sep = "")
    # Done in NAMESPACE
    #require(svMisc)
    if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, append = append, ...)
	} else {	
		# Process the command in the standard function 
		x <- eval(xexp, envir = .GlobalEnv)
		res <- switch(type,
			"typelist"= unique(c("raw", "ascii", "html", "latex", listCustoms("export", "default"))),
			"html"    = exportHTML(x, file, append, ...),
			"latex"   = exportLaTeX(x, file, append, ...),
			"ascii"   = exportASCII(x, file, append, ...),
			exportRaw(x, file, append, ...))
	}
	if (type == "typelist") return(res) else invisible(res)
}
