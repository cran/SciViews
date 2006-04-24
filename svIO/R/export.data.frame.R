"export.data.frame" <-
function(x, type = "raw", file = NULL, append = FALSE, objname = deparse(substitute(x)), saslib = "work", sasmember = "", writeit = TRUE, ...) {

	"exportASCII" <- function(x, file, append, ...) {
		col1 <- dimnames(x)[[1]]
		for (i in 1:ncol(x)) col1 <- paste(col1, x[, i], sep = "\t")
		col1 <- paste(col1, collapse = "\n")
		row1 <- c("", dimnames(x)[[2]])
		txt <- paste(row1, collapse = "\t")
		txt <- paste(txt, col1, sep = "\n")
		# write code to file connection
		if (file != "") tmpfile <- file(file, c("w","a")[append+1]) else tmpfile <- file
		cat(txt, file = tmpfile, ...)
		if (file != "") close(tmpfile)
		invisible(return(txt))
	}

	"exportSAS" <- function(x, file, append, objname, saslib = "work", sasmember = "",writeit=TRUE, ...) {
		if (sasmember == "") {
			objname <- unlist(strsplit(objname, NULL))
			objname[objname == " "] <- "_"
			objname <- objname[objname %in% c(letters, LETTERS, 0:9, "_")]
			objname <- paste(objname, collapse = "", sep = "")
		}
		else objname <- sasmember
		.SASdatasetname <- objname
		line <- paste("DATA ", saslib, ".", objname, ";", sep = "")
		tmp <- dimnames(x)[[2]]
		# Special treatment for data.frame
		# Remove special characters in variable names
		tmp <- sapply(tmp, FUN = function(x) strsplit(x, NULL))
		tmp <- sapply(tmp, FUN = function(vec) return(vec[vec %in% c(letters, LETTERS, 0:9, "_")]), simplify = FALSE, USE.NAMES = FALSE)
		tmp <- sapply(tmp, FUN = function(vec){
		    if (!vec[1] %in% c(letters, LETTERS)) vec = c("V", vec[2:length(vec)]); return(vec)},
		    simplify = FALSE, USE.NAMES = FALSE)
		SAScolnames <- sapply(tmp, paste, collapse = "")
		line <- c(line, "\t INFILE cards missover;")
		line <- c(line, paste("\t INPUT", paste(SAScolnames, collapse = " "), ";", sep = " "))
		line <- c(line, "\t CARDS;")

		# PROC FORMAT
		.tmpSVline2 <- c("PROC FORMAT;")
		.tmpSVboolformat <- FALSE # Indicates whether format has to be created or not

		# PROC DATASETS
		.tmpSVline3 <- c(paste("PROC DATASETS lib=", saslib , " nolist;", sep = "", collapse = ""))
		.tmpSVline3 <- c(.tmpSVline3, paste("\t MODIFY ", objname, ";", sep = ""))

		# data
		processcolumn <- function(x, varname, i) {
			if (!class(x) %in% c("integer", "numeric")){
				x <- as.factor(as.character(x))
				.tmpSVboolformat <- TRUE
				SASlabels <- levels(x)
				SAScodes <- 1:length(SASlabels)
				tmpcodes <- paste(SAScodes, "=\"", SASlabels, "\"", collapse = "\n", sep="")
				.tmpSVline2 <- c(.tmpSVline2, paste("VALUE V", i, "f", sep = ""))
				.tmpSVline2 <- c(.tmpSVline2, paste(tmpcodes, ";", sep = "\n"))
				.tmpSVline3 <- c(.tmpSVline3, paste("\t FORMAT ", varname, " V", i, "f.;", sep = ""))
				x <- as.numeric(x)
			}
			x <- as.character(x)
			x[is.na(x)] <- "."
			return(x)
		}
		col1 <- paste("\t", processcolumn(x[, 1], SAScolnames[1], 1))
		if (dim(x)[2] > 1) {
			for (i in 2:dim(x)[2]) col1 <- paste(col1, processcolumn(x[, i], SAScolnames[i], i), sep = "\t")
			col1 <- paste(col1, collapse = "\n")
		}
		# DATASTEP
		line <- c(line, col1)
		line <- c(line, ";")
		line <- c(line, "RUN;\n")

		if (.tmpSVboolformat){
			# PROC FORMAT
			.tmpSVline2 <- c(.tmpSVline2, "RUN;")
			# PROC DATASETS
			.tmpSVline3 <- c(.tmpSVline3, "RUN;\nQUIT;")
			line <- c(line, "\n", .tmpSVline2, "\n", .tmpSVline3)
		}
		rm(list = c(".tmpSVline2", ".tmpSVline3", ".tmpSVboolformat"), pos = 1)

		# write code to file connection
		if (writeit==TRUE){
			line <- paste(line, collapse = "\n")
			if (file != "") tmpfile <- file(file, c("w", "a")[append + 1]) else tmpfile <- file
			cat(line, file = tmpfile, append = append, ...)
			if (file != "") close(tmpfile)
		}
		invisible(return(line))
	}

	if (is.null(file) || file == "clipboard") append = FALSE # Make sure we do not append to the clipboard!
	objname <- objname
	# Compute the expression
	xexp <- try(if (inherits(x, "expression")) x else NULL, silent = TRUE)
	if (inherits(xexp, "try-error") || is.null(xexp)) {
		xexp <- substitute(x)
		if (is.character(xexp)) # To make sure that non conventional names will be correctly evaluated, we use backticks!
			xexp <- parse(text=paste("`", xexp, "`", sep = ""))
		xexp <- as.expression(xexp)
	}
	# Do we have to pass this to a custom function as 'export.data.frame..type()'?
	custom <- paste("export.data.frame..", type, sep = "")
	if (exists(custom, mode = "function")) {
		res <- get(custom, mode = "function")(expr = xexp, objname = objname, file = file, append = append, ...)
	} else {	
		# Process the command in the standard function 
		x <- eval(xexp, envir = .GlobalEnv)
        # Done in NAMESPÄCE
        #require(svMisc)
        res <- switch(type,
			"typelist"= unique(c("raw", "ascii", "html", "latex", "sascode", listCustoms("export", "data.frame"))),
			"ascii"   = exportASCII(x, file, append, ...),
			"sascode" = exportSAS(x, file, append, objname, saslib, sasmember, writeit, ...),
			export.default(x = xexp, type = type, file = file, append = append, ...))
	}
	if (type == "typelist") return(res) else invisible(res)
}
