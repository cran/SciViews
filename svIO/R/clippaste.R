"clippaste" <-
function(name = "newobj", type = "ascii", objclass = "data.frame", pos = 1, ...) {
	# There is no readClipboard function outside of Windows... make one!
	### Rem: this is not tested yet. It follows a suggestion by Ted Harding
	if (!.Platform$OS == "windows") {
	    "readClipboard" <- function() {
			File <- tempfile()
			# Paste the content of the clipboard into File
			system(paste("wxpaste >", File), wait = TRUE, invisible = TRUE)  # TO DO: manage errors!
			# Read File and destroy it
			Data <- readLines(File)
			unlink(File)
			# Return results
			return(Data)
	    }
	}

	"clippasteASCII" <- function(name, objclass, pos, ...) {
		# Read clipboard content to create a matrix, a data.frame or a function in R
		# If function, content is sourced
		if (objclass == "function") {
			source(textConnection(readClipboard()))$value
			invisible(return(1))
		}
		tmp <- read.table(textConnection(readClipboard()))
		if (objclass == "matrix") tmp <- as.matrix(tmp)
		expr <- paste("assign('", name, "', tmp, pos=", pos, ")", sep="")
		try(eval(parse(text=expr)))
		invisible(TRUE)
	}

	"clippasteRAW" <- function(name, pos, ...) {
		expr <- paste("assign('", name, "',", paste(readClipboard(), sep="", collapse="\n"), ",pos=", pos, ")", sep="")
		try(eval(parse(text = expr)))
		invisible(TRUE)
	}

	res <- switch(type,
		raw = clippasteRAW(name, pos, ...),
		clippasteASCII(name, objclass, pos, ...))	# If "ASCII" or nomatch
	invisible(res)
}
