"guiSource" <-
function(file, out.form = getOption("R.output.format"), local = FALSE, echo = FALSE, print.eval = TRUE,
    verbose = getOption("verbose"), prompt.echo = getOption("prompt"),
    max.deparse.length = 150, chdir = FALSE) {

    # This is a reworked version of .Rsource from RpadUtils (Tom Short)
	# but this version uses source() itself
	
	if (is.null(out.form)) out.form <- "text"
	# We capture output from source() with default args slightly modified
	res <- capture.output(source(file = file, echo = echo, print.eval = print.eval,
		verbose = verbose, prompt.echo = prompt.echo,
		max.deparse.length = max.deparse.length, chdir = chdir))
    if (out.form == "html") {
		require(R2HTML)
		res <- HTML(res, file = "")
    } else if (out.form != "none")
        res <- paste(paste(res, collapse="\n"), "\n")
    invisible(res)
}

