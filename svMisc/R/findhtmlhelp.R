"findhtmlhelp" <-
function (topic, package = .packages(), lib.loc = NULL,
	verbose = getOption("verbose")) {
	# Determine if there is a HTML help file for a given topic
	# If yes return the filename, else return ""
	if (!missing(package))
		if (is.name(y <- substitute(package)))
			package <- as.character(y)
	if (!missing(topic)) {
		topic <- substitute(topic)
		if (is.name(topic)) topic <- as.character(topic)
		else if (!is.character(topic)) stop("Unimplemented help feature")
		if (!is.na(match(topic, c("+", "-", "*", "/", "^", "%%"))))
			topic <- "Arithmetic"
		else if (!is.na(match(topic, c("<", ">", "<=", ">=", "==", "!="))))
			topic <- "Comparison"
		else if (!is.na(match(topic, c("[", "[[", "$"))))
			topic <- "Extract"
		else if (!is.na(match(topic, c("&", "&&", "|", "||", "!"))))
			topic <- "Logic"
		else if (!is.na(match(topic, c("%*%"))))
			topic <- "matmult"
	}
	INDICES <- .find.package(package, lib.loc, verbose = verbose)
	file <- index.search(topic, INDICES, "AnIndex", "html")
	if (length(file) && file != "") {
		file <- chartr("/", "\\", file)
		if (file.exists(file))
			return(file)
	} else return("")
}
