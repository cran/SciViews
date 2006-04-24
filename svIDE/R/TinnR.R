# Specific functions for Tinn-R (adapted by J.-C. Faria from function in svGUI
# made by Ph. Grosjean)

"trObjSearch" <-
function (path = NULL, compare = TRUE) {
	Search <- as.matrix(data.frame(Workspace = search()))
	if (is.null(path)) return(Search)
	file <- file.path(path, "Search.txt")
	if (compare) {
		oldSearch <- getTemp(".guiObjSearchCache", default = "")
		if (!(all.equal(Search, oldSearch)[1] == TRUE)) {
			assignTemp(".guiObjSearchCache", Search)
			Changed <- TRUE
		} else Changed <- FALSE
	} else Changed <- TRUE
    write.table(Search, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
}

"trObjList" <-
function(id = "default", env.name = NULL, pos = 1, all.names = FALSE,
pattern = "", group = "", path = NULL, compare = TRUE) {
	oWidth <- getOption("width")
	options(width = 100)
	on.exit(options(width = oWidth))

	"describe" <- function(name, pos = 1) {
		Obj <- get(name, pos = pos)
		ObjDetails <- ""
		ObjGroup <- "other"
		ObjClass <- class(Obj)[1]
		if (inherits(Obj, "function")) {
			ObjGroup <- "function"
		} else if (inherits(Obj, "data.frame")){
			ObjDetails <- paste(dim(Obj), collapse = "x")
			ObjGroup <- "data.frame"
		} else if (inherits(Obj, "matrix")) {
			ObjDetails <- paste(dim(Obj), collapse = "x")
			ObjGroup <- "matrix"
		} else if (inherits(Obj, "array")) {
			ObjDetails <- paste(dim(Obj), collapse = "x")
			ObjGroup <- "array"
		} else if (inherits(Obj, "table")) {
			ObjDetails <- paste(dim(Obj), collapse = "x")
			ObjGroup <- "table"
		} else if (inherits(Obj, "list")) {
			ObjDetails <- as.character(length(Obj))
			ObjGroup <- "list"
		} else if (inherits(Obj, "vector")) {
			ObjDetails <- as.character(length(Obj))
			ObjGroup <- "vector"
		} else if (is.vector(Obj)) {
			ObjDetails <- as.character(length(Obj))
			ObjGroup <- "vector"
		}
		res <- c(name, ObjDetails, ObjGroup, ObjClass)
		return(res)
	}

	# Make sure that id is character
	id <- as.character(id)
	if (id == "") id <- "default"

	# Get pos if env.name is provided
	if (!is.null(env.name)) {
		pos <- match(env.name, search())
		if (is.na(pos)) pos <- 1	# Default value when no match (go back to .GlobalEnv)
	}

	# Get the list
	RawList <- ls(pos = pos, all.names = all.names, pattern = pattern)
	if (length(RawList) > 0) {
		List <- t(sapply(RawList, describe, pos = pos))
		if (nchar(group) > 0)
			if (group == "data") {	# Special treatment, everything that is not a function
				List <- List[List[, 3] != "function"]
			} else List <- List[List[, 3] == group, ]
		List <- matrix(List, ncol = 4)
		if (length(List) < 1) List <- matrix(c("","","",""), nrow = 1)
	} else List <- matrix(c("", "", "", ""), nrow = 1)
	colnames(List) <- c("Name", "Dims", "Group", "Class")

	if (!is.null(path)) {  # Write to files in this path
		# Create file names
		ListFile <- file.path(path, paste("List_", id, ".txt", sep=""))
		ParsFile <- file.path(path, paste("Pars_", id, ".txt", sep=""))
		# Determine if it is required to refresh these files
		if (compare) {
			allList <- getTemp(".guiObjListCache", default = list())
			if (length(allList) >= id) oldList <- allList[[id]] else oldList <- ""
			if (is.null(oldList)) oldList <- ""
			# Compare both versions
			if (!(all.equal(List, oldList)[1] == TRUE)) {
				allList[[id]] <- List
				# Keep a copy of the last version in TempEnv
				assignTemp(".guiObjListCache", allList)
				Changed <- TRUE
			} else Changed <- FALSE
		} else Changed <- TRUE
		if (Changed) {
			write.table(List, file = ListFile, row.names = FALSE, quote = FALSE, sep = "\t")
			# Write also in the Pars_<id>.txt file in the same directory
			cat("pos=", pos, "\n", sep = "", file = ParsFile)
			cat("envir=", search()[pos], "\n", sep = "", file = ParsFile, append = TRUE)
			cat("all.names=", all.names, "\n", sep = "", file = ParsFile, append = TRUE)
			cat("pattern=", pattern, "\n", sep = "", file = ParsFile, append = TRUE)
			cat("group=", group, "\n", sep = "", file = ParsFile, append = TRUE)
		}
	} else return(List) # If no path is specified, just return the matrix
}
