"guiObjSearch" <-
function(path = NULL, compare = TRUE) {
    # Done in NAMESPACE
    #require(svMisc)
    Search <- as.matrix(data.frame(Workspace = search()))
    if (!is.null(path)) {  # Write to a file called 'Search.txt' in this path
		file <- file.path(path, "Search.txt")
		if (compare) {
            oldSearch <- getTemp(".guiObjSearchCache", default = "")
 	        # Compare both versions
	        if (!(all.equal(Search, oldSearch)[1] == TRUE)) {
                assignTemp(".guiObjSearchCache", Search) # Keep a copy of the last version in TempEnv
		        Changed <- TRUE
			} else Changed <- FALSE
        } else Changed <- TRUE
		if (Changed) write.table(Search, file = file, row.names = FALSE, quote = FALSE, sep = "\t")
		return(Changed)
	} else return(Search) # If no file is specified, just return the matrix
}

