"listMethods" <-
function(f, S3 = TRUE, S4 = TRUE) {
	# Given a function, if it is generic then return a list of its methods
	
	## Check argument
	if (!inherits(f, "character"))
		stop("'f' must ba a character string!")
	# Keep only first item if a vector is provided
	f <- f[1]
	# Does this function exists?
	if (!exists(f, where = 1, mode = "function", inherits = TRUE))
		stop("'f' is not an existing function!")
	
	res <- NULL
		
	## S3 version
	if (S3) {
		# Get the list of functions that look like S3 methods for f
		S3met <- unclass(methods(f))	# It return a "MethodsFunction" object
		# Eliminate fun. in front of the methods' name
		S3met <- sub(paste(f, ".", sep = ""), "", S3met)
		L <- length(S3met)
		if (L > 0) { # Test all these possible candidates
			S3OK <- NULL
			for (i in 1:L) {
				test <- try(getS3method(f, S3met[i]), silent = TRUE)
				S3OK[1] <- (!inherits(test, "try-error"))	
			}
			# Keep only those candidates that succeed in the test
			S3met <- S3met[S3OK]
			attr(S3met, "info") <- attr(S3met, "info")[S3OK, ]
		}
		res$S3 <- S3met
	}
	
	## S4 version
	if (S4) {
		S4met <- character(0)
		if (isGeneric(f, where = .GlobalEnv)) {
			allS4met <- getMethods(f, where = .GlobalEnv)@allMethods
			if (!is.null(allS4met))
				S4met <- names(allS4met)
		}
		res$S4 <- S4met
	}
	
	res
}
