"Args" <-
function(name){
	# A better args() function using formals()
	#### TO DO: handle primitives and S3/S4 methods for generic functions
	res <- formals(name)
	if (length(res) < 1){
		res <- paste(name, "()", sep = "")
	} else {
		res1 <- names(res)
		for (i in 1:length(res1))
			if (deparse(res[[res1[i]]])[1] != "") 
				res1[i] <- paste(res1[i], "=", deparse(res[[res1[i]]]))
		res <- paste(res1, collapse = ", ")
		res <- paste(name, "(", res, ")", sep = "")
	}
	res
}
