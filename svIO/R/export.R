"export" <-
function(x, type = "raw", file, append = FALSE, objname = deparse(substitute(x)), ...)
	UseMethod("export")
