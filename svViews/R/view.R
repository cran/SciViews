"view" <-
function(x, type = "summary", objname = deparse(substitute(x)), ...)
	UseMethod("view")
