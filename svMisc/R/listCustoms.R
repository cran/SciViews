"listCustoms" <-
function(method, class) {
	# List all custom functions for a method and for a given class
	# For instance, a custom view is a function as 'view.class..<customview>'
	Pat <- paste("^", method, ".", class, "..", sep="")
	List <- sub(Pat, "", apropos(Pat, mode = "function"))
	return(List)
}
