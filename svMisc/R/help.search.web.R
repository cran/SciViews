"help.search.web" <-
function(apropos, type = c("google", "archive", "wiki")) {
	#acknowledge Barry Rowland code in r-help Fri, 08 Oct 2004 12:03:04

	type <- type[1]
	RSearchURL <- switch(type,
		"google"= paste("http://www.google.com/search?sitesearch=r-project.org&q=", apropos, sep = ''),
		"archive"= paste("http://www.google.com/u/newcastlemaths?q=", apropos, sep = ''),
		"wiki"= paste("http://fawn.unibw-hamburg.de/cgi-bin/Rwiki.pl?search=", apropos, sep = ''),
		stop("'type' could be only 'google', 'archive' or 'wiki', currently!"))
	browseURL(RSearchURL)
	return(invisible(0))
}

