\name{Complete}
\alias{Complete}

\title{ Get a completion list for a R code fragment }
\description{
  A list with matching items is returned in a string.
}
\usage{
Complete(code, givetype = FALSE, fieldsep = "|")
}
\arguments{
  \item{code}{ A fraction of R code needing completion }
  \item{givetype}{ Do we append the type of each entry to the list? }
  \item{fieldsep}{ The separator to use between the item and its type }
}

\value{
  A string with the completion list
}

\details{
    The completion list is context-dependent, and it calculated as if the code
    was entered at the command line.
    
    If the code ends with \code{$} or \code{[[}, then the function look for items
    in a list or data.frame whose name is the last idfentifier.
    
    If the code ends with \code{@}, then the function look for slots of the
    corresponding S4 object.
    
    If the code ends with a partial identifier name, the function returns all
    visible matching keywords.
    
    In all other situations, the function returns a list of the objects in
    .GlobalEnv.
}

\author{ Philippe Grosjean <phgrosjean@sciviews.org> }

\seealso{ \code{\link{guiComplete}} }
\examples{
    ## a data.frame
    data(iris)
	cat(Complete("item <- iris$"), "\n")
	cat(Complete("item <- iris[[", TRUE), "\n")
	
	## An S4 object
    setClass("track", representation(x = "numeric", y = "numeric"))
    t1 <- new("track", x=1:20, y=(1:20)^2)
    cat(Complete("item2 <- t1@", TRUE), "\n")
    
    ## A partial identifier
    cat(Complete("item3 <- va", TRUE), "\n")
    
    ## Otherwise, a list with the content of .GlobalEnv
    cat(Complete("item4 <- "), "\n")
}
\keyword{ utilities }
