\name{createSyntaxFile}
\alias{createSyntaxFile}

\title{ Create a syntax definition file for R language }
\description{
  A .svl syntax file describes the syntax of the language for SciViews GUIs. It is used mainly for syntax coloring of text in editors.
}
\usage{
createSyntaxFile(svlfile = "R.svl", pos = 2:length(search()))
}

\arguments{
  \item{svlfile}{ The name or location of the .svl file you want to create }
  \item{pos}{ A vector of integers indicating which positions in the search path should be recorded in the file }
}
\value{
  This function returns nothing. It is invoked for its side effects.
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }
\note{ SciViews-R uses a file named 'R.svl' and located in <SciViewsDir>/bin/languages. This function generates such a file. Do resist to the temptation to generate a very long list of keywords by loading many packages. SciViews cannot handle a list longer that 32k, that is roughly, 2000 - 2500 keywords }

\seealso{ \code{\link{getFunctions}}, \code{\link{getKeywords}} }
\examples{
	## Not run:
	\dontrun{createSyntaxFile("Rtemp.svl")
	file.show("Rtemp.svl", delete.file = TRUE)}
	## End(Not run)
}
\keyword{ utilities }
