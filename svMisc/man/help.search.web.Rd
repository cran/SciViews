\name{help.search.web}
\alias{help.search.web}

\title{ Search web documents about R and R functions }
\description{
  Retrieve web documents, messages in R mailing lists, or wiki containing \code{string}.
}
\usage{
    help.search.web(apropos, type = c("google", "archive", "wiki"))
}

\arguments{
  \item{apropos}{ The string to search }
  \item{type}{ The search engine, or location to use }
}

\value{
  Returns \code{TRUE} invisibly (invoked for its side effect of opening the web browser with the search result)
}
\author{ David Forrest <drf@vims.edu> & Philippe Grosjean <phgrosjean@sciviews.org> after Barry Rowland's original code, but see <http://fawn.unibw-hamburg.de/cgi-bin/Rwiki.pl?SearchFunctions> }

\seealso{ \code{\link[utils]{help.search}} }
\examples{
    \dontrun{
        help.search.web("volatility") # Google by default, may soon show Dr. Harrell's example
        help.search.web("volatility", type = "archive")  # In mailing list archive
        help.search.web("search", type = "wiki")
    }
}
\keyword{ utilities }
