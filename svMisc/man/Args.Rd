\name{Args}
\alias{Args}
\alias{CallTip}

\title{ Show function arguments in a human-readable way - get a call tip }
\description{
  \code{Args()} displays function arguments in a better way than \code{args()} does. It is primarily intended for code tips in GUIs.
}
\usage{
Args(name)
CallTip(code)
}
\arguments{
  \item{name}{ A string with the name of a function }
  \item{code}{ A fraction of R code ending with the name of a function, eventually followed by '(' }
}

\value{
  A string with the calling syntax of the function
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }
\note{ \code{Args()} is supposed to display S3/S4 methods, and primitives
  adequately,... but this is not implemented yet in the current version! }

\seealso{ \code{\link{findhtmlhelp}} }
\examples{
	Args("ls")
	CallTip("myvar <- lm(")
}
\keyword{ utilities }
