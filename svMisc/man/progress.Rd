\name{progress}
\alias{progress}

\title{ Display progression of a long calculation on the console }
\description{
  Display progression level of a long-running task in the console. Two mode can be used: either percent of achievement (55\%), or the number of items or steps done on a total (1 file on 10 done...).
}
\usage{
progress(value, max.value = NULL)
}

\arguments{
  \item{value}{ ~~Describe \code{value} here~~ }
  \item{max.value}{ ~~Describe \code{max.value} here~~ }
}
\details{
  The function uses backspace (\\8) to erase characters at the console.
}
\value{
  This function returns NULL invisibly. It is invoked for its side effects.
}
\author{ Philippe Grosjean <phgrosjean@sciviews.org> }
\note{ In a GUI, it is preferable to use a non modal dialog box with a progress widget, or to display such a progress widget in the status bar of your main window. See package 'tk2' for further information. }

\examples{
	# 1) A simple progress indicator in \%
	cat("\nSimple progress indicator in \%:\n")
	for (i in 1:101) {
		progress(i)
		Sys.sleep(0.05)
	}
	cat("Done!\n")

	# 2) A progress indicator with 'x on y'
	cat("\nA variation...\n")
	for (i in 1:31) {
		progress(i, 30)
		Sys.sleep(0.2)
	}
	cat("Done!\n")
}
\keyword{ utilities }
