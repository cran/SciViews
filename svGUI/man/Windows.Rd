\name{Windows}
\alias{Windows}

\title{ A graphic window displayed in the GUI client (for Windows only!) }
\description{
  This is similar to \code{windows} under Windows, but the device is displayed in the GUI client.
}
\usage{
Windows(width = 7, height = 7, pointsize = 12, record = getOption("graphics.record"),
    rescale = c("R", "fit", "fixed"), xpinch, ypinch, bg = "transparent", canvas = "white",
    gamma = getOption("gamma"), xpos = NA, ypos = NA, buffered = getOption("windowsBuffered"))
}

\arguments{
  \item{width}{ The width of the graph device }
  \item{height}{ The height of the graph device }
  \item{pointsize}{ The base size of the font }
  \item{record}{ Should recording mode be turned on? }
  \item{rescale}{ The way the graph is rescaled }
  \item{xpinch}{ X aspect ratio of the screen }
  \item{ypinch}{ Y aspect ratio of the screen }
  \item{bg}{ Color for the background }
  \item{canvas}{ Color for the canvas }
  \item{gamma}{ Gamma correction to apply }
  \item{xpos}{ X position of the window }
  \item{ypos}{ Y position of the window }
  \item{buffered}{ Should the window be buffered? }
}

\value{
  A new graph device is created.
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{guiStart}} }
\examples{
    #### TO DO...
}
\keyword{misc}
