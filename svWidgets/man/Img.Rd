\name{Img}
\alias{ImgGet}
\alias{ImgNames}
\alias{ImgType}
\alias{tkImgAdd}
\alias{tkImgDel}
\alias{tkImgReadPackage}

\title{ Manipulate image resources for the GUIs }
\description{
  Mechanism provided here is very simple and allows for automatic loading of image
  resources from any package subdirectory. Currently, only Tk images loaded from
  GIF files are supported... but more formats will be added in the future.
}
\usage{
  ImgGet(image)
  ImgNames()
  ImgType(image, warn = TRUE)
  tkImgAdd(file, type = "gif")
  tkImgDel(image)
  tkImgReadPackage(package, subdir = "gui", type = "gif")
}

\arguments{
  \item{image}{ Name of an image }
  \item{warn}{ Do we issue a warning if the type of image is not recognized? }
  \item{file}{ Image file from where to download the resource }
  \item{type}{ Type of image. Currently, only 'gif' is supported. }
  \item{package}{ Name of a package from where to load image resources }
  \item{subdir}{ Subdirectory in the package where the graphical resources are stored.
	By default, it is the "gui" subdirectory. }
}

\details{
  These functions care about (un)loading image resources. A list of these resources is
  maintained in '.gui.Imgs' located in the TempEnv environment.
}

\value{
  \code{tkImgAdd()} and \code{ImgGet()} return the handle to the newly created image.
  \code{ImgNames()} return the list of all images registered in .gui.Imgs.
}

\author{ Philippe Grosjean }

\seealso{ \code{\link{WinNames}} }

\examples{
  \dontrun{
    ## These cannot be run by examples() but should be OK when pasted
    ## into an interactive R session with the tcltk package loaded

	ImgNames()
	tkImgAdd(file.path(.path.package(package = "svWidgets")[1], "gui", "logoSciViews.gif"))
	ImgNames()
	ImgType("$Tk.logoSciViews")
	# Place that logo in a Tk window
	timg <- tkWinAdd("timg", title = "A Tk window with image", pos ="-40+20")
	labImg <- tklabel(timg, image = ImgGet("$Tk.logoSciViews"), bg = "white")
	tkpack(labImg)
	# When the image resource is deleted, it is not displayed any more, but there is no error
	tkImgDel("$Tk.logoSciViews")
	ImgNames()
	tkWinDelete("timg")
	# To read all image resources at once (place this in .Lib.first())
	tkImgReadPackage("svWidgets")
	ImgNames()
  }
}
\keyword{ utilities }
