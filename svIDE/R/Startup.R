# last modified 22 February 2005 by Ph. Grosjean

".onLoad" <-
function(lib, pkg) {
    # Starting the DDE server automatically if under Windows
    if (.Platform$OS.type == "windows") guiDDEInstall()
    
    # If an IDE is defined, start it now
    IDE <- getOption("IDE")
    if (!is.null(IDE) && file.exists(IDE))
        system(paste("\"", IDE, "\"", sep = ""), wait = FALSE)
}

