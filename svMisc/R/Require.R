"Require" <-
function(package, bundle = NULL, quietly = FALSE, warn.conflicts = TRUE,
    keep.source = getOption("keep.source.pkgs"), character.only = FALSE,
    version, save = TRUE, gui=getOption("guiRequire")) {
    if (character.only == FALSE)
        packageName <- as.character(substitute(package))
    else
        packageName <- package
    
    if (packageName %in% installed.packages()[,"Package"] || packageName %in% .packages()) {
        return(require(packageName, quietly, warn.conflicts, keep.source,
            character.only = TRUE, version, save))
    }    
    if (is.null(gui)) gui <- FALSE
    if (!gui || .Platform$OS.type != "windows") {
        return(require(packageName, quietly, warn.conflicts, keep.source,
            character.only=TRUE, version, save))
    }
    # Not needed any more with R 2.0.0 (use Depends: utils) require(utils) # for winDialog, file.choose, etc...
    libPaths <- .libPaths()
    libPathsString <- "''"
    if (length(libPaths) == 1)
        libPathsString <- .libPaths()
    if (length(libPaths) > 1)
        libPathsString <- paste("\"", paste(.libPaths(), collapse = "; "), "\"", sep = "")
    if (.Platform$OS.type == "windows")
        libPathsString <- gsub("/", "\\\\", libPathsString)
        
    if (!is.null(bundle)) packageName <- bundle # If the package is part of a bundle, we are better to look for the latter one!
	yesno <- winDialog(type = "yesno", message = paste("Package or bundle '", packageName, "' was not found in ",
        libPathsString, ".  Would you like to install it now?", sep = ""))
    if (yesno == "NO") {
        warning(paste("Missing package or bundle '", packageName, "' was neither installed nor loaded", sep = ""))
        return(FALSE)
    }

    validRepositoryOrFileChosen <- FALSE
    while (validRepositoryOrFileChosen == FALSE) {
        choicelist <- c("CRAN", "Bioconductor", "SciViews", "Local Repository", "Local Zip File")
        choice <- select.list(choicelist)
        ## check for cancel button
        if (choice == "") {
            warning(paste("Missing package or bundle '", packageName, "' was neither installed nor loaded", sep = ""))
            return(FALSE)
        }
        if (choice == "CRAN")
            Contrib <- contrib.url(getOption("CRAN"))
        else if (choice == "Bioconductor")
            Contrib <- contrib.url(getOption("BIOC"))
		else if (choice == "SciViews")
            Contrib <- "http://www.sciviews.org/SciViews-R"
        else if (choice == "Local Repository") {
            # Ask to select the repository "PACKAGES" description file
            pack <- choose.files(caption = "Select the directory PACKAGES description file",
                multi = FALSE, filters = c("Packages description", "PACKAGES"))
            if (length(pack) == 0) { # The user pressed Cancel
                warning(paste("Missing package or bundle '", packageName, "' was neither installed nor loaded", sep = ""))
                return(FALSE)
            }
            pack <- gsub("\\\\", "/", pack)
            Contrib <- sub("PACKAGES", "", paste("file:", pack, sep = ""))
        }
        validRepositoryOrFileChosen <- TRUE
        CRANpackages <- ""
        if (choice != "Local Zip File" &&
            inherits(try(suppressWarnings(CRANpackages <- CRAN.packages(contriburl = Contrib)[ , "Package"]), TRUE), "try-error")) {
            if (validRepositoryOrFileChosen == TRUE) { # until now, that is!
                yesno <- winDialog(type = "yesno", message = paste("The repository '", Contrib,
                     "' does not appear to be a valid repository.",
                     " Would you like to try another repository?", sep = ""))
                if (yesno == "NO") {
                    warning(paste("Missing package or bundle '", packageName, "' was neither installed nor loaded", sep = ""))
                    return(FALSE)
                }
                validRepositoryOrFileChosen <- FALSE
            }
        }
        if (CRANpackages[1] != "" || length(CRANpackages) > 1)
            if (choice != "Local Zip File" && !(packageName %in% CRANpackages)) {
                if (validRepositoryOrFileChosen == TRUE) { # until now, that is!
                    yesno <- winDialog(type = "yesno", message = paste("The package '", packageName,
                          "' was not found in the selected repository (", choice, ").",
                          " Would you like to try another repository?", sep = ""))
                    if (yesno == "NO") {
                        warning(paste("Missing package or bundle '", packageName, "' was neither installed nor loaded", sep = ""))
                        return(FALSE)
                    }
                    validRepositoryOrFileChosen <- FALSE
                }
        }
    }

    if (choice == "Local Zip File") {
        invalidFileName <- TRUE
        while (invalidFileName) { # Or unknown file name the first time through the while loop.
            filename <- choose.files('', filters = Filters[c('zip', 'All'), ], multi = FALSE)
            if (filename == "") { # The user pressed Cancel
                warning(paste("Missing package or bundle '", packageName, "' was neither installed nor loaded", sep = ""))
                return(FALSE)
            }
            yesnocancel <- "no"
            if (length(grep(paste(packageName, sep = ""), filename)) == 0)
                yesnocancel <- winDialog(type = "yesnocancel", message = paste("The selected file name does not contain the package name '",
                    packageName, "'. Would you like to select a different file instead?", sep = ""))
            else if (length(grep("\\.zip$", filename)) == 0)
                yesnocancel <- winDialog(type = "yesnocancel", message = paste("The selected file name does not end with '.zip'.",
                    " Would you like to select a different file instead?", sep = ""))
            if (yesnocancel == "CANCEL") {
                warning(paste("Missing package or bundle '", packageName, "' was neither installed nor loaded", sep = ""))
                return(FALSE)
            }
            if (yesnocancel == "YES") invalidFileName <- TRUE
            if (yesnocancel == "NO") invalidFileName <- FALSE
        }
        install.packages(filename, .libPaths()[1], CRAN = NULL)
        return(require(packageName, character.only = TRUE))
    }
        
    if (choice != "Local Zip File") {
        if (packageName %in% CRANpackages) {
            install.packages(packageName, contriburl = Contrib)
            return(require(packageName, character.only = TRUE))
        } else {
            cat(paste("The package or bundle was not found on", choice, "\n"))
            return(FALSE)
        }
    }
}
