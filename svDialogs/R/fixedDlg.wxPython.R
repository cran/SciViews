"guiDlgDir.wxPython" <- function(title = "Select a directory", dir = getwd(),
    new = TRUE, parent = 0) {
    # This function is not supposed to be called directly, so,
    # we do not check its arguments again here.
    # Done in NAMESPACE
    #require(svMisc)
    if (!Require(wxPython)) {
        # Allow for using default dialog box!
        warning("Package 'wxPython' required. Please, (re)install it!")
        return(NULL)
    }
    # Use wxDirDialog()
    ## parent and default arguments not used here yet!
    # We must check new manually here!
    repeat {
        res <- wxDirDialog(Set = c("Path", "Message"), SetValue = c(dir, title))
        if (is.null(res)) {# The user clicked 'Cancel'
            res <- character(0)# We have to return character(0) here instead
            break()
        } else {
            if (!new) { # The directory should exist
                if (file.exists(res) && file.info(res)$isdir) {
                    break()
                } else {
                    wxMessagebox(message = "This directory does not exist,\nplease select or enter an existing directory.",
                        title = "Error", icon = "warning", type = "ok",
                    .convert = FALSE)
                }
            } else {
                # We check it does not exist yet, or it is a directory
                if (!file.exists(res) || file.info(res)$isdir) {
                    break()# This is fine
                } else {
                    wxMessagebox(message = "This is not a directory,\nplease select or enter a new directory.",
                        title = "Error", icon = "warning", type = "ok",
                   .convert = FALSE)
                }
            }
        }
    }
    #### TO DO: bug! Text entered in the textbox (for new dirs) is not considered!
    return(invisible(res))
}

"guiDlgInput.wxPython" <- function(message = "Enter a value", title = "Input",
    default = "", parent = 0) {
    # This function is not supposed to be called directly, so,
    # we do not check its arguments again here.
    # Done in NAMESPACE
    #require(svMisc)
    if (!Require(wxPython)) {
        # Allow for using default dialog box!
        warning("Package 'wxPython' required. Please, (re)install it!")
        return(NULL)
    }
    # Use wxTextEntryDialog()
    ## parent argument not used here yet!
    res <- wxTextEntryDialog(message = message, caption = title,
        defaultValue = default)
    # If 'x' is clicked on the dialog box, it return NULL
    # but we need character(0) instead!
    if (is.null(res)) res <- character(0)
    return(invisible(res))
}

"guiDlgList.wxPython" <- function(items, message = "Select one item:",
    title = "Selection", default = 1, multi = FALSE, new = FALSE,
    sort = FALSE, parent = 0) {
    # This function is not supposed to be called directly, so,
    # we do not check its arguments again here.
    # Done in NAMESPACE
    #require(svMisc)
    if (!Require(wxPython)) {
        # Allow for using default dialog box!
        warning("Package 'wxPython' required. Please, (re)install it!")
        return(NULL)
    }
    # Use wxTextEntryDialog()
    ## parent argument not used here yet!
    ### TO DO...
	stop("Not implemented yet!")
    # If 'x' is clicked on the dialog box, it return NULL
    # but we need character(0) instead!
    #if (is.null(res)) res <- character(0)
    #return(invisible(res))
    return(NULL)
}

"guiDlgMessage.wxPython" <- function(message, title = "Message", type = "ok",
    default = 1, icon = "info", parent = 0) {
    #### TO DO: handle default button settings!
    # This function is not supposed to be called directly, so,
    # we do not check its arguments again here.
    # Done in NAMESPACE
    #require(svMisc)
    if (!Require(wxPython)) {
        # Allow for using default dialog box!
        warning("Package 'wxPython' required. Please, (re)install it!")
        return(NULL)
    }
    # Use wxMessageBox()
    ## parent and default arguments not used here yet!
    res <- wxMessageBox(message = message, title = title, type = type,
        icon = icon, .convert = TRUE)
    return(invisible(res))
}

"guiDlgOpen.wxPython" <- function(title = "Select file", defaultFile = "",
    defaultDir = "", multi = FALSE, filters = c("All files (*.*)", "*.*"),
    parent = 0) {
    #### TO DO: better handling of filters!
    #### TO DO: allowing multiple selection of files!
    if (multi) warning("multi == TRUE not handled yet in wxPython!")
    # This function is not supposed to be called directly, so,
    # we do not check its arguments again here.
    # Done in NAMESPACE
    #require(svMisc)
    if (!Require(wxPython)) {
        # Allow for using default dialog box!
        warning("Package 'wxPython' required. Please, (re)install it!")
        return(NULL)
    }
    # Use wxFileOpenDialog()
    ## parent argument not used here yet!
    # If the file already exists, must deal with the ask for confirmation!
    repeat {
        res <- wxFileOpenDialog(Set = c("Message", "Filename", "Directory",
            "Wildcard"), SetValue = c(title, defaultFile, defaultDir,
            paste(filters[ , 2], collapse = ";")),
        Get = c("Directory", "Filename"))
        if (length(res) == 2) {# Return directory and filename
            defaultdir = res[1]
            defaultFile = res[2]
            res <- file.path(gsub("[\\]", "/", res[1]), res[2])
        }
        if (is.null(res)) {# The user clicked 'Cancel'
            res <- character(0)# We have to return character(0) here instead
            break()
        } else if (file.exists(res)) break() else {
            # The file does not exists!
            wxMessageBox(message = paste(defaultFile,
                "\nFile not found.\nPlease verify the correct file name was given."),
                title = title, icon = "warning", type = "ok", .convert = FALSE)
        }
    }
    return(invisible(res))
}

"guiDlgSave.wxPython" <- function(title = "Save As", defaultFile = "",
    defaultDir = "", defaultExt = "", filters = c("All files (*.*)", "*.*"),
    parent = 0) {
    #### TO DO: better handling of filters!
    # This function is not supposed to be called directly, so,
    # we do not check its arguments again here.
    # Done in NAMESPACE
    #require(svMisc)
    if (!Require(wxPython)) {
        # Allow for using default dialog box!
        warning("Package 'wxPython' required. Please, (re)install it!")
        return(NULL)
    }
    # Use wxFileSaveDialog()
    ## parent argument not used here yet!
    # If the file already exists, must deal with the ask for confirmation!
    repeat {
        res <- wxFileSaveDialog(Set = c("Message", "Filename", "Directory",
            "Wildcard"), SetValue = c(title, defaultFile, defaultDir,
            paste(filters[ , 2], collapse = ";")),
        Get = c("Directory", "Filename"))
        if (length(res) == 2) {# Return directory and filename
            defaultdir = res[1]
            defaultFile = res[2]
            res <- file.path(gsub("[\\]", "/", res[1]), res[2])
        }
        if (is.null(res)) {# The user clicked 'Cancel'
            res <- character(0)# We have to return character(0) here instead
            break()
        } else if (!file.exists(res)) break() else {
            # The file already exists, ask for confirmation!
            res2 <- wxMessageBox(message = paste(res, "already exists,\nDo you want to replace it?"),
                title = title, icon = "warning", type = "yesno",
                .convert = TRUE)
            if (res2 == "yes") break()
        }
    }

    if (length(res) > 0) {
        # Default extension is not treated yet, do it now!
        # If there is no dot in the name, append the extension
        if (length(grep("[\.]", basename(res))) == 0)
            if (defaultExt != "") res <- paste(res, defaultExt, sep = ".")
    }
    return(invisible(res))
}
