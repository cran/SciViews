"guiViewsFile" <-
function()
    paste(tempfile(pattern = "view", tmpdir = guiViewsDir()), "html", sep = ".")
