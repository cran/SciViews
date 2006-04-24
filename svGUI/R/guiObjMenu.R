"guiObjMenu" <-
function(id = "default", selobject, objects, envir,  path = NULL) {
	# Get a context menu  for given object(s)
	# It must return a data frame with the following columns:
	# - type: either "item" or "menu"
	# - menu: name of the menu. "" for top menu, use & for shortcut
	# - item: name of the item in the menu, or "-" for a separator
	# - action: the command to issue. Precede it with "silent " to run the command silently
	#           <obj>, <objname>, <objlist>, <pos> and <envir> are replaced by their values by the object browser
	#           before the command is issued
	# - checked: if the menu entry should present a check mark (not used yet)
	# - enabled: if the menu entry is enabled (not used yet)
	# - icon: the icon to use in the menu (not used yet)
	
	# This is a preliminary menu... should be reworked and completed!
	# Should we use the convention to start a menu entry by ">", when the command is echoed to the prompt?
	
    # Done in NAMESPACE
    #require(svMisc)
    if (selobject == "") {	# This is a menu for an environment
		# The menu is different depending if we are in .GlobalEnv or not
		if (envir == ".GlobalEnv") {
			Menu <- data.frame(type = c("item", "item"), menu = c("", ""), item = c("&Load", "&Source"),
							   action = c("#LOAD#", "#SOURCE#"), checked = c(FALSE, FALSE),
			                   enabled = c(TRUE, TRUE), icon = c("", ""))
		} else {	# Another environment than .GlobalEnv
			Menu <- NULL	# if package
			if (strsplit(envir, ":")[[1]][1] == "package") {
				# Rem: detach should not be used for package:base, for instance!
				pckname <- strsplit(envir, ":")[[1]][2]
				nondetachpck <- c("base", "TempEnv", "svGUI", "svMisc", "svIO", "svViews", "svDialogs", "svWin", "R2HTML", "tcltk", "tcltk2", "HMisc", "methods", "stats", "graphics", "grDevices", "utils", "datasets", "Autoloads")
				if (!(pckname %in% nondetachpck))
					Menu <- rbind(Menu, data.frame(type = c("item"), menu = c(""), item = c("&Detach"),
								  action = c("detach(<envir>)"), checked = c(FALSE),
								  enabled = c(TRUE), icon = c("")))
				# Info on package (package content)
				Menu <- rbind(Menu, data.frame(type = c("item"), menu = c(""), item = c("Package &Info"),
							  action = c(paste("library(help = ", pckname, ")", sep = "")), checked = c(FALSE),
							  enabled = c(TRUE), icon = c("")))
			} else {
				Menu <- rbind(Menu, data.frame(type = c("item"), menu = c(""), item = c("&Detach"), 
							  action = c("detach(<envir>)"), checked = c(FALSE),
							  enabled = c(TRUE), icon = c("")))
			}
			
		}
	} else {	# This is a menu for an object
		# The first entry of View menu is always 'View' -> 'default >'
		if (envir == ".GlobalEnv") {
			ActionList <- c("", "view(<obj>)", "")
		} else {
			ActionList <- c("", "view(<obj>, objname = <objname>)", "")
		}
		Menu <- data.frame(type = c("menu", "item", "item"), menu = c("&View", "&View", "&View"), item = c("", "&default", "-"),
		                   action = ActionList, checked = c(FALSE, FALSE, FALSE),
			               enabled = c(TRUE, TRUE, TRUE), icon = c("", "", ""))
	
		# Complete the '&View' submenu with more entries
		obj <- get(selobject, pos = envir)
		viewlist <- eval(parse(text = paste("view(get('", selobject, "', pos = '", envir, "'), type = 'typelist', path = '')", sep = "")))
		llist <- length(viewlist)
		if (llist > 0) {
			MenuItems <- data.frame(type = rep("item", llist), menu = rep("&View", llist), item = viewlist,
			  	action = paste("view(<obj>, type='", viewlist, "', objname = <objname>)", sep = ""), checked = rep(FALSE, llist),
			  	enabled = rep(TRUE, llist), icon = rep("", llist))
			Menu <- rbind(Menu, MenuItems)
		}
		
		# Add &Edit, -, &Report & &Save objects that must be always there
		if (envir == ".GlobalEnv") {
			ActionEdit <- c("<obj> <- edit(<obj>)", "report(<obj>)")
		} else {
			ActionEdit <- c(paste("assign(<objname>, edit(<obj>), pos = 1)", sep=""), "report(<obj>, objname = <objname>)")  
		}
		MenuItems <- data.frame(type = c("item", "item"), menu = c("", ""), item = c("&Edit", "&Report"),
			action = ActionEdit, checked = c(FALSE, FALSE), enabled = c(TRUE, TRUE), icon = c("", ""))
		Menu <- rbind(Menu, MenuItems)
	
		# If we are in .GlobalEnv and the object is a data.frame, then I can attach/detach it
		if (envir == ".GlobalEnv" && inherits(obj, "data.frame")) {
			# Look if this object is already attached (i.e., present in the search path)
			if (selobject %in% search()) {
				MenuItems <- data.frame(type = c("item", "item", "item"), menu = c("", "", ""), item = c("Re&attach", "&Detach", "-"),
		                                action = c("detach(<objname>); attach(<obj>)", "detach(<objname>)", ""), checked = c(FALSE, FALSE, FALSE),
			                            enabled = c(TRUE, TRUE, TRUE), icon=c("", "", "")) 	
			} else {	# The object is not attached yet...
				MenuItems <- data.frame(type = c("item", "item"), menu = c("", ""), item = c("&Attach", "-"),
					                    action = c("attach(<obj>)", ""), checked = c(FALSE, FALSE),
			                            enabled = c(TRUE, TRUE), icon=c("", "")) 
			}
			Menu <- rbind(MenuItems, Menu)
		}
	
		# We look if there is an help file, then we add Help and Example at the top of the menu
		if (eval(parse(text = paste("findhtmlhelp('", selobject, "')", sep = ""))) != "") {
			MenuItems <- data.frame(type = c("item", "item", "item"), menu = c("", "", ""), item = c("&Help", "&Example", "-"),
		                            action = c("help(<objname>)", "example(<objname>)", ""), checked = c(FALSE, FALSE, FALSE),
			                        enabled = c(TRUE, TRUE, TRUE), icon=c("", "", "")) 
			Menu <- rbind(MenuItems, Menu)
		}
		
		# Create the '&Copy' submenu with all possible entries
		copylist <- eval(parse(text = paste("copy(get('", selobject, "', pos = '", envir, "'), type = 'typelist')", sep = "")))
		llist <- length(copylist)
		if (llist > 0) { 
			MenuCopy <- data.frame(type = "menu", menu = "&Copy", item = "", action = "", checked = FALSE, enabled = TRUE, icon = "")
			MenuItems <- data.frame(type = rep("item", llist), menu = rep("&Copy", llist), item = copylist,
				                    action = paste("copy(<obj>, type = '", copylist, "', objname = <objname>)", sep = ""), checked = rep(FALSE, llist),
				                    enabled = rep(TRUE, llist), icon = rep("", llist))
			Menu <- rbind(Menu, MenuCopy, MenuItems)
		}
		### TO DO: create the E&xport... submenu with the same entries (same format as copy(), but ask for a file)
	
		# Add -, &Multiple objects, &Save..., De&lete (if .GlobalEnv) and &Select all 
		if (envir == ".GlobalEnv") {
			MenuItems <- data.frame(type = c("item", "menu", "item", "item", "item"),
									menu = c("", "&Multiple objects", "&Multiple objects", "&Multiple objects", ""),
									item = c("-", "", "&Save...", "De&lete", "&Select all"),
				                    #action = c("", "", "silent guiSave(list = <objlist>, envir = pos.to.env(<pos>))", "silent guiRemove(<objlist>, ask = FALSE)", "#SELECT ALL#"),
				                    # Rem: do not ask for confirmation yet for object remove, because the dialog box appears behind R (will be fixed!)
				                    # also, when run silently, the object browser is not updated => force update after objects removing!
				                    action = c("", "", "silent guiSave(list = <objlist>, envir = pos.to.env(<pos>))", "remove(list = <objlist>)", "#SELECT ALL#"),
				                    checked = c(FALSE, FALSE, FALSE, FALSE, FALSE),
				                    enabled = c(TRUE, TRUE, TRUE, TRUE, TRUE),
				                    icon = c("", "", "", "", ""))
				                    # Rem: "#SELECT ALL#" is a code interpreted by the object browser directly to select all objects in it
		} else MenuItems <- data.frame(type = c("item", "menu", "item", "item"),
									menu = c("", "&Multiple objects", "&Multiple objects", ""),
									item = c("-", "", "&Save...", "&Select all"),
				                    action = c("", "", "silent guiSave(list = <objlist>, envir = pos.to.env(<pos>))", "#SELECT ALL#"),
				                    checked = c(FALSE, FALSE, FALSE, FALSE),
				                    enabled = c(TRUE, TRUE, TRUE, TRUE),
				                    icon = c("", "", "", "")) 
		Menu <- rbind(Menu, MenuItems)
	}	# Done (menu construction)
	
	# Write the menu specification in a file
	if (!is.null(path)) {	# Write this to a file
		if (path == "") path <- guiObjDir()
		MenuFile <- file.path(path, paste("Menu_", id, ".txt", sep=""))
		write.table(Menu, file = MenuFile, row.names = FALSE, quote = FALSE, sep = "\t")
		# Warn the client that the menu is ready by returning a command
        return(guiCmd(paste("<<<<Menu", id)))
	} else return(Menu) # If no path is specified, just return the data frame
}

