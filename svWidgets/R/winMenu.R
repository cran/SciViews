"winMenuChangeItem" <-
function(menu, item, action, options = "") {
    # Note: this is buggy under R 2.2.0!
	if (action == "") return(invisible())
	require(utils)
	if (!isRgui())
		stop("This function can only be used with Rgui under Windows!")
	# First check if the entry exists
	if (!menu %in% winMenuNames())
		stop("menu '", menu, "' does not exist!")
	if (!item %in%  names(winMenuItems(menu)))
		stop("item '", item, "' does not exist!")
	if (action == "enable" || action == "disable")
		stop("Use 'winMenuStateItem()' instead to enable/disable winMenu entries!")
	# The only change possible for winMenus is action (use winMenuStateItem() to enable/disable)
	winMenuAddItem(menu, item, action)
}

"winMenuStateItem" <-
function(menu, item, active = TRUE) {
	require(utils)
    if (!isRgui())
		stop("This function can only be used with Rgui under Windows!")
	# First check if the entry exists
	if (!menu %in% winMenuNames())
		stop("menu '", menu, "' does not exist!")
	if (!item %in%  names(winMenuItems(menu)))
		stop("item '", item, "' does not exist!")
	# Enable/disable the entry
	cmd <- if(active) "enable" else "disable"
	winMenuAddItem(menu, item, cmd)
}
