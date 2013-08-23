###############################################################################
#
# TODO: Add comment
#
# Apr 4, 2012
# 4:21:45 AM
# Author: Reinhard Simon (user)
# (c) International Potato Center
#
###############################################################################

addIssue <- function(title, summary="", 
		type="task", assignee="rsimon",component="Testing", 
		project="GTDM", version="1.1.4"){
	cmd = paste("bin/ac-2.1.0/jira --action createIssue --project \"",project,"\" --assignee \"",assignee, 
		"\" --version \"",version,"\" --type \"",type,
		"\" --summary \"",title,"\" --description \"",summary,
		"\" --components \"",component,"\" --fixVersions \"",version,"\" --affectsVersions \"",version,
		"\"",sep="")

    system(cmd)
}
