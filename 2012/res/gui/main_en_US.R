mlist <- list(
'File' = list(
a_NEW_FB_ = gaction(label = 'New fieldbook form(s)', icon='new', handler=function(h,...) m.new(w), parent=w) ,
sep = list(separator = TRUE) ,
a_CHECK_FB_ = gaction(label = 'Check fieldbook', icon='open', handler=function(h,...) m.check(w), parent=w) ,
a_OPEN_FB_ = gaction(label = 'Open fieldbook', icon='open', handler=function(h,...) m.open(w), parent=w) ,
sep = list(separator = TRUE) ,
a_COMB_FB_ = gaction(label = 'Combine fieldbook', icon='', handler=function(h,...) m.comb(w), parent=w) ,
a_OPEN_COMB_FB_ = gaction(label = 'Open combined fieldbook', icon='', handler=function(h,...) m.open.comb(w), parent=w) ,
sep = list(separator = TRUE) ,
a_SET_REPO_ = gaction(label = 'Set repository', icon='', handler=function(h,...) m.set.repo(w), parent=w) ,
sep = list(separator = TRUE) ,
a_QUIT_ = gaction(label = 'Quit', icon='quit', handler=function(h,...) m.quit(w), parent=w)
),
'Edit' = list(
a_UPDATE_TEMPLATE_ = gaction(label = 'Update template', icon='', handler=function(h,...) m.update.template(w), parent=w)
),
'Analysis' = list(
a_ANOVA_ = gaction(label = 'ANOVA', icon='', handler=function(h,...) m.anova(w), parent=w) ,
a_MET_ = gaction(label = 'MET (Multi-environment trial)', icon='', handler=function(h,...) m.met(w), parent=w)
),
'Preferences' = list(
a_PREF_GENERAL_ = gaction(label = 'General', icon='', handler=function(h,...) m.pref.general(w), parent=w) ,
a_PREF_FB_VARS_ = gaction(label = 'Fieldbook variables', icon='', handler=function(h,...) m.pref.fb.vars(w), parent=w) ,
sep = list(separator = TRUE) ,
a_PREF_LOCS_ = gaction(label = 'Localities', icon='', handler=function(h,...) m.pref.locs(w), parent=w)
),
'Tools' = list(
a_TOOLS_RSSE_ = gaction(label = 'Response to selection for a single experiment', icon='', handler=function(h,...) m.tools.rsse(w), parent=w) ,
a_TOOLS_RSSL_ = gaction(label = 'Response to selection with several locations', icon='', handler=function(h,...) m.tools.rssl(w), parent=w) ,
a_TOOLS_RSSLY_ = gaction(label = 'Response to selection with several locations and years', icon='', handler=function(h,...) m.tools.rssly(w), parent=w) ,
a_TOOLS_RSSLY2_ = gaction(label = 'Response to selection with several locations in two steps', icon='', handler=function(h,...) m.tools.rssly2(w), parent=w)
),
'Window' = list(
a_WINDOW_SYNC_ = gaction(label = 'Refresh summary table view', icon='', handler=function(h,...) m.win.sync(w), parent=w)
),
'Help' = list(
a_HELP_MANUAL_ = gaction(label = 'Manual', icon='', handler=function(h,...) m.help.man(w), parent=w) ,
a_HELP_PROTOCOL_ = gaction(label = 'Protocols', icon='', handler=function(h,...) m.help.prot(w), parent=w) ,
a_HELP_SELECT_ = gaction(label = 'On selection indices', icon='', handler=function(h,...) m.help.select(w), parent=w) ,
a_HELP_ELSTON_ = gaction(label = 'About Elston index', icon='', handler=function(h,...) m.help.elston(w), parent=w) ,
a_HELP_RANKS_ = gaction(label = 'How ranks are calculated', icon='', handler=function(h,...) m.help.ranks(w), parent=w) ,
sep = list(separator = TRUE) ,
a_HELP_MAN_OL_ = gaction(label = 'Go to online manual', icon='', handler=function(h,...) m.help.manol(w), parent=w) ,
a_HELP_WEB_ = gaction(label = 'Go to web site', icon='', handler=function(h,...) m.help.web(w), parent=w) ,
a_HELP_FEEDBK_ = gaction(label = 'Give feedback', icon='', handler=function(h,...) m.help.feedbk(w), parent=w) ,
sep = list(separator = TRUE) ,
a_HELP_ABOUT_ = gaction(label = 'About Data Collector', icon='', handler=function(h,...) m.help.about(w), parent=w)
)
)
