--- lua_source {{{
local insx = require("insx")
insx.preset.standard.setup()
-- auto html tags.
require("insx").add(
	">",
	require("insx.recipe.substitute")({
		pattern = [[<\(\w\+\).\{-}\%#]],
		replace = [[\0>\%#</\1>]],
	})
)
-- delete html tags.
require("insx").add(
	"<BS>",
	require("insx.recipe.substitute")({
		pattern = [[<\(\w\+\).\{-}>\%#</.\{-}>]],
		replace = [[\%#]],
	})
)
--- }}}
