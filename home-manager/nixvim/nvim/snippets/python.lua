return {
	{
		prefix = "ip",
		body = "import ipdb; ipdb.set_trace()",
		desc = "ipdb breakpoint",
	},
	{
		prefix = "range",
		body = {
			"for ${1:index} in range(${2:range}):",
			"\t{$0}",
		},
		desc = "for range loop",
	},
}
