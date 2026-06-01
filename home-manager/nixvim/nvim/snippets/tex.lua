return function()
	local file_name = vim.fn.expand("%:t"):gsub("%.tex$", "")

	return {
		{
			prefix = "begin",
			body = {
				"\\begin{$1}",
				"\t$0",
				"\\end{$1}",
			},
			desc = "generic environment",
		},
		{
			prefix = "item",
			body = {
				"\\begin{itemize}",
				"\t\\item $0",
				"\\end{itemize}",
			},
			desc = "itemize environment",
		},
		{
			prefix = "enu",
			body = {
				"\\begin{enumerate}",
				"\t\\item $0",
				"\\end{enumerate}",
			},
			desc = "enumerate environment",
		},
		{
			prefix = "de",
			body = {
				"\\begin{description}",
				"\t\\item[$1] $0",
				"\\end{description}",
			},
			desc = "description environment",
		},
		{
			prefix = "fr",
			body = {
				"\\begin{frame}[t]{$1}",
				"\t$0",
				"\\end{frame}",
			},
			desc = "beamer frame",
		},
		{
			prefix = "sec",
			body = "\\section{$1}",
			desc = "section",
		},
		{
			prefix = "br",
			body = {
				"\\begin{block}{$1}",
				"\t$0",
				"\\end{block}",
			},
			desc = "beamer block",
		},
		{
			prefix = "fnurl",
			body = "\\footnote{\\url{$1}}",
			desc = "footnote url",
		},
		{
			prefix = "prompt_fig",
			body = {
				"\\begin{figure}[ht]",
				"\t\\centering",
				"\t\\fbox{",
				"\t\t\\parbox{0.9\\linewidth}{",
				"\t\t\t$0",
				"\t\t}",
				"\t}",
				"\t\\caption{$1}",
				"\t\\label{fig:" .. file_name .. "}",
				"\\end{figure}",
			},
			desc = "figure with prompt",
		},
		{
			prefix = "figure",
			body = {
				"\\begin{figure}[ht]",
				"\t\\centering",
				"\t\\includegraphics[width=\\linewidth]{$1}",
				"\t\\caption{$2}",
				"\t\\label{fig:" .. file_name .. "}",
				"\\end{figure}",
			},
			desc = "figure",
		},
		{
			prefix = "table",
			body = {
				"\\begin{table}[ht]",
				"\t\\centering",
				"\t\\begin{tabular}{$1}",
				"\t\t$0",
				"\t\\end{tabular}",
				"\t\\caption{$2}",
				"\t\\label{tab:" .. file_name .. "}",
				"\\end{table}",
			},
			desc = "table",
		},
	}
end
