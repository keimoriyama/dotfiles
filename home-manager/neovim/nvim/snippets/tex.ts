import { TSSnippet } from "https://deno.land/x/denippet_vim@v0.5.1/loader.ts";
import { Denops } from "jsr:@denops/std@^7.0.0";

export const snippets: Record<string, TSSnippet> = {
  // ファイル名を用いてcaptionとlabelを貼るスニペット
  "prompt_fig": {
    prefix: "prompt_fig",
    body: async (denops: Denops) => {
      const fileName = (await denops.call("expand", "%:t") as string).replace(
        ".tex",
        "",
      );
      return [
        "\\begin{figure}[ht]",
        "\t\\centering",
        "\t\fbox{",
        "\t\t\\parbox{0.9\linewidth}{",
        "$0",
        "\t\t}",
        "\t}",
        `\t\\caption{$1}`,
        `\t\\label{fig:${fileName}}`,
        "\\end{figure}",
      ];
    },
  },
  "figure": {
    prefix: "figure",
    body: async (denops: Denops) => {
      const fileName = (await denops.call("expand", "%:t") as string).replace(
        ".tex",
        "",
      );
      return [
        "\\begin{figure}[ht]",
        "\t\\centering",
        "\t\\includegraphics[width=\\linewidth]{$1}",
        "\t\\caption{$2}",
        `\t\\label{fig:${fileName}}`,
        "\\end{figure}",
      ];
    },
  },
  "table": {
    prefix: "table",
    body: async (denops: Denops) => {
      const fileName = (await denops.call("expand", "%:t") as string).replace(
        ".tex",
        "",
      );
      return [
        "\\begin{table}[ht]",
        "\t\\centering",
        "\t\\begin{tabular}{$1}",
        "\t\t$0",
        "\t\\end{tabular}",
        `\t\\caption{$2}`,
        `\t\\label{tab:${fileName}}`,
        "\\end{table}",
      ];
    },
  },
};
