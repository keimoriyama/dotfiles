import {
  BaseConfig,
  ContextBuilder,
  Dpp,
  Plugin,
} from "https://deno.land/x/dpp_vim@v0.0.3/types.ts";
import { Denops, fn } from "https://deno.land/x/dpp_vim@v0.0.3/deps.ts";

type Toml = {
  hooks_file?: string;
  ftplugins?: Record<string, string>;
  plugins: Plugin[];
};

export class Config extends BaseConfig {
  override async config(args: {
    denops: Denops;
    contextBuilder: ContextBuilder;
    basePath: string;
    dpp: Dpp;
  }): Promise<{ plugins: Plugin[]; stateLines: string[] }> {
    args.contextBuilder.setGlobal({
      protocols: ["git"],
    });
    const [context, options] = await args.contextBuilder.get(args.denops);

    const tomls: Toml[] = [];
    tomls.push(
      await args.dpp.extAction(
        args.denops,
        context,
        options,
        "toml",
        "load",
        {
          path: "./dpp.toml",
          options: {
            lazy: false,
          },
        },
      ) as Toml,
    );

    const recordPlugins: Record<string, Plugin> = {};
    const ftplugins: Record<string, string> = {};
    const hooksFiles: string[] = [];
    for (const toml in tomls) {
      for (const plugin of toml.plugins) {
        recordPlugins[plugin.name] = plugin;
      }
      if (toml.ftplugins) {
        for (const filetype of Object.keys(toml.ftplugins)) {
          if (ftplugins[filetype]) {
            ftplugins[filetype] += `\n${toml.ftplugins[filetype]}`;
          } else {
            ftplugins[filetype] = toml.ftplugins[filetype];
          }
        }
      }
      if (toml.hooks_file) {
        hooksFiles.push(toml.hooks_file);
      }
    }
  }
}
