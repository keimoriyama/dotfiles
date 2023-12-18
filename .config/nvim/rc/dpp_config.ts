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
    const files = [
      ["dpp.toml", false],
      ["colorscheme.toml", false],
      ["utils.toml", true],
      ["ddc.toml", true],
      ["ddu.toml", true],
      ["lsp.toml", true],
      ["ftplugin.toml", true],
    ];
    const base_dir = "~/.config/nvim/rc/";
    for (const file of files) {
      tomls.push(
        await args.dpp.extAction(
          args.denops,
          context,
          options,
          "toml",
          "load",
          {
            path: base_dir + file[0],
            options: {
              lazy: file[1],
            },
          },
        ) as Toml,
      );
    }

    const recordPlugins: Record<string, Plugin> = {};
    const ftplugins: Record<string, string> = {};
    const hooksFiles: string[] = [];
    for (const toml of tomls) {
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
    const localPlugins = await args.dpp.extAction(
      args.denops,
      context,
      options,
      "local",
      "local",
      {
        directory: "~/Program/vim_plugins/",
        options: {
          frozen: true,
          merged: false,
        },
      },
    ) as Plugin[];
    localPlugins.forEach((plugin: Plugin) => {
      recordPlugins[plugin.name] = plugin;
    });
    const lazyResult = await args.dpp.extAction(
      args.denops,
      context,
      options,
      "lazy",
      "makeState",
      {
        plugins: Object.values(recordPlugins),
      },
    ) as LazyMakeStateResult;

    return {
      ftplugins,
      hooksFiles,
      plugins: lazyResult.plugins,
      stateLines: lazyResult.stateLines,
    };
  }
}
