import {
  BaseConfig,
  ContextBuilder,
  Dpp,
  Plugin,
} from "https://deno.land/x/dpp_vim@v0.0.3/types.ts";
import { Denops, fn } from "https://deno.land/x/dpp_vim@v0.0.3/deps.ts";
import { getcwd } from "https://deno.land/x/denops_std@v5.0.1/function/mod.ts";

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
    const base_dir = await getcwd(args.denops);
    const toml_path = base_dir + "/rc/dpp.toml";
    tomls.push(
      await args.dpp.extAction(
        args.denops,
        context,
        options,
        "toml",
        "load",
        {
          path: toml_path,
          options: {
            lazy: false,
          },
        },
      ) as Toml,
    );

    const recordPlugins: Record<string, Plugin> = {};
    const ftplugins: Record<string, string> = {};
    const hooksFiles: string[] = [];
    for (const toml of tomls) {
      console.log(toml);
    }
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
      checkFiles: await fn.globpath(
        args.denops,
        base_dir + "/rc",
        "*",
        1,
        1,
      ) as unknown as string[],
      ftplugins,
      hooksFiles,
      plugins: lazyResult.plugins,
      stateLines: lazyResult.stateLines,
    };
  }
}
