import { Denops, ensure, is, open, execute, globals, join } from "./deps.ts"

export async function main(denops: Denops): Promise<void> {
	denops.dispatcher = {
		async open_file(path: unknown) {
			const baseDir = globals.get(denops, "base_dir");
			const filename = ensure(path, is.String);
			console.log(baseDir, filename)
			const path2file = join(baseDir, filename);

			open(denops, path2file)
		}
	}
	await execute(
		denops,
		`command! -nargs=1 Test echomsg denops#request('${denops.name}', 'open_file', [<q-args>])`,
	)
}
