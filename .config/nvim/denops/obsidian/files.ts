import { Denops, open, execute, globals, join, format } from "./deps.ts"

export async function main(denops: Denops): Promise<void> {
	denops.dispatcher = {
		async open_file(filename: unknown) {
			const baseDir = await globals.get(denops, "base_dir");
			const path2file = join(baseDir, filename);
			open(denops, path2file)
		},
		async create_today() {
			const baseDir = await globals.get(denops, "base_dir");
			const filename = await gen_date_str();
			const path2file = join(baseDir, filename);
			open(denops, path2file)
		}
	}
	await execute(
		denops,
		`command! -nargs=1 Test call denops#request('${denops.name}', 'open_file', [<q-args>])`,
	)
	await execute(
		denops,
		`command! TestToday call denops#request('${denops.name}', 'create_today', [])`,
	)
}

async function gen_date_str(): Promise<string> {
	const d = new Date();
	const filename = format(d, "yyyy-MM-dd") + ".md";
	return filename;
}
