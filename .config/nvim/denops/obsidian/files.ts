import { Denops } from "./deps.ts"
import { ensure, is } from "./deps.ts"
import { open } from "./deps.ts"
import { execute } from "./deps.ts"

export async function main(denops: Denops): Promise<void> {
	denops.dispatcher = {
		open_file(path: unknown) {
			const path2file = ensure(path, is.String);
			open(denops, path2file)
		}
	}
	await execute(
		denops,
		`command! -nargs=1 Test echomsg denops#request('${denops.name}', 'open_file', [<q-args>])`,
	)
}
