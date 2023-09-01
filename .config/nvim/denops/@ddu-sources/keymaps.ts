import { BaseSource, DduOptions, Item, SourceOptions } from "https://deno.land/x/ddu_vim@v3.6.0/types.ts";
import { Denops, fn } from "https://deno.land/x/ddu_vim@v3.6.0/deps.ts";
import { ActionData } from "https://deno.land/x/ddu_kind_file@v0.3.2/file.ts";
import { nvim_get_keymap } from "https://deno.land/x/denops_std@v5.0.1/function/nvim/mod.ts";

type Params = Record<never, never>

export class Source extends BaseSource<Params> {
	override kind = "keymaps"

	override gather(args: {
		denops: Denops; options: DduOptions; sourceOptions: SourceOptions; sourceParams: Params; iput: string
	}): ReadableStream<Item<ActionData>[]> {
		return new ReadableStream({
			async start(controller) {

				const keymapItems = async () => {
					const modes = ['v', 'n', 'i', 't']
					let items: Item<ActionData>[] = []
					for (const m of modes) {
						// コマンド毎の一覧を取得
						const keymaps = await neovim_get_keymaps(args.denops, m)
						items = items.concat(keymaps)
					}
					return items
				}
				controller.enqueue(
					await keymapItems()
				)
				controller.close()
			}
		})
	}
	override params(): Params {
		return {}
	}
}

async function neovim_get_keymaps(denops: Denops, mode: string): Promise<Item<ActionData>[]> {
	let keymaps = await nvim_get_keymap(denops, mode);
	let items: Item<ActionData>[] = [];
	for (const keymap of keymaps) {
		let words = ""
		words = keymap.mode + " " + keymap.lhs + " " + keymap.rhs + " " + keymap.desc
		items.push({
			word: words
		})
	}
	return items
}
