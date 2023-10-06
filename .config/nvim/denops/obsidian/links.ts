import { Denops, globals, getcurpos, getline, execute, Position, walk, expand, open } from './deps.ts'

export async function main(denops: Denops): Promise<void> {
	denops.dispatcher = {
		async follow_link() {
			// [[]]で囲まれたカーソル下の文字列を取得
			const cursor_pos: Position = await getcurpos(denops);
			const lnum: number = cursor_pos[1]
			const col: number = cursor_pos[2]
			const str_under_cursor: string = await getline(denops, lnum)
			const begin_parenthesis_pos: number = str_under_cursor.indexOf("[[")
			const end_parrenthesis_pos: number = str_under_cursor.lastIndexOf("]]")
			console.log(begin_parenthesis_pos, end_parrenthesis_pos, col)
			if (begin_parenthesis_pos == -1 || end_parrenthesis_pos == -1) {
				console.log("cursor is not under the link for the note")
				return
			}
			if (begin_parenthesis_pos < col || col < end_parrenthesis_pos) {
				const baseDir: string = await expand(denops, await globals.get(denops, "base_dir"));
				const file_ailias: string = str_under_cursor.slice(begin_parenthesis_pos + 2, end_parrenthesis_pos)
				// ファイル名の確定
				const filename: string = file_ailias.slice(0, file_ailias.indexOf("|")) + ".md"
				// 絶対パスで指定する
				let file_paths: strings[] = []
				for await (const files of walk(baseDir, { includeDirs: false, match: [filename] })) {
					file_paths.push(files)
				}
				if (file_paths.length == 1) {
					open(denops, file_paths[0].path)
				}
			}
		}
	}
	await execute(
		denops,
		`command! Test call denops#request('${denops.name}', 'follow_link', [])`,
	)
}
