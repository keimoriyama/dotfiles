import { Denops } from "./deps.ts";
import { main as file_main } from "./files.ts"
// import { Maze } from "https://deno.land/x/maze_generator@v0.4.0/mod.js";
export async function main(denops: Denops): void {
	file_main(denops)
}

// export async function main(denops: Denops): Promise<void> {
//   denops.dispatcher = {
//     async echo(text: unknown): Promise<unknown> {
//       ensureString(text);
//       return await Promise.resolve(text);
//     },
//     async maze(opener: unknown): Promise<unknown> {
//       ensureString(opener);
//       const [xSize, ySize] = (await denops.eval("[&columns,&lines]")) as [
//         number,
//         number,
//       ];
//       const maze = new Maze({ xSize: xSize / 3, ySize: ySize / 3 }).generate();
//       const content = maze.getString();
//       await denops.cmd(opener || "new");
//       await denops.call("setline", 1, content.split("\r?\n/g"));
//       await execute(
//         denops,
//         `
// 					setlocal bufhidden=wipe buftype=nofile
// 					setlocal nobackup noswapfile
// 					setlocal nomodified nomodifiable
// 					`,
//       );
//     },
//   };
//   await execute(
//     denops,
//     `command! -nargs=1 HelloWorldEcho echomsg denops#request('${denops.name}', 'echo', [<q-args>])`,
//   );
//   await denops.cmd(
//     `command! -nargs=? -bar Maze call denops#request('${denops.name}', 'maze', [<q-args>])`,
//   );
// }
