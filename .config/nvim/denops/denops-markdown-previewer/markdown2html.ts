import { CSS, KATEX_CSS, render } from "jsr:@deno/gfm";
// import { Denops } from "jsr:@denops/std@^7.0.0";

export function markdown2html(markdown: string) {
  console.log(markdown);
  const body = render(markdown);
  return `
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
      main {
        max-width: 800px;
        margin: 0 auto;
      }
      ${CSS}
    </style>
  </head>
  <body>
    <main data-color-mode="light" data-light-theme="light" data-dark-theme="dark" class="markdown-body">
      ${body}
    </main>
  </body>
</html>
`;
}
