{pkgs, ...}: {
  programs.nixvim = {
    opts = {
      fileencodings = "utf-8,iso-2022-jp,euc-jp,sjis";
      relativenumber = true;
      hlsearch = true;
      incsearch = true;
      # laststatus = 0;
      statusline = "─";
      wildmenu = false;
      modeline = false;
      autoindent = true;
      autoread = true;
      swapfile = false;
      backup = false;
      writebackup = false;
      tabstop = 4;
      shiftwidth = 4;
      modifiable = true;
      clipboard =
        if pkgs.stdenv.isDarwin
        then "unnamedplus,unnamed"
        else "unnamedplus";
      splitright = true;
      # cmdheight = 0;
      scrolloff = 0;
      cursorline = true;
      cursorcolumn = true;
      mouse = "";
    };
  };
}
