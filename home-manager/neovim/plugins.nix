{
  pkgs,
  sources,
}: {
  dpp-vim = pkgs.vimUtils.buildVimPlugin {
    pname = "dpp-vim";
    src = sources.dpp-vim.src;
    version = sources.dpp-vim.version;
    dontBuild = true;
  };
  dpp-ext-toml = pkgs.vimUtils.buildVimPlugin {
    pname = "dpp-ext-toml";
    src = sources.dpp-ext-toml.src;
    version = sources.dpp-ext-toml.version;
    dontBuild = true;
  };
  dpp-ext-lazy = pkgs.vimUtils.buildVimPlugin {
    pname = "dpp-ext-lazy";
    src = sources.dpp-ext-lazy.src;
    version = sources.dpp-ext-lazy.version;
    dontBuild = true;
  };
  dpp-ext-installer = pkgs.vimUtils.buildVimPlugin {
    pname = "dpp-ext-installer";
    src = sources.dpp-ext-installer.src;
    version = sources.dpp-ext-installer.version;
    dontBuild = true;
  };
  dpp-protocol-git = pkgs.vimUtils.buildVimPlugin {
    pname = "dpp-protocol-git";
    src = sources.dpp-protocol-git.src;
    version = sources.dpp-protocol-git.version;
    dontBuild = true;
  };
}
