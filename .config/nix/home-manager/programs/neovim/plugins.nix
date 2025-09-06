{pkgs sources}:
{
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
  }
