{
  pkgs,
  sources,
}: {
  programs.fish = {
    enable = true;
    plugins = [
      {
        name = "z";
        src = pkgs.fishPlugins.z.src;
      }
      {
        name = "git-abbr";
        src = pkgs.fishPlugins.git-abbr.src;
      }
      {
        name = "autols";
        src = sources.fish-autols.src;
      }
      {
        name = "ghq-fzf";
        src = sources.fish-ghq-fzf.src;
      }
      {
        name = "fish-cdf";
        src = sources.fish-cdf.src;
      }
      {
        name = "fish-fzf-bd";
        src = sources.fish-fzf-bd.src;
      }
      {
        name = "fish-gcd";
        src = sources.fish-gcd.src;
      }
    ];
    shellAliases = {
      c = "clear";
      n = "nvim";
      python = "python3";
    };
    functions = {
      fish_prompt = builtins.readFile ./fish_prompt.fish;
      fish_right_prompt = "
	printf \"%s\" (__fish_git_prompt)
	";
    };
    loginShellInit = builtins.readFile ./loginShellInit.fish;
    interactiveShellInit = builtins.readFile ./interactiveShellInit.fish;
  };
}
