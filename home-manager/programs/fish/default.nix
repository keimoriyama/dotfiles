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
      # g = "git";
      # ga = "git add";
      # gd = "git diff";
      # gp = "git push";
      # gpo = "git push origin";
      # gb = "git branch";
      # gs = "git status";
      # gco = "git checkout";
      # gf = "git fetch";
      # gc = "git commit";
      c = "clear";
      n = "nvim";
      python = "python3";
    };
    functions = {
      fish_prompt = "
    echo -n (set_color cyan)(prompt_pwd) (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '
";
      fish_right_prompt = "
	printf \"%s\" (__fish_git_prompt)
	";
    };
    loginShellInit = "
set -x PATH \"/nix/var/nix/profiles/default/bin\" \"$PATH\"
set -x PATH \"/run/current-system/sw/bin/\" \"$PATH\"
set -x PATH \"$HOME/.nix-profile/bin\" \"$PATH\"
set -x PATH \"$PATH\" \"/opt/homebrew/bin\"
set -x PATH \"$PATH\" \"$HOME/.cargo/bin\" 
set -x PATH \"$PATH\" \"$HOME/.local/bin\"
set -x PATH \"$PATH\" \"$HOME/.roswell/bin/\"
# Fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind '-'

if test -f $HOME/.local/mocword-data/mocword.sqlite 
	echo 'mocword data is installed'
else
	mkdir $HOME/.local/mocword-data/
	curl -OL --output-dir $HOME/.local/mocword-data/ https://github.com/high-moctane/mocword-data/releases/download/eng20200217/mocword.sqlite.gz
	gzip -d $HOME/.local/mocword-data/mocword.sqlite.gz 
end
";
    interactiveShellInit = "
set -gx MOCWORD_DATA $HOME/.local/mocword-data/mocword.sqlite
set -gx DPP_PATH $HOME/.cache/dpp
set -gx HYDRA_FULL_ERROR 1
set -gx NIX_USER_CONF_FILES $XDG_CONFIG_HOME/nix/nix.conf:$$XDG_CONFIG_HOME/nix/local.conf;
set -gx TYPST_FONT_PATHS $HOME/Library/Application Support/Adobe/.User Owned Fonts/:$HOME/Library/Application Support/Adobe/CoreSync/plugins/livetype/.r/
";
  };
}
