DOTPATH=~/dotfiles

GITHUB_URL=https://github.com/keimoriyama/dotfiles

if [ "$(uname)" == 'Darwin' ]; then
  #プロンプトをechoを使って表示
  echo Do you want to install Brewfile?Y/N:
  #入力を受付、その入力を「str」に代入
  read str
  #結果を表示
    if [ $str == 'y' ];then
      cd ~/dotfiles
      source ./bin/brewInstall.sh
    fi
fi

if [ "$(uname)" == 'Linux' ]; then
  #プロンプトをechoを使って表示
  echo Do you want to install ?Y/N:
  #入力を受付、その入力を「str」に代入
  read str
  #結果を表示
    if [ $str == 'y' ];then
      cd ~/dotfiles
      source ./bin/ubuntuInstall.sh
    fi
fi

echo Do you want to deploy?Y/N:
read ans
if [ $ans == 'y' ];then

  # git が使えるなら git
  if type "git";then
      git clone --recursive "$GITHUB_URL" "$DOTPATH"

  # 使えない場合は curl か wget を使用する
  elif type "curl" || type "wget"; then
      tarball="https://github.com/keimoriyama/dotfiles/archive/master.tar.gz"

      # どっちかでダウンロードして，tar に流す
      if type "curl"; then
          curl -L "$tarball"

      elif type "wget"; then
          wget -O - "$tarball"

      fi | tar zxv

      # 解凍したら，DOTPATH に置く
      mv -f dotfiles-master "$DOTPATH"

  else
      die "curl or wget required"
  fi

  cd ~/dotfiles
  if [ $? -ne 0 ]; then
      die "not found: $DOTPATH"
  fi
fi
# 手動でリンクを貼る
ln -sf ~/dotfiles/fish ~/.config/fish
ln -sf ~/dotfiles/karabiner/ ~/.config/karabiner
# 移動できたらリンクを実行する
for f in .??*
do
    [ "$f" = ".git" ] && continue

    ln -snfv "$DOTPATH/$f" "$HOME"/"$f"
done
# change shell
chsh -s $(which zsh)
echo Do you want to init .zprezto ?Y/N:
read str
if [ $str == 'y' ];then
  setopt EXTENDED_GLOB
  for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
  done
fi

#プロンプトをechoを使って表示
echo Do you want to install nerdfont ?Y/N:
#入力を受付、その入力を「str」に代入
read str
#結果を表示
  if [ $str == 'y' ];then

    #insatll nerd font
    git clone --branch=master --depth 1 https://github.com/ryanoasis/nerd-fonts.git
    cd nerd-fonts
    ./install.sh   # "Source" to install Sauce Code Nerd Font
    cd ..
    rm -rf nerd-fonts
  fi

  # tpmのインストール
 git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

echo Do you want to intall Rust? Y/N:
read str
if [ $str == 'y' ];then
  curl https://sh.rustup.rs -sSf | s://sh.rustup.rs -sSf | sh
  cargo install exa
fi
