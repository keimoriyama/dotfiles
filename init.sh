export PLATFORM

DOTPATH=~/.dotfiles

echo Do you want to deploy?Y/N:
read ans
if [ $ans == 'y' ];then

  # git が使えるなら git
  if type "git";then
      git clone --recursive "$GITHUB_URL" "$DOTPATH"

  # 使えない場合は curl か wget を使用する
  elif type "curl" || type "wget"; then
      tarball="https://github.com/keimoriyama/.dotfiles/archive/master.tar.gz"

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

  cd ~/.dotfiles
  if [ $? -ne 0 ]; then
      die "not found: $DOTPATH"
  fi
fi
# 手動でリンクを貼る
ln -sf ~/.dotfiles/.config/ $HOME
# 移動できたらリンクを実行する
for f in .??*
do
    [ "$f" = ".git" ] && continue

    ln -snfv "$DOTPATH/$f" "$HOME"/"$f"
done
