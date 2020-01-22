ln -sf ~/dotfiles/fish ~/.config/fish
ln -sf ~/dotfiles/.zpreztorc ~/.zpreztorc
ln -sf ~/dotfiles/tmux-powerline ~/tmux-powerline
ln -sf ~/dotfiles/zprezto ~/zprezto


DOTPATH=~/dotfiles

GITHUB_URL=https://github.com/keimoriyama/dotfiles

if [ "$(uname)" == 'Darwin' ]; then
  #プロンプトをechoを使って表示
  echo -n Do you want to install Brewfile?:
  #入力を受付、その入力を「str」に代入
  read str
  #結果を表示
  echo $str
    if [str == 'y'];then
      source ./bin/brewInstall.sh
    fi
fi

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

# 移動できたらリンクを実行する
for f in .??*
do
    [ "$f" = ".git" ] && continue

    ln -snfv "$DOTPATH/$f" "$HOME"/"$f"
done
