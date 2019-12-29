#!/bin/sh
"installing homebrew..."
which brew >/dev/null 2>&1 || /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
echo "run brew doctor..."
which brew >/dev/null 2>&1 && brew doctor

echo "run brew update..."
which brew >/dev/null 2>&1 && brew update
brew upgrade 


echo "start brew install apps..."

brew bundle 

cat << END
**************************************************
HOMEBREW INSTALLED! bye.
**************************************************
END
