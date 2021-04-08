wget https://github.com/neovim/neovim/releases/download/v0.4.4/nvim.appimage
chmod +x nvim.appimage 
mv nvim.appimage /usr/local/bin/nvim
apt install pip fzf exuberant-ctags vimproc composer php-xml php-mbstring php-intl php-curl
pip install neovim
npm i -g typescript
ln -s /usr/local/share/node-v12.19.0-linux-x64/lib/node_modules/typescript/bin/tsserver /usr/local/bin/tsserver
ln -s /usr/local/share/node-v12.19.0-linux-x64/lib/node_modules/typescript/bin/tsc /usr/local/bin/tsc
ln -s /home/ctizen/.vim/plugged/phpactor/bin/phpactor /usr/local/bin/phpactor

