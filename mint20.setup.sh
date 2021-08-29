cd ~

apt remove vim-tiny

curl -sL https://deb.nodesource.com/setup_14.x -o nodesource_setup.sh
bash nodesource_setup.sh
apt install -y git tmux mc zsh kitty xclip nodejs php-cli rofi flameshot compton vim playerctl telegram-desktop spotify-client xfonts-terminus xbacklight xpra feh


apt install -y cmake cmake-data libcairo2-dev libxcb1-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-image0-dev libxcb-randr0-dev libxcb-util0-dev libxcb-xkb-dev pkg-config python3-xcbgen xcb-proto libxcb-xrm-dev i3-wm libasound2-dev libmpdclient-dev libiw-dev libcurl4-openssl-dev libpulse-dev clang python3-sphinx libtool

apt install -y libxcb-composite0-dev libjsoncpp-dev
ln -s /usr/include/jsoncpp/json/ /usr/include/json
git clone https://github.com/jaagr/polybar.git
cd polybar && ./build.sh
cd ..

apt install -y g++ libgtk-3-dev gtk-doc-tools gnutls-bin valac intltool libpcre2-dev libglib3.0-cil-dev libgnutls28-dev libgirepository1.0-dev libxml2-utils gperf build-essential

git clone https://github.com/jwiegley/git-scripts.git
cp -R git-scripts/* /usr/local/bin/

git clone https://github.com/noctuid/zscroll
cd zscroll
python3 setup.py install
cd ..
