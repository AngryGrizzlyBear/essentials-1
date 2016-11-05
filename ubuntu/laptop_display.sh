# Window Manager
echo "Installing window manager..."
sudo apt-get -qq -y install xmonad
sudo apt-get -qq -y install compton
sudo apt-get -qq -y install libghc-xmonad-contrib-dev
sudo apt-get -qq -y install xorg
sudo apt-get -qq -y install xinit
sudo apt-get -qq -y install wmname
sudo apt-get -qq -y install libxcb-xinerama0-dev
sudo apt-get -qq -y install libxcb-randr0-dev
sudo apt-get -qq -y install xbacklight

# Build and install lemonbar from source
echo "Installing lemonbar from source..."
pushd
cd ~
git clone https://github.com/LemonBoy/bar.git
cd bar/
make
sudo make install
popd

# Dmenu
echo "Installing Dmenu..."
sudo apt-get -qq -y install suckless-tools
