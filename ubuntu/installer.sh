# Simple script to get an ubuntu system up and running
# Tested on Ubuntu 16.04 LTS

echo "======== UBUNTU AUTO-INSTALL SCRIPT ========"
# Pre-setup
echo "Running pre-setup..."
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-add-repository -y "deb http://repository.spotify.com stable non-free"
sudo apt-add-repository -y ppa:richardgv/compton
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D2C19886
sudo apt-get -qq -y update
sudo apt-get -qq -y upgrade

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

# Languages
echo "Installing languages..."
sudo apt-get -qq -y install python-dev
sudo apt-get -qq -y install python-pip
sudo apt-get -qq -y install python3-dev
sudo apt-get -qq -y install python3-pip
curl -kL https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install | bash

# Editors
echo "Installing editors..."
sudo apt-get -qq -y install vim
sudo apt-get -qq -y install neovim

# Terminals
echo "Installing terminal..."
sudo apt-get -qq -y rxvt-unicode

# Shell
echo "Installing shells..."
sudo apt-get -qq -y install zsh

echo "Changing shell to zsh..."
chsh -s $(which zsh)

# Source Control
echo "Installing source control..."
sudo apt-get -qq -y install git

# Remove Unity (This has to be done here so we can reinstall what unity removes)
echo "Removing Unity..."
sudo apt-get -qq -y purge unity

# Multimedia
echo "Installing multimedia..."
sudo apt-get -qq -y install ffmpeg
sudo apt-get -qq -y install mplayer
sudo apt-get -qq -y install spotify-client

# Tools
echo "Installing tools..."
sudo apt-get -qq -y install transmission
sudo apt-get -qq -y install htop
sudo apt-get -qq -y install scrot
sudo apt-get -qq -y install tmux
sudo apt-get -qq -y install feh
sudo apt-get -qq -y install unclutter
sudo apt-get -qq -y install xdotool
sudo apt-get -qq -y install cmake
sudo apt-get -qq -y install gsimplecal
sudo apt-get -qq -y install xclip

# Comms
echo "Installing comms..."
sudo apt-get -qq -y install irssi
sudo apt-get -qq -y install skype

# Browsers
echo "Installing browsers..."
sudo apt-get -qq -y install chromium-browser

# Fonts
echo "Installing fonts..."
sudo apt-get -qq -y install fonts-inconsolata
fc-cache --really-force -v

# Security
echo "Installing security..."
sudo apt-get -qq -y install physlock

# Cleanup
echo "Cleaning up unneccessary programs..."
sudo apt-get -qq -y purge firefox
sudo apt-get -qq -y purge rhythmbox
sudo apt-get -qq -y purge cheese
sudo apt-get -qq -y purge shotwell
sudo apt-get -qq -y purge shotwell-common
sudo apt-get -qq -y purge thunderbird
sudo apt-get -qq -y purge aisleriot
sudo apt-get -qq -y purge webbrowser-app
sudo apt-get -qq -y purge gnome-sudoku
sudo apt-get -qq -y purge mahjongg
sudo apt-get -qq -y purge gnomine
sudo apt-get -qq -y purge ace-of-penguins
sudo apt-get -qq -y purge onboard
sudo apt-get -qq -y purge lightdm
sudo apt-get -qq -y purge gnome-terminal
sudo apt-get -qq -y purge gnome-terminal-data
sudo apt -qq -y autoremove

echo "!!! DONE INSTALLING !!!"
echo "======== REBOOTING IN 30 SECONDS ========"
sleep 30
sudo reboot
