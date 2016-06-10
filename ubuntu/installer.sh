# Simple script to get an ubuntu system up and running
# Tested on Ubuntu 16.04 LTS

echo "======== UBUNTU AUTO-INSTALL SCRIPT ========"
# Pre-setup
echo "Running pre-setup..."
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-add-repository -y "deb http://repository.spotify.com stable non-free"
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D2C19886
sudo apt-get -qq -y update
sudo apt-get -qq -y upgrade

# Window Manager
echo "Installing window manager..."
sudo apt-get -qq -y install xmonad
sudo apt-get -qq -y install libghc-xmonad-contrib-dev

# Dmenu
echo "Installing Dmenu..."
sudo apt-get -qq -y install suckless-tools

# Languages
echo "Installing languages..."
sudo apt-get -qq -y install python-dev 
sudo apt-get -qq -y install python-pip 
sudo apt-get -qq -y install python3-dev 
sudo apt-get -qq -y install python3-pip

# Editors
echo "Installing editors..."
sudo apt-get -qq -y install vim
sudo apt-get -qq -y install neovim

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

# Browsers
echo "Installing browsers..."
sudo apt-get -qq -y install chromium-browser

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
sudo apt -qq -y autoremove

echo "!!! DONE INSTALLING !!!"
echo "======== REBOOTING IN 30 SECONDS ========"
sleep 30
sudo reboot
