# Simple script to get an ubuntu system up and running
# Tested on Ubuntu 16.04 LTS

echo "======== UBUNTU AUTO-INSTALL SCRIPT ========"
# Pre-setup
echo "Running pre-setup..."
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get -y update
sudo apt-get -y upgrade

# Window Manager
echo "Installing window manager..."
sudo apt-get -y install xmonad
sudo apt-get -y install libghc-xmonad-contrib-dev

# Dmenu
echo "Installing Dmenu..."
sudo apt-get -y install suckless-tools

# Languages
echo "Installing languages..."
sudo apt-get -y install python-dev 
sudo apt-get -y install python-pip 
sudo apt-get -y install python3-dev 
sudo apt-get -y install python3-pip

# Editors
echo "Installing editors..."
sudo apt-get -y install vim
sudo apt-get -y install neovim

# Source Control
echo "Installing source control..."
sudo apt-get -y install git

# Remove Unity (This has to be done here so we can reinstall what unity removes)
echo "Removing Unity..."
sudo apt-get -y purge unity

# Tools
echo "Installing tools..."
sudo apt-get -y install transmission
sudo apt-get -y install htop
sudo apt-get -y install chromium-browser

# Cleanup
echo "Cleaning up unneccessary programs..."
sudo apt-get -y purge firefox
sudo apt-get -y purge rhythmbox
sudo apt-get -y purge cheese
sudo apt-get -y purge shotwell
sudo apt-get -y purge shotwell-common
sudo apt-get -y purge thunderbird
sudo apt-get -y purge aisleriot
sudo apt-get -y purge webbrowser-app
sudo apt-get -y purge gnome-sudoku
sudo apt-get -y purge mahjongg
sudo apt-get -y purge gnomine
sudo apt-get -y purge ace-of-penguins
sudo apt-get -y purge onboard
sudo apt -y autoremove

echo "!!! DONE INSTALLING !!!"
echo "======== REBOOTING IN 30 SECONDS ========"
sleep 30
sudo reboot
