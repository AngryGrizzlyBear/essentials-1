# Simple script to get an ubuntu system up and running
# Tested on Ubuntu 16.04 LTS

# Pre-setup
sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update
sudo apt-get upgrade

# Window Manager
sudo apt-get install xmonad
sudo apt-get install libghc-xmonad-contrib-dev

# Dmenu
sudo apt-get install suckless-tools

# Languages
sudo apt-get install python-dev 
sudo apt-get install python-pip 
sudo apt-get install python3-dev 
sudo apt-get install python3-pip

# Editors
sudo apt-get install vim
sudo apt-get install neovim

# Tools
sudo apt-get install htop
sudo apt-get install chromium-browser

# Cleanup
sudo apt-get purge firefox
sudo apt-get purge rhythmbox
sudo apt-get purge cheese
sudo apt-get purge shotwell
sudo apt-get purge shotwell-common
sudo apt-get purge thunderbird
sudo apt-get purge aisleriot
sudo apt-get purge webbrowser-app
sudo apt-get purge gnome-sudoku
sudo apt-get purge mahjongg
sudo apt-get purge gnomine
sudo apt-get purge ace-of-penguins
sudo apt-get purge onboard
