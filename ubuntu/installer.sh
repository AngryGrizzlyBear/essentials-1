# Simple script to get an ubuntu system up and running
# Tested on Ubuntu 16.04 LTS

echo "======== UBUNTU AUTO-INSTALL SCRIPT ========"

echo "Checking for internet connection..."
nc -z 8.8.8.8 53  >/dev/null 2>&1
online=$?
if [ $online -ne 0 ]; then
  echo "No internet connection detected! Please enable an internet connection before continuing."
  exit
fi

# Pre-setup
./pre_setup.sh

# Laptop Display
if [[ $1 == "laptop" ]]
then
./laptop_display.sh
else
./desktop_display.sh
fi

# Software
./software_and_fonts.sh

# Terminals
echo "Installing terminal..."
if [[ $1 == "laptop" ]]
then
sudo apt-get -qq -y rxvt-unicode
else
sudo apt-get -qq -y tilda
fi

# Shell
./shell.sh


if [[ $1 == "laptop" ]]
then
echo "Removing Unity..."
sudo apt-get -qq -y purge unity
fi


if [[ $1 == "laptop"]]
then 
sudo apt-get -qq -y install feh
fi

./cleanup_trash

if [[$1 == "laptop"]]
then
sudo apt-get -qq -y purge lightdm
sudo apt-get -qq -y purge gnome-terminal
sudo apt-get -qq -y purge gnome-terminal-data
fi

# Right here is where the sed one-line would go to replace
# "quiet splash" with "text" in the grub config and then call
# sudo update grub. This eliminates Ubuntu getting stuck trying
# to boot a login screen that doesn't exist by dropping you
# straight into a tty.

echo "!!! DONE INSTALLING !!!"
echo "======== REBOOTING IN 30 SECONDS ========"
sleep 30
sudo reboot
