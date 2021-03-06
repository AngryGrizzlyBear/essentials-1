Y#!/bin/bash
#
# An almost completely untested automated package install script for new installs.
#
# This file is relatively untested and stuff is generally added to it mostly
# for the sake of remembering what commands to run. This may not work
# as expected (since I only ever use it when I have a fresh install) and you
# should look over the code yourself before running.
#

function install_yaourt {
  sudo pacman -S base-devel yajl &
  mkdir -p ~/temp/AUR/ && cd ~/temp/AUR/

  wget https://aur.archlinux.org/cgit/aur.git/snapshot/package-query.tar.gz
  tar xfz package-query.tar.gz
  cd package-query && makepkg
  sudo pacman -U package-query*.pkg.tar.xz

  wget https://aur.archlinux.org/cgit/aur.git/snapshot/yaourt.tar.gz
  tar xfz yaourt.tar.gz
  cd yaourt && makepkg
  sudo pacman -U yaourt*.pkg.tar.xz
}

# Make sure system is up to date
sudo pacman -Syu


# Install yaourt before doing anything else
install_yaourt

# Add infinality-bundle to pacman.conf
echo "" >> /etc/pacman.conf
echo "[infinality-bundle]" >> /etc/pacman.conf
echo "Server = http://bohoomil.com/repo/$arch" >> /etc/pacman.conf
echo "" >> /etc/pacman.conf

# If these two commands fail with an error about dirmngr, you will probably need to
# su into root and run dirmngr. This will error and create the .gnupg dotfile and the
# other files needed to make this work.
sudo pacman-key -r 962DDE58
sudo pacman-key --lsign-key 962DDE58

# Add AUR packages to install here - Change the graphics driver if you aren't using
# an Intel card.
packages=("wget", "emacs", "gvim", "openssh", "steam", "zsh", "zsh-completions", "tmux",
	        "xf86-video-fbdev", "xf86-video-intel", "xf86-video-vesa", "alsa-utils", "xorg",
          "xorg-xinit", "dmenu", "ttf-inconsolata", "i3", "google-chrome", "clojure", "leiningen",
	        "xclip", "playerctl", "infinality-bundle", "lieningen-completions", "npm", "conky",
          "feh", "networkmanager", "htop", "physlock", "mit-scheme", "pulseaudio", "ponymix", "rlwrap")

# Reload the font cache
fc-cache -fv


# Execute the array of commands
for (( i=0; i<${#packages[@]}; i++ )); do
  printf "\n **** Installing: ${packages[$i]} ****\n\n"

  eval "yaourt -S ${packages[$i]}"
done

printf "\n **** Finished Installing Packages **** \n"

# Add additional commands to be run here. They will be run one after the other in sequence.
commands=()

# Execute the array of commands
for (( i=0; i<${#commands[@]}; i++ )); do
  printf "\n **** Running: ${commands[$i]} ****\n\n"

  eval "yaourt -S ${commands[$i]} | yes"
done

# Set locale correctly
sudo sed -i -e 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sudo locale-gen


# TODO: This section

# install dotfiles
# run dotfile installer script

# configure physlock
# copy services/screensaver.service into /etc/systemd/system/ after
# copy services/10-physlock to /etc/pm/sleep.d/10-vlock
# replacing USERNAME with their name via sed.
# remember to use sudo

# Copy terminfo so that text editors can find it
mkdir ~/.terminfo
nkdir ~/.terminfo/r/
ln -s /usr/share/terminfo/r/rxvt-unicode ~/.terminfo/r/rxvt-unicode
