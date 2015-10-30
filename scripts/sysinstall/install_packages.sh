#!/bin/bash

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

# Install yaourt before doing anything else
install_yaourt


# Add AUR packages to install here
packages=("wget", "emacs", "gvim", "openssh", "steam", "zsh", "zsh-completions", "tmux") 

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
  
  eval "yaourt -S ${commands[$i]}"
done

