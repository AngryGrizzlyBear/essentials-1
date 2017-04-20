# Linux Essentials

This repository contains everything essential to getting a running Linux installation going with xmonad, tmux,
lemonbar, and many other configurations. The current flavor of the month for me is Nix OS. For Nix specific configuration instructions and configuration files, please go to the *nix* directory and ignore everything else here - which is left for historical Arch Linux install reasons.

The directory structure is as follows:

1. *dotfiles/* - This contains any related dotfiles
2. *scripts/* - This directory contains most of the important things to get the installation running
  1. *scripts/sysinstall/* - This directory contains a number of scripts for configuring the system
  2. *scripts/sysinit/* - This directory contains anything needed for system initialization
  3. *scripts/solutions/* - This directory contains any fixes that can be run for various packages
  4. *scripts/lemonbar/* - This directory contains any related lemonbar scripts
  5. *scripts/diagnostics/* - This directory contains scripts for helping to diagnose problems
3. *installation_instructions* - This file contains a series of command you copy and paste during install of Arch
4. *nix/* - This contains all the stuff needed to get up and running with my nix configuration

Take care with the dotfiles. My configurations change the terminal colors to be solarized dark. If you do not like them
they can be easily changed in the Xresources dotfile.

# Issues

There are of course a things that aren't covered (yet).

1. Thinkpad T440s function keys don't work (in the works)
2. Thinkpad T440s mousepad right-click and gestures do not work
3. No automated way to setup locker
4. No automated way to setup suspend on lid close
5. No chef script to do everything for you
6. Rustc - local libs aren't initially visible
   1. Rustc requires you to manually enter sudo ldconfig /usr/local/lib once so that it can find its libraries

# Software To Install

Below is a not a comprehensive list of software you should install and configure. The *install_packages.sh* script is
a work in progress that will get the system up and running with everything I am using.


## Package Management
1. Yaourt

## Wallpaper
1. Feh

In order to install wallpapers properly, the xinitrc in this repository includes a line to start a task in the
background to show the wallpaper. If you use sh instead of zsh, change it appropriately.

## Terminal
1. rxvt-unicode-256color

## Display
1. xorg
2. xorg-xinit
3. i3

## Screen Locking
1. Physlock

## Fonts
1. infinality-bundle
2. ibfonts-meta-base
3. ttf-inconsolata (for the terminal)

## Mousepad
1. Synaptic

## Editing
1. GVim

## Browsing
1. Google Chrome


# Neovim Configuration Notes

1. Make sure to run `:GoInstallBinaries` if you want to use `vim-go`. Make sure Go and `$GOPATH` are available.
