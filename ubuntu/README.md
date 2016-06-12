# Ubuntu Installation Notes

![desktop screenshot](../current-desktop.png?raw=true "Current Desktop")

By running the installer it wipes a complete install and turns it into something more minimal and less resource
intensive. An alternative would've been to start with the Ubuntu minimal disk and work up, but the full Ubuntu ISO comes with some conveniences that I wanted to keep. Kit includes:

  - Compton
  - Xmonad
  - Dmenu
  - Lemonbar
  - Urxvt
  - Physlock
  - feh
  - zsh

It also substitutes Ubuntu's stock media players with less resource intensive alternatives such as `mplayer` and
`ffmpeg`.

*NOTE:* After rebooting you will boot into a black screen. See the section on GRUB for details. To get to a login
screen press `ctrl + alt + f1` and login normally.

## X Server

To start Xmonad simply type `startx` at the tty.

Make sure Xresources and Xinitrc are symlinked properly in your home directory. If you have any problems getting
Xmonad to start, check the logs, and then those two files.

To Kill the server at any time press `alt + shift + q`.

## GRUB

A small change has to be applied to stop Ubuntu from booting into a non-existent display manager's login screen:

1. Type `sudo nano /etc/default/grub`
2. Find `GRUB_CMDLINE_LINUX_DEFAULT="quiet splash"`
3. Replace (2) with `GRUB_CMDLINE_LINUX_DEFAULT="text"`
4. Run `sudo upgrade-grub`

This will force Ubuntu to boot into a tty. To start Ubuntu type `startx` in any tty.

## Changing Desktop Backgrounds

Inside of `~/.xinitrc` is a line that starts with `feh`. Change the image being passed into this
to change the desktop background. Kill the xserver with `alt + shift + q` and `startx` again.

## TODO

  1. Have installer script `git clone` essentials and symlink everything correctly.
  2. Have installer automatically modify the grub config to set up text login mode.
