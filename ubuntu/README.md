# Ubuntu Install

## Desktop Notes

Install Ubuntu on a desktop with these scripts is a little different. I currently use cinnamon instead of xmonad on my desktop.


### Nvidia Graphics Card Issues with LUKS

It seems that the Nvidia proprietary drivers screw up booting into the splash 
screen irrepairably. This likely won't bother you much if you don't use LUKS
but for those of us who do disabling the splash screen is the only way to 
be able to enter your password in and continue booting. 

To fix this problem change `splash` to `nosplash` 
in your GRUB config and run `sudo update-grub` to lock in the changes.

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

### Xmonad Hotkeys

Here are some hotkeys to get you started:

* `mod4 + shift + q`: Kill X server
* `mod4 + q`: Recompile Xmonad
* `mod4 + w`: Move window up
* `mod4 + s`: Move window down
* `mod4 + a`: Move window left
* `mod4 + d`: Move window right
* `mod4 + enter`: Spawn a new window
* `mod4 + space`: Launch dmenu
* `mod4 + h`: Shift focus left
* `mod4 + j`: Shift focus down
* `mod4 + k`: Shift focus up
* `mod4 + l`: Shift focus right
* `mod4 + <1...9>`: Change to nth workspace 
* `mod4 + t`: Retile windows
* `mod4 + shift + c`: Close currently focused window

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
