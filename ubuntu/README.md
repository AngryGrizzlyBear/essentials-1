# Ubuntu Install

The `installer.sh` script is current very experimental and could brick your install so be careful using it. To use the installer script:

```
./installer.sh <desktop|laptop>
```

Supply either `desktop` or `laptop` as the argument to the installer. Otherwise, you can run each script individually.

## Desktop Setup Information

![desktop screenshot](../current-desktop-desktop.png?raw=true "Current Desktop")

Installing Ubuntu on a desktop with these scripts is a little different. I currently use cinnamon instead of xmonad on my desktop. 

### Nvidia Graphics Card Issues - Booting to Black Screen with Ubuntu 16.04 and LUKS

It seems that the Nvidia proprietary drivers screw up booting into the splash 
screen irrepairably. This likely won't bother you much if you don't use LUKS (I'm guessing since it will just boot through to where it needs to be anyway) but for those of us who do, disabling the splash screen is the only way I have found to be able to enter your password in and continue booting. 

To fix this problem change `splash` to `nosplash` in your GRUB config and run `sudo update-grub` to lock in the changes.

If you've found yourself in a situation where you are booting into a black screen and don't know how to fix it:

1. Push right shift a bunch of times until you get to the GRUB boot selection screen
2. Select your installation and press `e`
3. Scroll down until you see a place where it says `quiet splash`
4. Erase `quiet splash` and put either `nomodeset` or `nvidia.modeset=0` - If one doesn't work try the other (this isn't a permanent change)
5. You should be able to boot into your install far enough to `ctrl + alt + fN` (where N is 1 - 6) into a virtual terminal so you can make the above `nosplash` change to your GRUB config permanent

An additional script `fixplymouth.sh` was included here for archiving purposes. This script did not work for me, but it may work for your install. If you want to keep the splash screen, try running `fixplymouth.sh` first.

## Laptop Setup Information

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

### X Server

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

### GRUB Changes

A small change has to be applied to stop Ubuntu from booting into a non-existent display manager's login screen:

1. Type `sudo nano /etc/default/grub`
2. Find `GRUB_CMDLINE_LINUX_DEFAULT="quiet splash"`
3. Replace (2) with `GRUB_CMDLINE_LINUX_DEFAULT="text"`
4. Run `sudo upgrade-grub`

This will force Ubuntu to boot into a tty. To start Ubuntu type `startx` in any tty.

### Changing Desktop Backgrounds

Inside of `~/.xinitrc` is a line that starts with `feh`. Change the image being passed into this
to change the desktop background. Kill the xserver with `alt + shift + q` and `startx` again.

### TODO

  1. Have installer script `git clone` essentials and symlink everything correctly.
  2. Have installer automatically modify the grub config to set up text login mode.
