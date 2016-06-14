#! /bin/bash
echo $PATH 1>&2
compton &

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

xsetroot -cursor_name left_ptr

xset r rate 300 30


#scripts/wp slideshow &
#~/bar_start.sh &
# TODO: start lighter here 
unclutter -grab &
tmux start-server
pulseaudio -D
wmname compton
xmonad
