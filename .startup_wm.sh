#!/bin/bash

# for laptop on right side
#xrandr --output VGA1 --pos 0x0 --output HDMI1 --pos 1600x0 --output eDP1 --pos 3520x536

# for laptop on left side, monitor not rotated
#xrandr --output eDP1 --pos 0x900 --output HDMI1 --pos 1600x0 --mode 1920x1200 --scale 1x1 --output VGA1 --pos 3520x0 --mode 1600x1200 --scale 1x1

# for laptop on left side, first monitor rotated
#xrandr --output eDP1 --pos 0x1200 --output HDMI1 --pos 1600x0 --rotate left --mode 1920x1200 --scale 1x1 --output VGA1 --pos 2800x400 --mode 1600x1200 --scale 1x1

# for laptop on right side, second monitor rotated
xrandr \
    --output VGA1 --pos 0x400 --mode 1600x1200 --scale 1x1 \
    --output HDMI1 --pos 1600x0 --rotate left --mode 1920x1200 --scale 1x1 \
    --output eDP1 --pos 2800x1200

# for projector
# exec xrandr --output eDP1 --pos 0x0 --output HDMI1 --scale 1x1 --mode 1280x720 --pos 1600x0 --rate 50 --output VGA1 --off

# hsetroot -solid "#43271E"
# feh --bg-scale ~/wp/wp.jpg

xscreensaver -nosplash &
setxkbmap -layout "us,ru"
setxkbmap -option "grp:caps_toggle,grp_led:scroll,compose:ralt"
xkbcomp $DISPLAY - | egrep -v "group . = AltGr;" | xkbcomp - $DISPLAY
xdg-mime default google-chrome.desktop x-scheme-handler/http
xdg-mime default google-chrome.desktop x-scheme-handler/https

/usr/local/bin/set_ru_map

# remap right alt to normal alt
xmodmap -e "clear mod5"
xmodmap -e "keycode 108 = Alt_L"

