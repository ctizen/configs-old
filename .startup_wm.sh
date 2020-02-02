#!/bin/bash

# for laptop on right side
#xrandr --output VGA1 --pos 0x0 --output HDMI1 --pos 1600x0 --output eDP1 --pos 3520x536

# for laptop on bottom
xrandr --output HDMI1 --pos 0x0 --output eDP1 --pos 0x1080

# for laptop on left side, monitor not rotated
#xrandr --output eDP1 --pos 0x900 --output HDMI1 --pos 1600x0 --mode 1920x1200 --scale 1x1 --output VGA1 --pos 3520x0 --mode 1600x1200 --scale 1x1

# for laptop on left side, first monitor rotated
#xrandr --output eDP1 --pos 0x1200 --output HDMI1 --pos 1600x0 --rotate left --mode 1920x1200 --scale 1x1 --output VGA1 --pos 2800x400 --mode 1600x1200 --scale 1x1

# for laptop on right side, second monitor rotated
#xrandr \
#    --output VGA1 --pos 0x400 --mode 1600x1200 --scale 1x1 \
#    --output HDMI1 --pos 1600x0 --rotate left --mode 1920x1200 --scale 1x1 \
#    --output eDP1 --pos 2800x1200

# for projector
#xrandr --output eDP1 --pos 0x0 --output HDMI1 --scale 1x1 --mode 1280x720 --pos 1600x0 --rate 50 --output VGA1 --off
#xrandr --output eDP1 --pos 0x0 --output HDMI1 --scale 1x1 --mode 1920x1080 --pos 1600x0 --rate 50 --output VGA1 --off

# export QT_QPA_PLATFORMTHEME="qt5ct"
# xscreensaver -nosplash &
# setxkbmap -layout "us,ru"
# setxkbmap -option "grp:caps_toggle,grp_led:scroll,compose:ralt"
# xkbcomp $DISPLAY - | egrep -v "group . = AltGr;" | xkbcomp - $DISPLAY
xdg-mime default chromium.desktop x-scheme-handler/http
xdg-mime default chromium.desktop x-scheme-handler/https
xdg-mime default emacsclient.desktop $(grep '^text/*' /usr/share/mime/types)
xdg-mime default emacsdired.desktop inode/directory

# /usr/local/bin/set_ru_map

xfsettingsd &
xfce4-power-manager &
blueman-applet &
mail-notification &
nm-applet &
compton &
libinput-gestures-setup start

# backgrounds
###hsetroot -solid "#43271E"
feh --bg-scale ~/wp/wp.jpg

# remap right alt to normal alt
xmodmap -e "clear mod5"
xmodmap -e "keycode 108 = Alt_L"

