#!/bin/bash
# lauch a gtk application with a different theme
# set GTKRCFILE variable to your favourite theme
GTKRCFILE=Clearlooks
GTK2_RC_FILES=/usr/share/themes/"$GTKRCFILE"/gtk-2.0/gtkrc "$@"

