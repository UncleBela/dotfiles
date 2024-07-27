#!/usr/bin/sh

read -p "Please choose (dwm), (exwm) or (kde): " option
option=${option,,}
USERDIR="/home/$USER"

case $option in
    dwm)
	if test -f $USERDIR/.xinitrc-dwm; then
	    echo -e "Starting DWM."
	    startx $USERDIR/.xinitrc-dwm > /dev/null 2>&1
	    echo -e "Exiting DWM."
	else
	    echo "DE/WM does not exist."
	fi
	;;
    exwm)
	if test -f $USERDIR/.xinitrc-exwm; then
	    echo -e "Starting EXWM."
	    startx $USERDIR/.xinitrc-exwm > /dev/null 2>&1
	    echo -e "Exiting EXWM."
	else
	    echo "DE/WM does not exist."
	fi
	;;
    kde)
	if test -f $USERDIR/.xinitrc-kde; then
	    echo -e "Starting KDE."
	    startx $USERDIR/.xinitrc-kde > /dev/null 2>&1
	    echo -e "Exiting KDE."
	else
	    echo "DE/WM does not exist."
	fi
	;;
    *)
	echo "Invalid option."
esac
