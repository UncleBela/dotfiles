#!/bin/bash
# This shell script will build all the suckless files I use.

platform=$1
platform=${platform,,}

if [[ $platform != "pc" && $platform != "laptop" ]]; then
    echo $platform
    echo "Please provide (pc) or (laptop) as an argument on calling build.sh"
    exit
fi

echo -e "Dependencies will be installed if they are needed...\n"
sudo pacman -S --needed --noconfirm make gcc libavif gcr webkit2gtk
yay -S --needed --noconfirm ttf-iosevka ttf-font-awesome

echo -e

if [ $platform == "laptop" ]; then
    sudo cp ./dwm/config.h.x260 ./dwm/config.h
    sudo cp ./st/config.h.x260 ./st/config.h
    sudo cp ./dmenu/config.h.x260 ./dmenu/config.h
    sudo cp ./slstatus/config.h.x260 ./slstatus/config.h
elif [ $platform == "pc" ]; then
    sudo cp ./dwm/config.h.pc ./dwm/config.h
    sudo cp ./st/config.h.pc ./st/config.h
    sudo cp ./dmenu/config.h.pc ./dmenu/config.h
    sudo cp ./slstatus/config.h.pc ./slstatus/config.h
else
    echo "The what?"
    exit
fi

sudo cp ./slock/config.h.pc ./slock/config.h
sudo cp ./surf/config.h.pc ./surf/config.h

echo -e "Suckless software is being installed on a ${platform~~}.\n"

sudo make -C clean install ./dwm
sudo make -C clean install ./st
sudo make -C clean install ./slock
sudo make -C clean install ./surf
sudo make -C clean install ./dmenu
sudo make -C clean install ./slstatus

echo -e "\nSuccess!\nEverything should be up and working. Append \"exec dwm\" on the last line of your .xinitrc file in the home directory."
