#!/bin/bash
# This shell script will build all the suckless files I use.

echo -e "Dependencies will be installed if they are needed...\n"
sudo pacman -S --needed --noconfirm make gcc libavif gcr webkit2gtk
yay -S --needed --noconfirm ttf-iosevka ttf-font-awesome

echo -e

cd ./dwm && sudo make clean install && cd ..
cd ./st && sudo make clean install && cd ..
cd ./slock && sudo make clean install && cd ..
cd ./surf && sudo make clean install && cd ..
cd ./dmenu && sudo make clean install && cd ..
cd ./slstatus && sudo make clean install && cd ..

read -p "Delete LightDM and it's additional packages (y/n)? " lightdmDelete

case "$lightdmDelete" in
	y|Y) sudo pacman -Rdd --noconfirm light-locker lightdm lightdm-gtk-greeter lightdm-runit;;
	n|N) echo "LightDM will not be removed.";;
	*) echo "LightDM will not be removed.";;
esac

echo -e "\nSuccess!\nEverything should be up and running. Append \"exec dwm\" on the last line of your .xinitrc file in the home directory."
