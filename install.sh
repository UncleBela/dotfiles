#!/bin/sh

echo -e "Uncle Béla's dotfiles installation"

ls ~/.uncle_bela/dotfiles &> /dev/null
if [ $? -ne 0 ]; then
    git clone https://github.com/UncleBela/dotfiles.git ~/.uncle_bela/dotfiles
    if [ $? -ne 0 ]; then
	exit 1
    fi
else
    echo -e "\"dotfiles\" already exists in the directory ~/.uncle_bela."
    read -p "Do you wish to clone it again? (y/n): " choice
    case $choice in
	[yY]) git clone https://github.com/UncleBela/dotfiles.git ~/.uncle_bela/dotfiles
	      if [ $? -ne 0 ]; then
		  exit 1
	      fi
	      ;;
	[nN]) echo -e "Will be using already existing dotfiles...";;
    esac
fi

cd ~/.uncle_bela/dotfiles/
MAIN_DIR=$(pwd)

echo -e "------------------------"
echo -e "------------------------"
echo -e "|    C Music Player    |"
echo -e "------------------------"
echo -e "------------------------"

sudo pacman -S --needed --noconfirm cmus
mkdir -p ~/.config/cmus/
cp -R ./cmus/* ~/.config/cmus/
printf "C Music Player files copied."

echo -e "------------------------"
echo -e "------------------------"
echo -e "|        Emacs         |"
echo -e "------------------------"
echo -e "------------------------"

sudo pacman -S --needed --noconfirm emacs
mkdir -p ~/.emacs.d/
cp -R ./emacs/* ~/.emacs.d/
printf "Emacs config files copied."

cat > ~/.Xresources <<EOF
*.WINDOW_FOREGROUND: white
EOF

xrdb ~/.Xresources

echo -e "------------------------"
echo -e "------------------------"
echo -e "|         Fish         |"
echo -e "------------------------"
echo -e "------------------------"

sudo pacman -S --needed --noconfirm fish
chsh -s /usr/bin/fish
mkdir -p ~/.config/fish/
cp ./fish/* ~/.config/fish/
printf "Fish shell set and config files were copied."

echo -e "------------------------"
echo -e "------------------------"
echo -e "|       Scripts        |"
echo -e "------------------------"
echo -e "------------------------"

sudo cp ./scripts/* /usr/local/bin/

echo -e "------------------------"
echo -e "------------------------"
echo -e "|  Suckless Software   |"
echo -e "------------------------"
echo -e "------------------------"

printf "Installing suckless software now..."
cd ./suckless/
chmod +x ./build.sh

read -p "Will you be using a (pc), or a (laptop)?: " deviceChoice

case "$deviceChoice" in
	pc|PC) sh ./build.sh pc;;
	laptop|LAPTOP) sh ./build.sh laptop;;
	*) echo "No device selected.";;
esac

echo -e "------------------------"
echo -e "------------------------"
echo -e "|         Xorg         |"
echo -e "------------------------"
echo -e "------------------------"

cd $MAIN_DIR
cp ./X/.* ~/
sudo cp ./X/start-de.sh /usr/local/bin

echo -e "------------------------"
echo -e "------------------------"
echo -e "|      Additional      |"
echo -e "------------------------"
echo -e "------------------------"

printf "\nCopying Thunar config file..."
mkdir -p ~/.config/Thunar
cp ./thunar/uca.xml ~/.config/Thunar/uca.xml
