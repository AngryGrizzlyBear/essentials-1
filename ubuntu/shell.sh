# Shell
echo "Installing shells..."
sudo apt-get -qq -y install zsh

echo "Changing shell to zsh..."
chsh -s $(which zsh)
