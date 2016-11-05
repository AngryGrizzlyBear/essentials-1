# Languages
echo "Installing languages and tools..."
sudo apt-get -qq -y install libssl-dev
sudo apt-get -qq -y install libcurl4-openssl-dev
sudo apt-get -qq -y install python-dev
sudo apt-get -qq -y install python-pip
sudo apt-get -qq -y install python3-dev
sudo apt-get -qq -y install python3-pip
sudo apt-get -qq -y install python3-setuptools
sudo apt-get -qq -y install m4
sudo apt-get -qq -y install ocaml
sudo apt-get -qq -y install aspcud
sudo apt-get -qq -y install liblapack-dev
sudo apt-get install --install-recommends winehq-devel
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
sudo apt-get -qq -y install r-base
sudo apt-get -qq -y install r-base-dev
sudo apt-get install gdebi-core
wget https://download1.rstudio.org/rstudio-0.99.896-amd64.deb
sudo gdebi -n rstudio-0.99.896-amd64.deb
rm rstudio-0.99.896-amd64.deb
sudo apt-get -qq -y install r-cran-rcpparmadillo
sudo apt-get -qq -y install r-cran-rgl
sudo apt-get -qq -y install libpq-dev

# Editors
echo "Installing editors..."
sudo apt-get -qq -y install vim
sudo apt-get -qq -y install neovim

echo "Installing python tools for neovim..."
sudo pip install neovim

# Databases
echo "Installing databases..."
sudo apt-get -qq -y install mysql-server
sudo apt-get -qq -y install libmysqlclient-dev

# Source Control
echo "Installing source control..."
sudo apt-get -qq -y install git

# Multimedia
echo "Installing multimedia..."
sudo apt-get -qq -y install ffmpeg
sudo apt-get -qq -y install mplayer
sudo apt-get -qq -y install spotify-client
sudo apt-get -qq -y install pepperflashplugin-nonfree

# Tools
echo "Installing tools..."
sudo apt-get -qq -y install transmission
sudo apt-get -qq -y install htop
sudo apt-get -qq -y install scrot
sudo apt-get -qq -y install tmux
sudo apt-get -qq -y install unclutter
sudo apt-get -qq -y install xdotool
sudo apt-get -qq -y install cmake
sudo apt-get -qq -y install gsimplecal
sudo apt-get -qq -y install xclip
sudo apt-get -qq -y install texlive-full
sudo apt-get -qq -y install oracle-java8-installer

# Comms
echo "Installing comms..."
sudo apt-get -qq -y install irssi
sudo apt-get -qq -y install skype

# Browsers
echo "Installing browsers..."
sudo apt-get -qq -y install chromium-browser

# Security
echo "Installing security..."
sudo apt-get -qq -y install physlock

# Fonts
echo "Installing fonts..."
sudo apt-get -qq -y install fonts-inconsolata
fc-cache --really-force -v

