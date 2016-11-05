# Pre-setup
echo "Running pre-setup..."
sudo apt-add-repository ppa:neovim-ppa/unstable
sudo apt-add-repository -y "deb http://repository.spotify.com stable non-free"
sudo apt-add-repository -y ppa:richardgv/compton
sudo apt-add-repository -y "http://apt.postgresql.org/pub/repos/apt/"
sudo apt-add-repository -y ppa:webupd8team/java
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D2C19886
sudo dpkg --add-architecture i386
sudo add-apt-repository ppa:wine/wine-builds
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get -qq -y update
sudo apt-get -qq -y upgrade
