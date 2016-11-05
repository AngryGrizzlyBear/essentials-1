# Cleanup
echo "Cleaning up unneccessary programs..."
sudo apt-get -qq -y purge firefox
sudo apt-get -qq -y purge rhythmbox
sudo apt-get -qq -y purge cheese
sudo apt-get -qq -y purge shotwell
sudo apt-get -qq -y purge shotwell-common
sudo apt-get -qq -y purge thunderbird
sudo apt-get -qq -y purge aisleriot
sudo apt-get -qq -y purge webbrowser-app
sudo apt-get -qq -y purge gnome-sudoku
sudo apt-get -qq -y purge mahjongg
sudo apt-get -qq -y purge gnomine
sudo apt-get -qq -y purge ace-of-penguins
sudo apt-get -qq -y purge onboard
sudo apt -qq -y autoremove
