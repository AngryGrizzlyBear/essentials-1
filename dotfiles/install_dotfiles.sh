ln -s $(pwd)/global_gitignore ~/.global_gitignore
ln -s $(pwd)/ocamlinit ~/.ocamlinit
ln -s $(pwd)/tmux.conf ~/.tmux.conf
ln -s $(pwd)/vimrc ~/.vimrc
ln -s $(pwd)/zshrc ~/.zshrc

if [[ -d "~/.config" ]]
then
  cp -R $(pwd)/config/* ~/.config/
else
  ln -s $(pwd)/config ~/.config
fi
