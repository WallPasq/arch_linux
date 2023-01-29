## Para instalar o Arch Linux, siga a Wiki: https://wiki.archlinux.org/title/Installation_guide

## Atualiza os pacotes instalados
sudo pacman -Su && sudo pacman -Sy archlinux-keyring

## Compilação do Paru (gerenciador de pacotes)
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si
cd ..

## Instalação de pacotes
sudo pacman -S emacs		# Meu editor de texto (obs.: as configurações do emacs estão em .emacs.d/init.el)
sudo pacman -S zsh			# Meu shell (as configurações do ZSH estão em zsh/.zshrc)
sudo pacman -S firefox	# Navegador Firefox
paru brave-bin					# Navegador Brave
paru miniconda3					# Versão simplificada do conda, gerenciador de ambientes do Python
paru nvm								# Gerenciador de versões do node.js
paru discord						# Aplicativo de comunicação voltado para comunidades
paru telegram-desktop		# Serviço de mensagens instantâneas
paru scrot							# Linha de comando que serve para capturar tela
paru woeusb-ng					# Ferramenta que permite criar bootável do Windows

## Configurações e plugins para o ZSH (créditos ao LuizOMF -> https://gist.github.com/luizomf/1fe6c67f307fc1df19e58f224134dc8f)
chsh -s /bin/zsh
zsh

	## Instalação do Oh-my-zsh! -> https://ohmyz.sh/
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

	## Instalação do Spaceship Prompt -> https://github.com/spaceship-prompt/spaceship-prompti
git clone https://github.com/spaceship-prompt/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt" --depth=1
ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH_CUSTOM/themes/spaceship.zsh-theme"

	## Instalação do ZSH Autosuggestions -> https://github.com/zsh-users/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

	## Instalação do ZSH Syntax Highlighting -> https://github.com/zsh-users/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

	## Instalação da fonte Ubuntu Mono Powerline
mkdir -p ~/.fonts
git clone https://github.com/pdf/ubuntu-mono-powerline-ttf.git ~/.fonts/ubuntu-mono-powerline-ttf
fc-cache -vf

## Configuração do miniconda3
conda activate base				# Ativa o ambiente conda
conda install jupyter 		# Instala o jupyter notebook
conda install numpy				# Instala o numpy
conda install pandas			# Instala o pandas
conda install matplotlib	# Instala o matplotlib

	## Instala o tema Dracula no Jupyter Lab
pip install JLDracula

	## Caso o comando clear não esteja funcionando no ambiente conda, execute o comando abaixo
sudo mv $CONDA_PREFIX/bin/clear $CONDA_PREFIX/bin/clear_old
conda deactivate					# Desativa o ambiente conda

## Instala o node.js e npm nas versões estáveis
nvm install --lts
