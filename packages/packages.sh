## Para instalar o Arch Linux, siga a Wiki: https://wiki.archlinux.org/title/Installation_guide

## Atualiza os pacotes instalados
sudo pacman -Su && sudo pacman -Sy archlinux-keyring
sudo pacman -Syu

## Compilação do Paru (gerenciador de pacotes)
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si
cd ..

## Instalação de pacotes
sudo pacman -S emacs-nativecomp			# Meu editor de texto (obs.: as configurações do emacs estão em .emacs.d/init.el)
sudo pacman -S qt5ct								# Permite trocar o tema e os ícones do GTK
sudo pacman -S zsh									# Meu shell (as configurações do ZSH estão em zsh/.zshrc)
sudo pacman -S pasystray						# System tray do Pavu Control
sudo pacman -S pavucontrol					# Controlador de áudio Pavu Control
sudo pacman -S dunst								# Programa de notificações Dunst
sudo pacman -S feh									# Programa para manipulação de Wallpaper
sudo pacman -S picom								# Compositor de tela Picom
sudo pacman -S xss-lock slock				# Bloqueadores de tela
sudo pacman -S alsa-utils						# Controla o volume
sudo pacman -S brightnessctl				# Controla o brilho
sudo pacman -S tlp									# Controle de Wi-Fi e Bluetooth
sudo pacman -S playerctl						# Controle de música
sudo pacman -S firefox							# Navegador Firefox
sudo pacman -S unzip zip gzip tar		# Permitem compactar e descompactar arquivos
sudo pacman -S r tcl tk gcc-fortran	# Linguagem de progração R
sudo pacman -S julia								# Linguagem de programação Julia
sudo pacman -S go										# Linguagem de programação Go
sudo pacman -S obs-studio						# Programa de streaming e gravação Open Broadcaster Software
sudo pacman -S rclone fuse3					# Programa para configurar acesso à cloud (ex.: Google Drive)
paru caffeine-ng										# Deixa a tela do computador ligada
paru google-chrome									# Navegador Google Chrome
paru miniconda3											# Versão simplificada do conda, gerenciador de ambientes do Python
paru nvm														# Gerenciador de versões do node.js
paru rstudio-desktop-bin						# Instala o RStudio (IDE voltada para execução de scripts R)
paru discord												# Aplicativo de comunicação voltado para comunidades
paru scrot													# Linha de comando que serve para capturar a tela
paru woeusb-ng											# Ferramenta que permite criar bootável do Windows
paru pass-git-helper								# Instala o gerenciador de senhas pass. Para informações de configuração: https://github.com/languitar/pass-git-helper
paru github-cli											# Instala o controle do GitHub via linhas de comando. Para mais informações: https://github.com/cli/cli

##################################################################################################

## LEMBRETE IMPORTANTE: CONFIGURAR dnsmasq, dhcpdc e ufw

# https://wiki.archlinux.org/title/NetworkManager
# https://wiki.archlinux.org/title/Uncomplicated_Firewall

# sudo ufw default deny incoming
# sudo ufw default allow outgoing
# sudo ufw allow ssh

# Lembre de ativar o dnsmasq, o dhcpcd e o ufw com sudo systemctl enable e sudo systemctl restart

##################################################################################################

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

## Instala o tema do Dracula no GTK e no QT

	## Instalação no GTK
cd /usr/share/themes/
sudo curl -LO https://github.com/dracula/gtk/archive/master.zip
sudo unzip master.zip
sudo mv gtk-master/ Dracula/
sudo rm -rf master.zip
cd ~
gsettings set org.gnome.desktop.interface gtk-theme "Dracula"
gsettings set org.gnome.desktop.wm.preferences theme "Dracula"
cd /usr/share/icons
sudo curl -LO https://github.com/dracula/gtk/files/5214870/Dracula.zip
sudo unzip Dracula.zip
sudo rm -rf Dracula.zip
cd ~
gsettings set org.gnome.desktop.interface icon-theme "Dracula"

	## Instalaçao no QT
git clone https://github.com/dracula/qt5.git
sudo cp ~/qt5/Dracula.conf ~/.config/qt5ct/colors/
sudo cp ~/qt5/Dracula.conf /usr/share/qt5ct/colors/

	## Agora é necessário editar o arquivo /etc/environment
sudoedit /etc/environment
QT_QPA_PLATFORMTHEME=qt5ct		# Adicione essa linha ao final do arquivo e salve o mesmo

	## Reinicie o sistema e dê o seguinte comando
qt5ct		## Este comando abrirá o painel do qt5ct, aí basta escolher a fonte e os ícones do Dracula

	## Atualização dos arquivos do GTK
sudo nano /usr/share/gtk-3.0/settings.ini		# Troque os temas Adwaita por Dracula
sudo nano /usr/share/gtk-4.0/settings.ini		# Troque os temas Adwaita por Dracula

## Instala o tema Dracula no RStudio
# No console do RStudio, execute os comandos abaixo:
install.packages("rstudioapi")
rstudioapi::addTheme("https://raw.githubusercontent.com/dracula/rstudio/master/dracula.rstheme", apply = TRUE)

## Configuração do miniconda3
conda activate base																	# Ativa o ambiente conda
conda config --set auto_activate_base false					# Impede que o conda inicie o ambiente base automaticamente
sudo conda install -c conda-forge python-lsp-server # Instala o Language Server Protocol para a linguagem Python
sudo conda install jupyter 													# Instala o jupyter notebook
sudo conda install -c conda-forge jupyterlab-lsp		# Instala o Language Server Protocol no jupyter lab
sudo conda install numpy							         			# Instala o numpy
sudo conda install openpyxl													# Permite a manipulação de arquivos do Excel
sudo conda install pandas							         			# Instala o pandas
sudo conda install matplotlib					         			# Instala o matplotlib
sudo conda install scipy							         			# Instala o scipy	
sudo conda install scikit-learn				         			# Instala o scikit-learn
sudo conda install nltk															# Instala o nltk
sudo conda install flask							         			#	Instala o flask

	## Instala o tema Dracula no Jupyter Lab
pip install JLDracula

	## Caso o comando clear não esteja funcionando no ambiente conda, execute o comando abaixo
sudo mv $CONDA_PREFIX/bin/clear $CONDA_PREFIX/bin/clear_old
conda deactivate																		# Desativa o ambiente conda

## Instala a linguagem Julia no jupyter notebook
sudo julia
using Pkg
Pkg.add("IJulia")
exit()

## Instala o Language Server no R
sudo R
install.packages(“languageserver”)
quit()

## Instala o Language Server no Go
go install golang.org/x/tools/gopls@latest

## Configurações do node.js e do npm
nvm install --lts																								# Instala o node.js e o npm nas últimas versões estáveis
sudo npm install -g vscode-langservers-extracted								# Instala o language server para HTML e CSS
sudo npm i -g typescript-language-server; npm i -g typescript		# Instala o language server para JavaScript e TypeScript
sudo npm i -g bash-language-server															# Instala o language server para Bash
sudo npm install -g live-server																	# Instala o live server
