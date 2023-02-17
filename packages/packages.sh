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
sudo pacman -S emacs-nativecomp		# Meu editor de texto (obs.: as configurações do emacs estão em .emacs.d/init.el)
sudo pacman -S zsh								# Meu shell (as configurações do ZSH estão em zsh/.zshrc)
sudo pacman -S firefox						# Navegador Firefox
sudo pacman -S unzip zip gzip tar	# Permitem compactar e descompactar arquivos
sudo pacman -S r tcl tk						# Linguagem de progração R
sudo pacman -S julia							# Linguagem de programação Julia
paru google-chrome								# Navegador Google Chrome
paru miniconda3										# Versão simplificada do conda, gerenciador de ambientes do Python
paru nvm													# Gerenciador de versões do node.js
paru discord											# Aplicativo de comunicação voltado para comunidades
paru telegram-desktop							# Serviço de mensagens instantâneas
paru scrot												# Linha de comando que serve para capturar a tela
paru woeusb-ng										# Ferramenta que permite criar bootável do Windows
paru pass-git-helper							# Instala o gerenciador de senhas pass. Para informações de configuração: https://github.com/languitar/pass-git-helper
paru github-cli										# Instala o controle do GitHub via linhas de comando. Para mais informações: https://github.com/cli/cli

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

## Configuração do miniconda3
conda activate base																	# Ativa o ambiente conda
sudo conda install -c conda-forge python-lsp-server # Instala o Language Server Protocol para a linguagem Python
sudo conda install jupyter 													# Instala o jupyter notebook
sudo conda install r r-irkernel				         			# Instala o R no jupyter notebook
sudo conda install numpy							         			# Instala o numpy
sudo conda install pandas							         			# Instala o pandas
sudo conda install matplotlib					         			# Instala o matplotlib
sudo conda install scipy							         			# Instala o scipy	
sudo conda install scikit-learn				         			# Instala o scikit-learn
sudo conda install flask							         			#	Instala o flask
sudo conda update --all								         			# Atualiza todos os pacotes possíveis	

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

## Instala o node.js e npm nas versões estáveis, além dos pacotes para de Language Server para HTML, CSS, JavaScript e Bash
nvm install --lts
sudo npm install -g vscode-langservers-extracted
sudo npm i -g typescript-language-server; npm i -g typescript
sudo npm i -g bash-language-server
sudo npm install -g live-server 
