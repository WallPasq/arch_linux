#!/bin/bash

# Configurações para teclado ABNT2
setxkbmap -layout br -variant abnt2

# Executa o compositor de telas
picom &

# Habilita o compositor de tela
xss-lock -- slock &

# Executar o Emacs
exec emacs -mm --debug-init
