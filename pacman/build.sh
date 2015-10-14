#!/bin/sh
set -ue

makepkg $*

PKGNAME=`\ls -1 *.pkg.tar.xz`
cat <<++EOS > README
# Last Update   :       `date '+%Y/%m/%d'`
# Author        :       kunst1080
#
# Install       :       ./build.sh && pacman -U $PKGNAME
#               :       curl -L -O https://github.com/usp-engineers-community/Open-usp-Tukubai/raw/master/pacman/$PKGNAME && pacman -U $PKGNAME
#
# Uninstall     :       pacman -R open-usp-tukubai
#
++EOS
