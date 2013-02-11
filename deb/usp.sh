#!/bin/bash

INSTALL_PATH=/opt/usp/bin

if [ -d "$INSTALL_PATH" ];then
  echo $PATH | grep "$INSTALL_PATH" || export PATH=$PATH:$INSTALL_PATH
  # echo $PATH | grep "$INSTALL_PATH" || export $INSTALL_PATH:PATH=$PATH
fi
