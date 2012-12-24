#!/bin/bash

if [ -d '/opt/usp/bin' ];then
  echo $PATH | grep '/opt/usp/bin' || export PATH=$PATH:/opt/usp/bin
  # echo $PATH | grep '/opt/usp/bin' || export /opt/usp/bin:PATH=$PATH
fi
