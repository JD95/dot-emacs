#!/bin/bash

cd ~/emacs_launch_scripts/

# Update the repo
git pull origin master

# Copy the .emacs file into home
cp .emacs ~/.emacs
