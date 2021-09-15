---
layout: archive
title: "Documentation and useful tricks for HAL9000"
permalink: /documentation_RServer/
author_profile: true
toc: true
---

# R Server

## Create a new account

- you can add new users with by typing the command `sudo adduser newuser` in the Terminal (you have to be logged in with an admin account) where you should replace `newuser` with the name of the user you want to create
- you will be prompted to give some information for the user like an initial password etc.

## RServer commands

here are some useful RServer commands

- `sudo rstudio-server stop `
- `sudo rstudio-server start`
- `sudo rstudio-server restart`
- `sudo rstudio-server status`
- `sudo rstudio-server active-sessions`

## Add User to group

- This commands adds a user to a group `sudo usermod -a -G <groupname> <username>`

## Check R Version

- you can check the R version with the command `R --version`

## Update R Version of RServer

- to update the RVersion of the RServer you just have to update R on the computer the Server runs on. To do this, check the many up to date explanations on the internet (Google: "update R ubuntu 20.04")

# Ubuntu/Linux


## Show memory and CPU workload

- in the Terminal type `htop` which gives you detailed information about memory and CPU usage 

## Change your own password

- in the Terminal type `passwd` 
- insert your password

## Changing the password of another user 

- sign in with an admin account
- in Terminal type `sudo passwd user` where you should replace `user` with the name of the user you want to change the password of
- confirm comand with admin password 
- type in new password


## Check Ubuntu Version

- you can check the currently installed Ubuntu Version with by typing `lsb_release -a` in the terminal

## Restart/reboot computer via the terminal in RStudio

- you can also restart/reboot the computer running the RServer via the terminal pane in RStudio with the command `sudo reboot`

## look up IP adress

- `ifconfig`

## Open and Close documents with `vim`

- Sometimes you have to open & edit documents in a text editor via the terminal, for instance `vim`
- you can open documents in the terminal with vim by typing `vim document`
- To close a document `:q` and hit `Enter`