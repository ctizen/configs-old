#!/bin/bash

cd ~/Mail 
git add . 
git commit -m 'User update' 
if [ -z $? ]; then
    git fetch
    git rebase remotes/origin/master
    git push origin master
else
    git fetch
    git checkout master
fi

