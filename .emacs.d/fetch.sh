#!/bin/bash

cd ~/Mail 
git add --all . 
git commit -m 'User update' 
if [ $? -eq 0 ]; then
    git fetch
    echo "Fetch ok"
    git rebase remotes/origin/master
    echo "Rebase ok"
    git push origin master
    echo "Push ok"
else
    git pull -r origin master
    echo "Pull ok"
fi
