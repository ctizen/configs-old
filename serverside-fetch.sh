#!/bin/bash

offlineimap

cd /home/notepad/Mail
git add --all .
git commit -m 'System update'
if [ $? -eq 0 ]; then
    git fetch
    echo "Fetched ok"
    git rebase remotes/origin/master
    echo "Rebase ok"
    git push origin master
    echo "Push ok"
else
    git pull -r origin master
    echo "Pulled ok"
fi
q
