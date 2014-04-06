#!/bin/sh

# Requires hub: brew install hub; alias git=hub
# to run: ./newpost [REPO NAME] [POST TITLE] [LOCAL DIR NAME]
hub create lambda-oinks/$1
(cd ~/lambda-oinks/posts
git clone lambda-oinks/$1
(cd $1
echo "---" >> index.md
echo "title: $2" >> index.md
echo "date: `date +%F`" >> index.md
echo "author: Oinkina" >> index.md
echo "mathjax: on" >> index.md
echo "---" >> index.md
git add index.md
git commit -a -m "initialized with template for index.md"
)
rm -rf ~/lambda-oinks/posts/$1
git submodule add lambda-oinks/$1 $3
git commit -a -m "added new post submodule: $1"
git push origin master
)
(cd ~/lambda-oinks
git add .
