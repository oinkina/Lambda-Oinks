#!/bin/sh
ghc --make site.hs
./site rebuild
cp -r ~/lambda-oinks/_site/* ~/oinkina.github.io/
(cd ~/oinkina.github.io
git add .
git commit -a -m "blog update"
git push origin master)