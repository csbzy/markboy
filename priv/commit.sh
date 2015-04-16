# !/bin/sh

git add --all
git commit  -m "${1}"
git push origin master

cp -rf *  ../_rel/markdown_middleware_example/lib/markdown_middleware-1/priv/
