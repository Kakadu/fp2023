#!/usr/bin/env bash

git remote | grep upstream
if [ $? -ne 0 ]
then
  git remote add upstream https://github.com/Kakadu/fp2023.git
  git fetch upstream master
fi

echo ""
CHANGES=`git diff-tree HEAD~1..HEAD | rev | cut -f 1 | rev`

set answer=""
for dir in $CHANGES
do
  #echo $dir
  if [ -d "$dir" ]; then
    if [ "$dir" != ".github" ]; then
      answer="$answer $dir"
      #echo dir answer="$answer"
    fi
  else
    :
  fi
done

topnames=`echo $answer | xargs -n1 | sort -u | xargs`
rez=`echo $topnames | wc -l`
if [ "$rez" = "1" ]; then
  echo "latest=$topnames"
  exit 0
else
  echo "Too many cancdidates ($rez) to be a last solution"
  echo "'$topnames'"
  exit 1
fi
