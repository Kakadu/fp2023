#!/usr/bin/env sh

REPONAME=$1
LANGNAME=$2
OUTFILE=$3
DATE=$(TZ='Europe/Moscow' date +%F\ %k:%M)

echo "[Документация](https://kakadu.github.io/$REPONAME/docs/$LANGNAME) и [тестовое покрытие](https://kakadu.github.io/$REPONAME/cov/$LANGNAME) $(cat _coverage/percent.txt) должны скоро появиться.\n\n $DATE" > $OUTFILE
