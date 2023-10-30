#!/usr/bin/env sh

REPONAME=$1
LANGNAME=$2
OUTFILE=$3
DATE=$(TZ='Europe/Moscow' date +%F\ %k:%M)

echo "Документация и тестовое покрытие $(cat _coverage/percent.txt) должны скоро появиться.\n\nhttps://kakadu.github.io/$REPONAME/docs/$LANGNAME\n\nhttps://kakadu.github.io/$REPONAME/cov/$LANGNAME\n\n$DATE" > $OUTFILE
