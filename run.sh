#!/bin/sh
while true
do
	git pull origin master
	make
	./cloogleirc "$@"
	sleep 5s
done
