#!/bin/sh
while true
do
	git pull origin master
	git submodule init
	git submodule update
	make -B
	./cloogleirc "$@"
	sleep 5s
done
