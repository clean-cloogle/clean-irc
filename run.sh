#!/bin/sh
while true
do
	git pull origin master
	make
	./cloogle
	sleep 5s
done
