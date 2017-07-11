#!/bin/sh
set -e
if [ "$#" -eq 0 ];
then
	echo "Usage: $0 DIR" >&2
	exit 1;
fi
TARGET="$(realpath "$1")"

echo "Removing and repopulating $TARGET" >&2
if [ -d "$TARGET" ]; then
	mv "$TARGET" "$TARGET.$(date +%F)" || rm -fr "$TARGET"
fi
rm -fr "$TARGET"
mkdir -p "$TARGET"

echo "Downloading and installing clean nightly" >&2
curl -sSL ftp://ftp.cs.ru.nl/pub/Clean/builds/linux-x64/clean-bundle-complete-linux-x64-latest.tgz \
	| tar --gunzip --strip-components=1 --extract --directory="$TARGET"

echo "Apply patches" >&2
curl -sSL https://gitlab.science.ru.nl/mlubbers/iTasks-SDK/raw/95-addbackgroundtask/Libraries/iTasks/_Framework/TaskServer.dcl\
	> "$TARGET"/lib/iTasks/iTasks/_Framework/TaskServer.dcl
curl -sSL https://gitlab.science.ru.nl/mlubbers/iTasks-SDK/raw/95-addbackgroundtask/Libraries/iTasks/_Framework/TaskServer.icl\
	> "$TARGET"/lib/iTasks/iTasks/_Framework/TaskServer.icl

echo "export CLEAN_HOME=$TARGET; export PATH=$TARGET/bin:\$PATH;"
