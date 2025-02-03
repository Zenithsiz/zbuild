#!/bin/env bash

rsync \
	--delete \
	--ignore-existing \
	--recursive \
	vscode-extension/ \
	~/.vscode/extensions/zenithsiz.zbuild-0.1.0
