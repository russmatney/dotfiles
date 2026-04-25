#!/usr/bin/env bash

if [ "$(systemctl is-active bluetooth.service)" = "active" ]; then
	echo "1"
else
	echo "0"
fi
