#!/bin/sh
emacsclient -e "(emsane-scanadf-emacsclient-notify \"$1\" \"$EMSANE_STATE\")"