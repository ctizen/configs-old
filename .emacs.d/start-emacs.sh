#!/bin/bash

Xephyr -br -ac -noreset -screen 1920x1055 -dpi 120 :5 &
DISPLAY=:5 i3 &
DISPLAY=:5 emacs
