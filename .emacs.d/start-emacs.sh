#!/bin/bash

Xephyr -br -ac -noreset :5
DISPLAY=:5 emacs
