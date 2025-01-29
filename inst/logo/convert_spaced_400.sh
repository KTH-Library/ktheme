#! /bin/bash

set -e

convert -alpha set -bordercolor none -border 33%x30% -gravity South -chop 0%x0% -resize x400 kth_logo_blue.png kth_logo_blue_spaced.png

convert -alpha set -bordercolor none -border 33%x30% -gravity South -chop 0%x0% -resize x400 kth_logo_white.png kth_logo_white_spaced.png

convert -alpha set -bordercolor none -border 33%x30% -gravity South -chop 0%x0% -resize x400 kth_logo_black.png kth_logo_black_spaced.png

