#!/bin/bash
mencoder -nosound mf://*.jpg -mf w=650:h=650:type=jpg:fps=7 -ovc lavc -lavcopts vcodec=mpeg4:mbd=2:keyint=132:v4mv:vqmin=3 -o ant.avi
rm *.jpg
