.PHONY: clean

teaser.gif: palette.png original.flv teaser-start.txt teaser-length.txt
	ffmpeg -y -ss `cat teaser-start.txt` -t `cat teaser-length.txt` -i original.flv -i palette.png -filter_complex "scale=256:-1:flags=neighbor[x];[x][1:v]paletteuse" $@
	@echo
	@echo ======================
	@echo IMPORTANT INSTRUCTIONS
	@echo ======================
	@echo
	@echo Now open $@ in gimp, do File -\> Export as..., and set all the frame durations to 16ms. See also https://superuser.com/q/1459724/139240\; perhaps one day we can automate this.

palette.png: original.flv
	ffmpeg -y -ss `cat teaser-start.txt` -t `cat teaser-length.txt` -i original.flv -vf "scale=256:-1:flags=neighbor,palettegen" $@

clean:
	-rm teaser.gif palette.png
