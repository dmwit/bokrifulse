.PHONY: clean

teaser.gif: palette.png original.flv teaser-start.txt teaser-length.txt
	ffmpeg -y -ss `cat teaser-start.txt` -t `cat teaser-length.txt` -i original.flv -i palette.png -filter_complex "fps=50,scale=256:-1:flags=neighbor[x];[x][1:v]paletteuse" $@

palette.png: original.flv
	ffmpeg -y -ss `cat teaser-start.txt` -t `cat teaser-length.txt` -i original.flv -vf "scale=256:-1:flags=neighbor,palettegen" $@

clean:
	-rm teaser.gif palette.png
