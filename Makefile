.PHONY: clean

all: release.zip teaser/teaser.gif

release.zip: bokrifulse.lua config README.md $(wildcard problems/*)
	-rm release.zip
	zip $@ $^

teaser/teaser.gif: teaser/palette.png teaser/original.flv teaser/start.txt teaser/length.txt
	ffmpeg -y \
		-ss `cat teaser/start.txt` \
		-t `cat teaser/length.txt` \
		-i teaser/original.flv \
		-i teaser/palette.png \
		-filter_complex "fps=50,scale=256:-1:flags=neighbor[x];[x][1:v]paletteuse" \
		$@

teaser/palette.png: teaser/original.flv teaser/start.txt teaser/length.txt
	ffmpeg -y \
		-ss `cat teaser/start.txt` \
		-t `cat teaser/length.txt` \
		-i teaser/original.flv \
		-vf "scale=256:-1:flags=neighbor,palettegen" \
		$@

clean:
	-rm teaser/teaser.gif teaser/palette.png
