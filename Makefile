.PHONY: clean release

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

release.zip: bokrifulse.lua config README.md $(wildcard problems/*) $(wildcard config.*)
	-rm release.zip
	zip $@ $^

release: release.zip
	@if [ -z "$$version" ]; \
		then \
			echo Pick a version number. The latest release zip is currently:; \
			basename `realpath ~/public_html/bokrifulse/00-bokrifulse-latest.zip`; \
			echo Then run \`version=whatever make release\`.; \
			exit 1; \
		fi
	cp release.zip ~/public_html/bokrifulse/bokrifulse-"$$version".zip
	ln -sf bokrifulse-"$$version".zip ~/public_html/bokrifulse/00-bokrifulse-latest.zip
	unison hcoop

clean:
	-rm teaser/teaser.gif teaser/palette.png release.zip
