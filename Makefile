README.md: docs/index.md tools/replace-readme-body.lua
	pandoc $@ \
		--to=gfm-smart \
		--lua-filter=tools/replace-readme-body.lua \
		--markdown-headings=setext \
		--metadata=bodyfile=$< \
		--reference-links \
		--reference-location=section \
		--columns=66 \
		--output=$@
