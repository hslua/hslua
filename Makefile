# The top-level README is generated from index.md.
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

# Other README files are generated from pages of the same name.
%/README.md: docs/%.md tools/replace-readme-body.lua
	pandoc $@ \
		--to=gfm-smart \
		--lua-filter=tools/replace-readme-body.lua \
		--markdown-headings=setext \
		--metadata=bodyfile=$< \
		--reference-links \
		--reference-location=section \
		--columns=66 \
		--output=$@

.PHONY: release-date
release-date:
	find . \
	    -name CHANGELOG.md \
	    -exec sed -i'' \
                -e "s/^Release pending/Released $$(date '+%d-%m-%Y')/" \
            '{}' ';'

.PHONY: publish
publish:
	for archive in $$(cabal sdist all | grep -v '^Wrote tarball sdist to'); do \
	    cabal upload "$$archive" --publish; \
	done
