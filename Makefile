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
                -e "s/^Release pending/Released $$(date '+%Y-%m-%d')/" \
            '{}' ';'

.PHONY: publish
publish:
	cabal update
	for archive in $$(cabal sdist all | grep -v '^Wrote tarball sdist to'); do \
	    tagname=$$(basename "$$archive" | sed -e 's/\.tar\.gz//'); \
	    if ! cabal info -v0 "$$tagname" 2>&1 > /dev/null ; then \
	      cabal upload "$$archive" --publish ; \
	      git tag --sign --message="$$tagname" $$tagname ; \
	    fi \
	done
