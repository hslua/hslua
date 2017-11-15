--
-- Tests for the hstext module
--
local hstext = require 'hstext'

assert(hstext.lower 'YELLING' == 'yelling')
assert(hstext.upper 'silence' == 'SILENCE')
assert(hstext.len 'five' == 4)

-- Test UTF-8
assert(hstext.upper 'Lübeck' == 'LÜBECK')
assert(hstext.upper 'Spaß' == 'SPASS')

assert(hstext.len 'Straße' == 6)
assert(hstext.len 'Charité' == 7)
assert(hstext.len '☃' == 1)

assert(hstext.reverse 'être' == 'ertê')
