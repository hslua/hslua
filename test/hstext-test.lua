--
-- Tests for the hstext module
--
local hstext = require 'hstext'

assert(hstext.lower 'YELLING' == 'yelling')
assert(hstext.upper 'silence' == 'SILENCE')

-- Test UTF-8
assert(hstext.upper 'Lübeck' == 'LÜBECK')
assert(hstext.upper 'Spaß' == 'SPASS')
