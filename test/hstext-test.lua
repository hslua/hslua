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
assert(hstext.reverse 'être' == 'ertê')

local hw = 'Hello, World'
assert(string.sub(hw, 6)      == hstext.sub(hw, 6))
assert(string.sub(hw, -1, -1) == hstext.sub(hw, -1, -1))
assert(string.sub(hw, -7, -2) == hstext.sub(hw, -7, -2))
assert(string.sub(hw,  7, -2) == hstext.sub(hw, 7, -2))
assert(string.sub(hw,  1,  5) == hstext.sub(hw, 1, 5))
assert(string.sub(hw, -19, 5) == hstext.sub(hw, -19, 5))
