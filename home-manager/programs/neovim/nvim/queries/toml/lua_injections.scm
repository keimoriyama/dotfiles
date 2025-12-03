;; lua_add = ''' ... ''' を Lua としてインジェクト
((pair
  (bare_key) @_key
  (string) @lua_code)
 (#eq? @_key "lua_add")
 (#set! injection.language "lua"))
