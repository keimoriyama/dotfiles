;; extends

(document
  (table_array_element 
  (pair 
   (bare_key) @key
   (string) @content)
   (#eq? @key "lua_source")
   (#set! injection.language "lua")))
