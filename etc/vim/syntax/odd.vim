if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "odd"

syntax keyword oddSpecialForms def lambda quote set brace if module public private import
hi link oddSpecialForms Keyword

syntax keyword oddCommonFunctions not
hi link oddCommonFunctions Function

syntax match oddComment "\v\/\/.*$"
highlight link oddComment Comment
