" liva syntax file
" Language: Liva
" Maintainer: Patrick Haller

if exists("b:current_syntax")
    finish
endif

" Keywords
syn keyword basicLanguageKeywords return class end fun do while for if let in else external as import

" Comments
syn keyword todos TODO FIXME 
syn match comment "//.*$" contains=todos

" Numbers
syn match number '\d\+' contained display
syn match number '[-+]\d\+' contained display

" Floating point number with decimal no E or e (+,-)
syn match number '\d\+\.\d*' contained display
syn match number '[-+]\d\+\.\d*' contained display

" Strings
syn region string start='"' end='"' contained
syn region sesc start='"' end='"'

let b:current_syntax = "cel"

hi def link todos        Todo
hi def link comment      Comment
hi def link string       Constant
hi def link sesc         PreProc
hi def link number       Constant
