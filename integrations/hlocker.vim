" Vim syntax file
" Language: Hlocker
" Maintainer: ShrykeWindgrace
" Latest Revision: 03 July 2026

if exists('b:current_syntax')
  finish
endif

syn match hlockerComment '^[;#].*'
syn match hlockerAccount '[^;#]*' contained
syn match hlockerDate '\d\{4}[-/.]\d\{1,2}[-/.]\d\{1,2}' contained nextgroup=hlockerAccount skipwhite
syn match hlockerKeyword '\c^\%(close\|open\)\>' nextgroup=hlockerDate skipwhite

hi def link hlockerComment Comment
hi def link hlockerAccount Identifier
hi def link hlockerDate Number
hi def link hlockerKeyword Keyword

let b:current_syntax = "hlocker"
