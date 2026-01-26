" Vim syntax file
" Language: Hlocker
" Maintainer: ShrykeWindgrace
" Latest Revision: 26 January 2024

if exists('b:current_syntax')
  finish
endif

syn match hlockerComment '[;#].*$'
syn match hlockerAccount '[^;#]*' contained
syn match hlockerDate '\d\{4}-\d\{2}-\d\{2}' nextgroup=hlockerAccount skipwhite
syn match hlockerKeyword 'close\|open' nextgroup=hlockerDate skipwhite

hi def link hlockerComment Comment
hi def link hlockerAccount Identifier
hi def link hlockerDate Number
hi def link hlockerKeyword Keyword

