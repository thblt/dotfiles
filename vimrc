execute pathogen#infect()
syntax on
filetype plugin indent on

set go-=T
colorscheme Tomorrow-Night
set guifont=Menlo:h14

""" Indenting
set autoindent
set softtabstop=4
set shiftwidth=4
set tabstop=4
set expandtab
set smarttab

""" Editing visual aids
set number
set cursorline
set showmatch
set ruler
set nofoldenable

""" Useful stuff
set encoding=utf-8
set nocompatible
set scrolloff=3
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2
set relativenumber
set undofile

""" Dim inactive windows
let g:diminactive_use_syntax = 1
let g:diminactive_use_colorcolumn = 1

let s:clang_library_path='/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/'
if isdirectory(s:clang_library_path)
    let g:clang_library_path=s:clang_library_path
endif
