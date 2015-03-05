
"  ██████╗ ███████╗███╗   ██╗███████╗██████╗  █████╗ ██╗
" ██╔════╝ ██╔════╝████╗  ██║██╔════╝██╔══██╗██╔══██╗██║
" ██║  ███╗█████╗  ██╔██╗ ██║█████╗  ██████╔╝███████║██║
" ██║   ██║██╔══╝  ██║╚██╗██║██╔══╝  ██╔══██╗██╔══██║██║
" ╚██████╔╝███████╗██║ ╚████║███████╗██║  ██║██║  ██║███████╗
"  ╚═════╝ ╚══════╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝
" MacVim menu customizations are in .gvimrc. They wouldn't work here

execute pathogen#infect()
call pathogen#helptags()

""" All things modern and very general stuff
set nocompatible                       " Give up vi compatibility
filetype plugin indent on
syntax on
set encoding=utf-8
set shell=zsh\ -i
set ttyfast                            " Fast terminal we have
set tags=./tags;$HOME				   " Look for tags in . then search down the tree until ~

""" Appearance and visual helpers
colorscheme jellybeans
set cursorline                         " Highlight current line
set guifont=Menlo\ Regular\ for\ Powerline:h15
set guioptions=mgt                     " Menu, tearoff items, tearoff items. No toolbar, no scrollbars.
set laststatus=2                       " Always show a status bar
set nofoldenable                       " Disable folding by default
set relativenumber                     " Show *relative* line numbers
set scrolloff=3                        " Minimal number of screen lines to keep above and below the cursor
set visualbell                         " Visual beep in terminals
set wildmenu                           " Command-line completion menu
set wildmode=list:longest,full         " Autocomplete as much as possible + show full menu

""" Editing
set backspace=indent,eol,start         " In insert mode, allow backspace over autoindent, line breaks (join) and start of insert
set undofile                           " Create undofiles
set showmatch                          " Show matching brackets
set listchars=tab:▸\ ,eol:¬            " Show invisibles the TextMate way

""" Indenting
set autoindent                         " Enable auto-indentation
set tabstop=4                          " Tab = 4 spaces
set softtabstop=4                      " Tab = 4 spaces
set shiftwidth=4                       " Indent/deindent = 4 spaces
set smarttab                           " Be smart, somehow

""" Search and replace
set ignorecase                         " Search is case-insensitive by default
set smartcase                          " ...unless there are uppercase characters in the search pattern
set gdefault                           " Global substitution by default (replace all)
set incsearch                          " Show matches in realtime
set hlsearch                           " Highlight previous search matches
" Leader-space clears search results
nnoremap <leader><space> :noh<cr>

" ███╗   ███╗ █████╗ ██████╗ ██████╗ ██╗███╗   ██╗ ██████╗ ███████╗
" ████╗ ████║██╔══██╗██╔══██╗██╔══██╗██║████╗  ██║██╔════╝ ██╔════╝
" ██╔████╔██║███████║██████╔╝██████╔╝██║██╔██╗ ██║██║  ███╗███████╗
" ██║╚██╔╝██║██╔══██║██╔═══╝ ██╔═══╝ ██║██║╚██╗██║██║   ██║╚════██║
" ██║ ╚═╝ ██║██║  ██║██║     ██║     ██║██║ ╚████║╚██████╔╝███████║
" ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝     ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚══════╝
" Mappings

inoremap $$ <esc>
inoremap <D-o> <esc>:CtrlP<cr>
nnoremap <D-o> :CtrlP<cr>

" ██████╗ ██╗     ██╗   ██╗ ██████╗ ██╗███╗   ██╗███████╗
" ██╔══██╗██║     ██║   ██║██╔════╝ ██║████╗  ██║██╔════╝
" ██████╔╝██║     ██║   ██║██║  ███╗██║██╔██╗ ██║███████╗
" ██╔═══╝ ██║     ██║   ██║██║   ██║██║██║╚██╗██║╚════██║
" ██║     ███████╗╚██████╔╝╚██████╔╝██║██║ ╚████║███████║
" ╚═╝     ╚══════╝ ╚═════╝  ╚═════╝ ╚═╝╚═╝  ╚═══╝╚══════╝
" Plugins

" ╔═╗┬┬─┐┬  ┬┌┐┌┌─┐
" ╠═╣│├┬┘│  ││││├┤
" ╩ ╩┴┴└─┴─┘┴┘└┘└─┘

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme="powerlineish"

" ╔═╗┬ ┬┌┬┐┌─┐╔╦╗┌─┐┌─┐
" ╠═╣│ │ │ │ │ ║ ├─┤│ ┬
" ╩ ╩└─┘ ┴ └─┘ ╩ ┴ ┴└─┘

if has("mac")
	" Homebrew exuberant ctags and not BSD ctags
	let g:CtagsCmd = '/usr/local/bin/ctags'
endif

" ╔═╗┌┬┐┬─┐┬  ╔═╗
" ║   │ ├┬┘│  ╠═╝
" ╚═╝ ┴ ┴└─┴─┘╩

" Use ag
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" ╔═╗┬  ┌─┐┌┐┌┌─┐   ╔═╗┌─┐┌┬┐┌─┐┬  ┌─┐┌┬┐┌─┐
" ║  │  ├─┤││││ ┬───║  │ ││││├─┘│  ├┤  │ ├┤
" ╚═╝┴─┘┴ ┴┘└┘└─┘   ╚═╝└─┘┴ ┴┴  ┴─┘└─┘ ┴ └─┘

if has("mac")
	let s:clang_library_path='/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/'
endif
if isdirectory(s:clang_library_path)
    let g:clang_library_path=s:clang_library_path
endif

" ╔╦╗┬┌┬┐  ╦┌┐┌┌─┐┌─┐┌┬┐┬┬  ┬┌─┐
"  ║║││││  ║│││├─┤│   │ │└┐┌┘├┤
" ═╩╝┴┴ ┴  ╩┘└┘┴ ┴└─┘ ┴ ┴ └┘ └─┘

let g:diminactive_use_syntax      = 0
let g:diminactive_use_colorcolumn = 0

" ╔═╗┌─┐┌─┐┬ ┬╔╦╗┌─┐┌─┐┌─┐
" ║╣ ├─┤└─┐└┬┘ ║ ├─┤│ ┬└─┐
" ╚═╝┴ ┴└─┘ ┴  ╩ ┴ ┴└─┘└─┘

let g:easytags_async           = 0
let g:easytags_auto_highlight  = 0
let g:easytags_auto_update     = 1
let g:easytags_dynamic_files   = 1
let g:easytags_file            = '~/._vimtags'
let g:easytags_include_members = 1
let g:easytags_suppress_report = 1

" ╦  ┬┌┬┐┌─┐┬  ┬┌─┐┬ ┬┌┬┐
" ║  ││││├┤ │  ││ ┬├─┤ │
" ╩═╝┴┴ ┴└─┘┴─┘┴└─┘┴ ┴ ┴

let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:limelight_conceal_guifg   = 'DarkGray'
let g:limelight_conceal_guifg   = '#777777'

autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight!

" ╔╦╗┌─┐┌─┐╔╗ ┌─┐┬─┐
"  ║ ├─┤│ ┬╠╩╗├─┤├┬┘
"  ╩ ┴ ┴└─┘╚═╝┴ ┴┴└─

nnoremap <F8> :Tagbar<cr>
imap <F8> <esc>:Tagbar<cr>ih

" ███████╗██╗██╗     ███████╗████████╗██╗   ██╗██████╗ ███████╗███████╗
" ██╔════╝██║██║     ██╔════╝╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝
" █████╗  ██║██║     █████╗     ██║    ╚████╔╝ ██████╔╝█████╗  ███████╗
" ██╔══╝  ██║██║     ██╔══╝     ██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ╚════██║
" ██║     ██║███████╗███████╗   ██║      ██║   ██║     ███████╗███████║
" ╚═╝     ╚═╝╚══════╝╚══════╝   ╚═╝      ╚═╝   ╚═╝     ╚══════╝╚══════╝
" Filetypes

" ╔╦╗┌─┐┬─┐┬┌─┌┬┐┌─┐┬ ┬┌┐┌     ╔╦╗╦╔═╔╦╗
" ║║║├─┤├┬┘├┴┐ │││ │││││││ aka ║║║╠╩╗ ║║
" ╩ ╩┴ ┴┴└─┴ ┴─┴┘└─┘└┴┘┘└┘     ╩ ╩╩ ╩═╩╝

let g:vim_markdown_math=1
let g:vim_markdown_frontmatter=1

" Ascii Fonts:
"	Heading 1: ANSI Shadow
"	Heading 2: Calvin S
