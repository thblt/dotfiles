filetype off                                "  Vundle wants this
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'                  "  Vundle itself

Plugin 'bling/vim-airline'                  "  Status line
Plugin 'dahu/vim-lotr'                      "  Persistent view of registers contents in sidebar
Plugin 'gilligan/vim-lldb'                  "  lldb integration
Plugin 'godlygeek/tabular'                  "  For tabularization. Required by plasticboy/markdown
Plugin 'junegunn/goyo.vim'                  "  'Zenroom' mode
Plugin 'junegunn/limelight.vim'             "  With Goyo : fade contents outside the current paragraph
Plugin 'kien/ctrlp.vim'                     "  Fuzzy search
Plugin 'kien/rainbow_parentheses.vim'       "  Different colors for nested ({[etc
Plugin 'Lokaltog/vim-easymotion'            "  Improved motion commands
Plugin 'majutsushi/tagbar.git'              "  tags bar
Plugin 'mhinz/vim-startify'                 "  Startup screen with recent files
Plugin 'mileszs/ack.vim'                    "  Ack
Plugin 'nanotech/jellybeans.vim'            "  Color theme
Plugin 'octol/vim-cpp-enhanced-highlight'   "  Improved highlighting for C++
Plugin 'plasticboy/vim-markdown'            "  Improved markdown handling
Plugin 'Raimondi/delimitMate'               "  Autocompletion for quotes, parens…
Plugin 'rhysd/vim-clang-format'             "  Interface to clang_format
Plugin 'rstacruz/sparkup'                   "  HTML/XML Zen typing
Plugin 'scrooloose/nerdcommenter'           "  For comments
Plugin 'scrooloose/nerdtree'                "  Dir/file browsing
Plugin 'tpope/vim-fugitive'                 "  Git integration
Plugin 'tpope/vim-projectionist'            "  Mostly for 'alternate' files (.c/.h etc)
Plugin 'tpope/vim-repeat'                   "  Improved dot
Plugin 'tpope/vim-surround'                 "  Edit surroundings
Plugin 'jceb/vim-orgmode'
"Plugin 'chriskempson/base16-vim'
"Plugin 'craigemery/vim-autotag'            "  Automatic ctags invocation. Useless with clang completion.
"Plugin 'dhruvasagar/vim-table-mode'        "  Table editor
"Plugin 'vimoutliner/vimoutliner'           "  Outlining
if v:version >= 740
	Plugin 'honza/vim-snippets'                 "  Default snippets for UltiSnips
	Plugin 'SirVer/ultisnips'                   "  Snippets
	Plugin 'Valloric/YouCompleteMe'             "  Completion
endif

call vundle#end()

"  ██████╗ ███████╗███╗   ██╗███████╗██████╗  █████╗ ██╗
" ██╔════╝ ██╔════╝████╗  ██║██╔════╝██╔══██╗██╔══██╗██║
" ██║  ███╗█████╗  ██╔██╗ ██║█████╗  ██████╔╝███████║██║
" ██║   ██║██╔══╝  ██║╚██╗██║██╔══╝  ██╔══██╗██╔══██║██║
" ╚██████╔╝███████╗██║ ╚████║███████╗██║  ██║██║  ██║███████╗
"  ╚═════╝ ╚══════╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝

""" All things modern and very general stuff
set nocompatible                                "  Give up vi compatibility
filetype plugin indent on
syntax on
set encoding=utf-8
if has("gui")
	set shell=bash\ -i                          "  zsh is *weird* on GUI Vim.
endif
set ttyfast                                     "  Fast terminal we have
set tags=./tags;$HOME                           "  Look for tags in . then search down the tree until ~

""" Appearance and visual helpers
colorscheme jellybeans
set cursorline                                  "  Highlight current line
set guifont=Menlo\ Regular\ for\ Powerline:h15
set guioptions=mgt                              "  Menu, tearoff items, tearoff items. No toolbar, no scrollbars.
set laststatus=2                                "  Always show a status bar
set noshowmode                                  "  Don't show current mode (Airline does it)
set number                                      "  Show line numbers
set relativenumber                              "  Show *relative* line numbers (with `number`, shows absolute number at current line instead of 0)
set scrolloff=3                                 "  Minimal number of screen lines to keep above and below the cursor
set visualbell                                  "  Visual beep in terminals
set wildmenu                                    "  Command-line completion menu
set wildmode=list:longest,full                  "  Autocomplete as much as possible + show full menu

""" Editing
set autoread                                    "  Reload files changed outside vim
set backspace=indent,eol,start                  "  In insert mode, allow backspace over autoindent, line breaks (join) and start of insert
set hidden                                      "  Do not close abandoned buffers
set listchars=tab:▸\ ,eol:¬                     "  Show invisibles the TextMate way
set nofoldenable                                "  Disable folding by default
set nowrap                                      "  Disable by default, reenable for specifics filetypes
set undofile                                    "  Create undofiles
set showmatch                                   "  Show matching brackets

""" Indenting
set autoindent                                  "  Enable auto-indentation
set tabstop=4                                   "  Tab = 4 spaces
set softtabstop=4                               "  Tab = 4 spaces
set shiftwidth=4                                "  Indent/deindent = 4 spaces
set smarttab                                    "  Be smart, somehow

""" Search and replace
set ignorecase                                  "  Search is case-insensitive by default
set smartcase                                   "  ...unless there are uppercase characters in the search pattern
set gdefault                                    "  Global substitution by default (replace all)
set incsearch                                   "  Show matches in realtime
set hlsearch                                    "  Highlight previous search matches

" ███╗   ███╗ █████╗ ██████╗ ██████╗ ██╗███╗   ██╗ ██████╗ ███████╗
" ████╗ ████║██╔══██╗██╔══██╗██╔══██╗██║████╗  ██║██╔════╝ ██╔════╝
" ██╔████╔██║███████║██████╔╝██████╔╝██║██╔██╗ ██║██║  ███╗███████╗
" ██║╚██╔╝██║██╔══██║██╔═══╝ ██╔═══╝ ██║██║╚██╗██║██║   ██║╚════██║
" ██║ ╚═╝ ██║██║  ██║██║     ██║     ██║██║ ╚████║╚██████╔╝███████║
" ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝     ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚══════╝
" Mappings
" MacVim menu customizations are in .gvimrc. They wouldn't work here

" Leader-space clears search results
nnoremap <leader><space> :noh<cr>
"if has("mac")
  "nnoremap <M-left> :bprevious<cr>
  "nnoremap <M-right> :bnext<cr>
"endif
augroup thblt
	au!
	au FocusLost * wa                             "  Save on losing focus
	au FocusLost,TabLeave * call feedkeys("\<C-\>\<C-n>") "  Return to normal mode on losing focus
	" au FocusLost,TabLeave * stopinsert doesn't work: http://stackoverflow.com/questions/2968548/vim-return-to-command-mode-when-focus-is-lost
augroup END

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
let g:airline_theme = "powerlineish"

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
let g:ctrlp_map = '<D-o>'
let g:ctrlp_cmd = 'CtrlP'

" Use ag
if executable('ag')
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" ╔═╗┌─┐┌─┐┬ ┬╔╦╗┌─┐┌─┐┌─┐
" ║╣ ├─┤└─┐└┬┘ ║ ├─┤│ ┬└─┐
" ╚═╝┴ ┴└─┘ ┴  ╩ ┴ ┴└─┘└─┘
let g:easytags_async = 0
let g:easytags_auto_highlight = 0
let g:easytags_auto_update = 1
let g:easytags_dynamic_files = 1
let g:easytags_file = '~/._vimtags'
let g:easytags_include_members = 1
let g:easytags_suppress_report = 1

" ╦  ┬┌┬┐┌─┐┬  ┬┌─┐┬ ┬┌┬┐
" ║  ││││├┤ │  ││ ┬├─┤ │
" ╩═╝┴┴ ┴└─┘┴─┘┴└─┘┴ ┴ ┴
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_conceal_guifg = '#777777'

autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight!

" ╔═╗┬─┐┌─┐ ┬┌─┐┌─┐┌┬┐┬┌─┐┌┐┌┬┌─┐┌┬┐
" ╠═╝├┬┘│ │ │├┤ │   │ ││ │││││└─┐ │
" ╩  ┴└─└─┘└┘└─┘└─┘ ┴ ┴└─┘┘└┘┴└─┘ ┴
let g:projectionist_heuristics = {
	\ "*": {
		\ "*.cpp": { "alternate": "{}.hpp" },
		\ "*.hpp": { "alternate": "{}.cpp" },
		\ "*.c":   { "alternate": "{}.h" },
		\ "*.h":   { "alternate": "{}.c" },
	\}}
"
" ╦ ╦┬ ┌┬┐┬╔═╗┌┐┌┬┌─┐┌─┐
" ║ ║│  │ │╚═╗││││├─┘└─┐
" ╚═╝┴─┘┴ ┴╚═╝┘└┘┴┴  └─┘
let g:UltiSnipsExpandTrigger="<C-tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" ╦ ╦┌─┐┬ ┬╔═╗┌─┐┌┬┐┌─┐┬  ┌─┐┌┬┐┌─┐╔╦╗┌─┐
" ╚╦╝│ ││ │║  │ ││││├─┘│  ├┤  │ ├┤ ║║║├┤
"  ╩ └─┘└─┘╚═╝└─┘┴ ┴┴  ┴─┘└─┘ ┴ └─┘╩ ╩└─┘
let g:ycm_auto_trigger=0
let g:ycm_use_ultisnips_completer=1
let g:ycm_autoclose_preview_window_after_insertion=1

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
