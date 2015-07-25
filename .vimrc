filetype off                                "  Vundle wants this
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'                " Vundle itself

Plugin 'kien/ctrlp.vim'                   " Fuzzy search
Plugin 'mileszs/ack.vim'                  " Ack
Plugin 'tpope/vim-fugitive'               " Git integration
" Look and feel
Plugin 'bling/vim-airline'                " Status line
Plugin 'junegunn/goyo.vim'                " 'Zenroom' mode
Plugin 'junegunn/limelight.vim'           " With Goyo : fade contents outside the current paragraph
Plugin 'mhinz/vim-startify'               " Startup screen with recent files
Plugin 'nanotech/jellybeans.vim'          " Jellybeans theme
Plugin 'scrooloose/nerdtree'              " Dir/file browsing
" Editing
Plugin 'chrisbra/NrrwRgn'                 " Narrow region à la Emacs
Plugin 'danro/rename.vim'                 " Rename on disk
Plugin 'godlygeek/tabular'                " For tabularization. Required by plasticboy/markdown
Plugin 'jceb/vim-orgmode'                 " Emacs' org-mode clone
Plugin 'kien/rainbow_parentheses.vim'     " Different colors for nested ({[etc
Plugin 'Lokaltog/vim-easymotion'          " Improved motion commands
Plugin 'plasticboy/vim-markdown'          " Improved markdown handling
Plugin 'Raimondi/delimitMate'             " Autocompletion for quotes, parens…
Plugin 'rstacruz/sparkup'                 " HTML/XML Zen typing
Plugin 'sjl/gundo.vim'                    " Undo tree
Plugin 'tpope/vim-abolish'                " working with variants of a word
Plugin 'tpope/vim-speeddating'            " c-a c-x for dates
Plugin 'vim-scripts/YankRing.vim'         " Yank registers manipulation
" Programming
Plugin 'gilligan/vim-lldb'                " lldb integration
Plugin 'majutsushi/tagbar.git'            " tags bar
Plugin 'rhysd/vim-clang-format'           " Interface to clang_format
Plugin 'scrooloose/nerdcommenter'         " For comments
Plugin 'scrooloose/syntastic'             " Syntax checking
Plugin 'tpope/vim-projectionist'          " Mostly for 'alternate' files (.c/.h etc)
Plugin 'tpope/vim-repeat'                 " Improved dot (allows repeating plugin actions. Transparent)
Plugin 'tpope/vim-surround'               " Edit surroundings
" Syntaxes
Plugin 'digitaltoad/vim-jade'             " Jade templates
Plugin 'octol/vim-cpp-enhanced-highlight' " Improved highlighting for C++
Plugin 'tmhedberg/SimpylFold'             " Folding for Python
" Documentation
Plugin 'mrtazz/DoxygenToolkit.vim'        " Doxygen
" Snippets and completion
if v:version >= 704
	Plugin 'SirVer/ultisnips'             " Snippets
	Plugin 'honza/vim-snippets'           " Default snippets for UltiSnips
	Plugin 'Valloric/YouCompleteMe'       " Completion
endif
"Plugin 'chriskempson/base16-vim'
"Plugin 'craigemery/vim-autotag'            "  Automatic ctags invocation. Useless with clang completion.
"Plugin 'dhruvasagar/vim-table-mode'        "  Table editor
"Plugin 'vimoutliner/vimoutliner'           "  Outlining

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
" Note on colorschema/hightlighting listchars:
" Two highlight groups: SpecialKey and NonText
highlight specialKey guibg='NONE'
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
set foldenable                                  "  Disable folding by default
set foldmethod=syntax
set foldlevelstart=20                           "  All folds open by default
set nowrap                                      "  Disable by default, reenable for specifics filetypes
set undofile                                    "  Create undofiles
set showmatch                                   "  Show matching brackets

""" Indenting
set tabstop=4                                   "  Tab = 4 spaces
set softtabstop=4                               "  Tab = 4 spaces
set shiftwidth=4                                "  Indent/deindent = 4 spaces
set smarttab                                    "  Be smart, somehow

""" Search and replace
set ignorecase                                  "  Search is case-insensitive by default…
set smartcase                                   "  ...unless there are uppercase characters in the search pattern
set gdefault                                    "  Global substitution by default (replace all)
set incsearch                                   "  Show matches in realtime
set hlsearch                                    "  Highlight previous search matches

""" Programming
set makeprg=wmake\ %:p:h

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

" Cycle buffers with ctrl-(shift-)-tab
nnoremap <c-tab> :bnext<cr>
nnoremap <c-s-tab> :bprevious<cr>

" Numbers without shift in normal mode on
" French AZERTY keyboard.
nnoremap & 1
nnoremap é 2
nnoremap " 3
nnoremap ' 4
nnoremap ( 5
nnoremap § 6
nnoremap è 7
nnoremap ! 8
nnoremap ç 9
nnoremap à 0
" Reversing 
nnoremap 1 &
nnoremap 2 é
nnoremap 3 "
nnoremap 4 '
nnoremap 5 (
nnoremap 6 §
nnoremap 7 è
nnoremap 8 !
nnoremap 9 ç
nnoremap 0 à

augroup thblt
	au!
	au FocusLost * :silent! wa                                     "  Save on losing focus
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
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#fnamecollapse=0
let g:airline#extensions#tabline#fnametruncate=0
let g:airline_theme = "powerlineish"
let g:airline#extensions#ctrlp#show_adjacent_modes = 1

" ╔═╗┌┬┐┬─┐┬  ╔═╗
" ║   │ ├┬┘│  ╠═╝
" ╚═╝ ┴ ┴└─┴─┘╩
let g:ctrlp_map = '<D-o>'
let g:ctrlp_cmd = 'CtrlP .'

" Use ag
if executable('ag')
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  " let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
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

" ╔╦╗┌─┐┬─┐┬┌─┌┬┐┌─┐┬ ┬┌┐┌     ╔╦╗╦╔═╔╦╗
" ║║║├─┤├┬┘├┴┐ │││ │││││││ aka ║║║╠╩╗ ║║
" ╩ ╩┴ ┴┴└─┴ ┴─┴┘└─┘└┴┘┘└┘     ╩ ╩╩ ╩═╩╝
let g:vim_markdown_math=1
let g:vim_markdown_frontmatter=1

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

" ╔╦╗┌─┐┌─┐╔╗ ┌─┐┬─┐
"  ║ ├─┤│ ┬╠╩╗├─┤├┬┘
"  ╩ ┴ ┴└─┘╚═╝┴ ┴┴└─
nmap <F8> :TagbarToggle<CR>
"
" ╔═╗┬ ┬┌┐┌┌┬┐┌─┐┌─┐┌┬┐┬┌─┐
" ╚═╗└┬┘│││ │ ├─┤└─┐ │ ││
" ╚═╝ ┴ ┘└┘ ┴ ┴ ┴└─┘ ┴ ┴└─┘
let g:syntastic_mode_map = { "mode": "passive" }

" ╦ ╦┬ ┌┬┐┬╔═╗┌┐┌┬┌─┐┌─┐
" ║ ║│  │ │╚═╗││││├─┘└─┐
" ╚═╝┴─┘┴ ┴╚═╝┘└┘┴┴  └─┘
let g:UltiSnipsExpandTrigger='<c-e>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

" ╦ ╦┌─┐┬ ┬╔═╗┌─┐┌┬┐┌─┐┬  ┌─┐┌┬┐┌─┐╔╦╗┌─┐
" ╚╦╝│ ││ │║  │ ││││├─┘│  ├┤  │ ├┤ ║║║├┤
"  ╩ └─┘└─┘╚═╝└─┘┴ ┴┴  ┴─┘└─┘ ┴ └─┘╩ ╩└─┘
let g:ycm_global_ycm_extra_conf="~/.vim/.ycm_extra_conf.py"
"let g:ycm_auto_trigger=2
"let g:ycm_use_ultisnips_completer=1 " default: 1
"let g:ycm_autoclose_preview_window_after_insertion=1
"nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>

" ███████╗██╗   ██╗███╗   ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
" ██╔════╝██║   ██║████╗  ██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
" █████╗  ██║   ██║██╔██╗ ██║██║        ██║   ██║██║   ██║██╔██╗ ██║███████╗
" ██╔══╝  ██║   ██║██║╚██╗██║██║        ██║   ██║██║   ██║██║╚██╗██║╚════██║
" ██║     ╚██████╔╝██║ ╚████║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║███████║
" ╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝

" delete the buffer; keep windows; create a scratch buffer if no buffers left
" (from somewhere in vim wiki?)
function! s:Kwbd(kwbdStage)
  if(a:kwbdStage == 1)
    if(!buflisted(winbufnr(0)))
      bd!
      return
    endif
    let s:kwbdBufNum = bufnr("%")
    let s:kwbdWinNum = winnr()
    windo call s:Kwbd(2)
    execute s:kwbdWinNum . 'wincmd w'
    let s:buflistedLeft = 0
    let s:bufFinalJump = 0
    let l:nBufs = bufnr("$")
    let l:i = 1
    while(l:i <= l:nBufs)
      if(l:i != s:kwbdBufNum)
        if(buflisted(l:i))
          let s:buflistedLeft = s:buflistedLeft + 1
        else
          if(bufexists(l:i) && !strlen(bufname(l:i)) && !s:bufFinalJump)
            let s:bufFinalJump = l:i
          endif
        endif
      endif
      let l:i = l:i + 1
    endwhile
    if(!s:buflistedLeft)
      if(s:bufFinalJump)
        windo if(buflisted(winbufnr(0))) | execute "b! " . s:bufFinalJump | endif
      else
        enew
        let l:newBuf = bufnr("%")
        windo if(buflisted(winbufnr(0))) | execute "b! " . l:newBuf | endif
      endif
      execute s:kwbdWinNum . 'wincmd w'
    endif
    if(buflisted(s:kwbdBufNum) || s:kwbdBufNum == bufnr("%"))
      execute "bd! " . s:kwbdBufNum
    endif
    if(!s:buflistedLeft)
      set buflisted
      set bufhidden=delete
      set buftype=
      setlocal noswapfile
    endif
  else
    if(bufnr("%") == s:kwbdBufNum)
      let prevbufvar = bufnr("#")
      if(prevbufvar > 0 && buflisted(prevbufvar) && prevbufvar != s:kwbdBufNum)
        b #
      else
        bn
      endif
    endif
  endif
endfunction

command! Kwbd call s:Kwbd(1)
nnoremap <silent> <Plug>Kwbd :<C-u>Kwbd<CR>

" Create a mapping (e.g. in your .vimrc) like this:
"nmap <C-W>! <Plug>Kwbd

" Ascii Fonts:
"	Heading 1: ANSI Shadow
"	Heading 2: Calvin S
