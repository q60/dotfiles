" PLUGINS "
call plug#begin()
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'ap/vim-css-color'
Plug 'stevearc/vim-arduino'
Plug 'junegunn/fzf'
Plug 'scrooloose/nerdtree'                " project and file navigation
Plug 'Xuyuanp/nerdtree-git-plugin'        " NerdTree git functionality
Plug 'majutsushi/tagbar'                  " class/module browser
Plug 'vim-ctrlspace/vim-ctrlspace'        " tabs/buffers/fuzzy/workspaces/bookmarks
Plug 'mileszs/ack.vim'                    " ag/grep
Plug 'fisadev/FixedTaskList.vim'          " pending tasks list
Plug 'yuttie/comfortable-motion.vim'      " smooth scrolling
Plug 'MattesGroeger/vim-bookmarks'        " bookmarks
Plug 'tpope/vim-surround'                 " parentheses, brackets, quotes, XML tags, and more
Plug 'vimwiki/vimwiki'                    " personal Wiki
Plug 'jreybert/vimagit'                   " git operations
Plug 'jiangmiao/auto-pairs'
Plug 'mhinz/vim-startify'                 " vim start page
Plug 'garbas/vim-snipmate'                " snippets manager
Plug 'MarcWeber/vim-addon-mw-utils'       " dependencies #1
Plug 'tomtom/tlib_vim'                    " dependencies #2
Plug 'honza/vim-snippets'                 " snippets repo
Plug 'scrooloose/nerdcommenter'           " easy code documentation
Plug 'mitsuhiko/vim-sparkup'              " sparkup(XML/jinja/htlm-django/etc.) support
Plug 'klen/python-mode'                   " Python mode (docs, refactor, lints...)
Plug 'jmcantrell/vim-virtualenv'
Plug 'numirias/semshi', { 'do': ':UpdateRemotePlugins' }
Plug 'Shirk/vim-gas'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
call plug#end()

set hidden

" COLORS AND GUI "
colorscheme dracula
set termguicolors
" set background=dark
hi Normal guibg=none
set guifont=IBM\ Plex\ Mono:h20
if has("gui_running")
  let g:dracula_italic = 0
endif

" AIRLINE "
let g:airline_theme='dracula'
if trim(system('echo $TERM')) != 'xterm-termite'
	let g:airline_powerline_fonts = 0
	let g:airline_left_sep = ''
	let g:airline_right_sep = ''
endif
let g:airline#extensions#tabline#enabled = 1

" MISC "
set relativenumber
au BufNewFile,BufRead * set tabstop=2
au BufNewFile,BufRead * set shiftwidth=2
set smarttab
set expandtab
set autoindent
set ruler
set ttyfast
set cursorline
set showmatch
map <F1> :q<CR>
map <F2> :w<CR>
map <F3> :source %<CR>
map <C-o> :tabp<CR>
map <C-p> :tabn<CR>
au BufNewFile,BufRead *.S set filetype=gas

" NERD "
let g:NERDSpaceDelims = 1
" use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1
" add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
" allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '-'
map <C-c> :NERDTreeToggle<CR>


" YT LIST "
func Highlight()
	set filetype=yt-list
	let s:concealends = has('conceal') ? ' concealends' : ''
	sy match matchLineStart /^[<@]\@!/
	sy match matchURL /[https\?:\/\/[[:alnum:]%?=\/_#.-]*]/
	sy match matchElse /\s::\s.*$/
	exe 'syn region matchUnderline matchgroup=matchUnderlineDelimiter start="_" end="_" keepend contains=matchLineStart' . s:concealends
	exe 'syn region matchItalic matchgroup=matchItalicDelimiter start="`" end="`" keepend contains=matchLineStart' . s:concealends
	exe 'syn region matchBold matchgroup=matchBoldDelimiter start="\$" end="\$" keepend contains=matchLineStart' . s:concealends
  hi Normal          guifg=#2F9FF7
  hi matchUnderline  gui=underline
	hi matchItalic		 gui=italic
	hi matchBold			 gui=bold
  hi matchURL        guifg=#FF62C4 gui=bold
  hi matchElse       guifg=#FFAE62
  setl conceallevel=2
endf

func ReadUrl()
	let url = getline('.')
	let lineNu = 0
	while lineNu<line('$')
		let lineNu += 1
		let url = getline(lineNu)
		if url =~? '^https\?:\/\/[[:alnum:]%?=\/_#.-]*$'
			let urlFormed = '[' . url . ']'
			call setline(lineNu, urlFormed)
			let videoName = substitute(trim(system("wget '" . url . ''' -q -O - | cat | grep -oP ''(?<=\<meta itemprop="name" content=").*(?=">)''')), '[3 q', '', 'g')
			call setline(lineNu, urlFormed . ' :: ' . videoName)
		else
			if !url =~? '[https\?:\/\/[[:alnum:]%?=\/_#.-]*]'
				if !url =~? '#.*$'
					exe 1+lineNu 'delete _'
				endif
			endif
		endif
	endwhile
endf

au BufWritePre *.yt call ReadUrl()
au BufNewFile,BufRead *.yt call Highlight()

" ARDUINO "
let g:arduino_dir = '/usr/share/arduino'
func! MyStatusLine()
	let port = arduino#GetPort()
	let line = '[' . g:arduino_board . '] [' . g:arduino_programmer . ']'
	if !empty(port)
		let line = line . ' [' . port . ':' . g:arduino_serial_baud . ']'
	endif
	return line
endf
au BufNewFile,BufRead *.ino let g:airline_section_c = '%{MyStatusLine()}'
