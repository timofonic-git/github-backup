if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <S-Left> :cprev
imap <S-Right> :cnext
imap <C-Left> :popa
imap <C-Right> a
inoremap <expr> <C-Up> HaskellKeyword()
imap <C-Down> :w:SyntasticChecka
noremap  :make
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cfile>"),0)
noremap <S-Left> :cprev
noremap <S-Right> :cnext
noremap <C-Left> :pop
noremap <C-Right> 
noremap <C-Down> :w:SyntasticCheck
inoremap <expr>  HaskellKeyword()
iabbr jjjj --[[Joey]]
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autowrite
set backspace=indent,eol,start
set backup
set backupdir=~/tmp
set fileencodings=utf-8
set guicursor=n:blinkon0
set helplang=en
set history=50
set ignorecase
set incsearch
set iskeyword=a-z,A-Z,_,.,39
set nojoinspaces
set laststatus=2
set lazyredraw
set pastetoggle=<C-P>
set printoptions=paper:letter
set ruler
set runtimepath=~/.vim,~/.vim/bundle/syntastic,~/.vim/bundle/vim-fugitive,~/.vim/bundle/vim-hdevtools,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after
set shortmess=filnxtToOI
set showcmd
set showmatch
set smartcase
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tags=.tags,tags
set textwidth=75
set viminfo='20,\"1000
set visualbell
set wildmenu
" vim: set ft=vim :
