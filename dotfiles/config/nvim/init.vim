set fileencoding=utf-8

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source %
endif

call plug#begin('~/.config/nvim/plugged')

" Solarized theme
Plug 'altercation/vim-colors-solarized'

" Zenburn theme
Plug 'zenburn'

" Syntastic
Plug 'scrooloose/syntastic'

" Git
Plug 'tpope/vim-fugitive'

" Easy Commenting
Plug 'scrooloose/nerdcommenter'

" Surround - Easy changing of quotes and stuff
Plug 'tpope/vim-surround'

" Ctrlp full path fuzzy file finder
Plug 'kien/ctrlp.vim'

" Ack from vim
Plug 'mileszs/ack.vim'

" Improved vim status bar
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Indent guides
Plug 'Yggdroot/indentLine'

" Autocomplete certain pairs (", ], etc)
Plug 'jiangmiao/auto-pairs'

" Language specific syntax highlighting
Plug 'derekwyatt/vim-scala'
Plug 'kchmck/vim-coffee-script'
Plug 'rust-lang/rust.vim'
Plug 'klen/python-mode'
Plug 'groenewege/vim-less'
Plug 'cakebaker/scss-syntax.vim'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'digitaltoad/vim-jade'
Plug 'wavded/vim-stylus'
Plug 'raichoo/haskell-vim'
Plug 'OCamlPro/ocp-indent'
Plug 'slim-template/vim-slim'
Plug 'fatih/vim-go'
Plug 'isRuslan/vim-es6'

" Org Mode
Plug 'jceb/vim-orgmode'

if !has("win32") || !has("win16")
  "Code Completion
  " Post-update hook for YCM
  " Make sure to sudo pip install neovim before running this
  function! BuildYCM(info)
    if a:info.status == 'installed' || a:info.force
      !./install.sh
    endif
  endfunction
  Plug 'Valloric/YouCompleteMe', { 'do': function('BuildYCM') }

  " Tree explorer
  "Instead of forcing vim to start Nerdtree just do it manually
  " autocmd vimenter * NERDTree  " make NERDTree come up automatically
  " on vim start

  Plug 'scrooloose/nerdtree'
  " For nerdtree indicators of what has changed
  Plug 'Xuyuanp/nerdtree-git-plugin'

  " Make Vim play nice with tmux
  " Use <c-h> <c-j> <c-k> <c-l>
  " to move between window panes in tmux or vim
  Plug 'christoomey/vim-tmux-navigator'
endif

" Markdown
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

" Snippet Support/Configuration
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

let g:UltiSnipsExpandTrigger="<c-s>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsSnippetsDir=["~/.config/nvim/plugged/vim-snippets/UltiSnips", "~/.config/nvim/snippets"]

" Allow UltiSnips to split the window vertically
let g:UltiSnipssEditSplit="vertical"

"""""""""""""""""""""""""""""""

" end plugin list
call plug#end()

" Standard variables
set expandtab                  " tabs to spaces
set tabstop=2                  " spaces entered when tab key is pressed
set shiftwidth=2               " spaces entered for indentation
set number                     " Line numbering
set clipboard=unnamedplus      " Share system clipboard
set eol                        " End of line at bottom of file
set shiftround
set list
set list listchars=tab:»·,trail:·

" custom functions
function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
    set number
  else
    set relativenumber
  endif
endfunc

" Autocmds

" Stripping trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

" Fix rust
autocmd FileType rust setlocal shiftwidth=2 tabstop=2
autocmd BufRead,BufNewFile *.rs set filetype=rust

" Set column width to 80 only on python files
autocmd BufNewFile,BufRead *.py setlocal colorcolumn=80

" Disable the rust style guide recommended 4 space indentation
let g:rust_recommended_style=0

" Explicitly provide vim-go the gofmt command
let g:go_fmt_command = "gofmt"

" Hotkeys
" Note noremap is a normal mode non-recursive mapping
" nnoremap and nmap make the bind only work in normal mode

let mapleader=" "
map <silent> <Leader>t :NERDTreeToggle<CR>
nnoremap <silent> <Leader>r :call NumberToggle()<CR>

" This one maps F5 to delete all trailing whitespace
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Pressing enter in command mode clears the current search highlighting until
" the next search.
nnoremap <silent> <CR> :noh<CR><CR>

" Move tabs left and right
nnoremap <silent> _ :tabp<CR>
nnoremap <silent> + :tabn<CR>

" ctrlp hotkeys
let g:ctrlp_map='<c-p>'
let g:ctrlp_cmd='CtrlP'

" Rebind Ctrl-P to match the window opening keys of nerd tree
let g:ctrl_p_prompt_mappings = {
  \ 'AcceptSelection("h")': ['<c-i>', '<c-cr>', '<c-s>'],
  \ 'AcceptSelection("v")': ['<c-s>', '<RightMouse>'],
\ }

" ctrlp configuration
set wildignore+=*/tmp/*,*.so,*.swp,*.zip      " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe   " Windows

let g:ctrlp_working_path_mode='ra'

" Highlight any line with ErrorMsg that goes over 120 characters
if exists('+colorcolumn')
  set colorcolumn=120
else
  au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
endif

" Javascript linting on the fly
" npm install -g eslint to get it working
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_jsxhint_exec = 'eslint'

" Color customizations
colorscheme zenburn

" Python-mode settings

" disable pymode folding
let g:pymode_folding = 0

" set pydoc key
let g:pymode_doc_key = 'K'

" Disable auto python-mode autocomplete documentation window
set completeopt=menu

" Disable markdown automatic section folding
let g:vim_markdown_folding_disabled = 1

" Merlin for OCaml
" TODO: Detect OPAM and install/configure merlin. Until then this is commented out.
" let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
" execute "set rtp+=" . g:opamshare . "/merlin/vim"

" Fugitive bindings
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gc :Gcommit<CR>

" F9 is bound to fold and unfold in all modes
inoremap <F9> <C-O>za
nnoremap <F9> za
onoremap <F9> <C-C>za
vnoremap <F9> zf

" Line indent guides are defaulted to off
let g:indentLine_enabled=0

" Leader + i will toggle line indent guides
map <silent> <Leader>i :IndentLinesToggle<CR>
