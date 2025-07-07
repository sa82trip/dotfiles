-- Neovim 설정 파일

-- 1. 기본 편집기 설정
-- 줄 번호 표시
vim.opt.nu = true
vim.opt.relativenumber = true

-- 탭 및 들여쓰기 설정
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true

-- 마우스 지원 활성화
vim.opt.mouse = 'a'

-- 클립보드 통합 활성화
vim.opt.clipboard = 'unnamedplus'

-- 트루 컬러 지원 활성화
vim.opt.termguicolors = true

-- 스크롤 오프셋 설정 (커서 위/아래로 유지할 줄 수)
vim.opt.scrolloff = 8

-- 자동 줄 바꿈 설정
vim.opt.wrap = false

-- 인코딩 설정
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

-- 리더 키 설정
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- 2. 키 바인딩 (예시)
-- 파일 저장
vim.keymap.set('n', '<leader>w', ':w<CR>', { desc = '파일 저장' })
-- Neovim 종료
vim.keymap.set('n', '<leader>q', ':q<CR>', { desc = 'Neovim 종료' })
-- 저장 후 종료
vim.keymap.set('n', '<leader>wq', ':wq<CR>', { desc = '저장 후 Neovim 종료' })
-- 상대 줄 번호 토글
vim.keymap.set('n', '<leader>rn', ':set relativenumber!<CR>', { desc = '상대 줄 번호 토글' })

-- 삽입 모드에서 jk 또는 jj로 노멀 모드 전환
vim.keymap.set('i', 'jk', '<ESC>', { desc = 'jk로 삽입 모드 종료' })
vim.keymap.set('i', 'jj', '<ESC>', { desc = 'jj로 삽입 모드 종료' })

-- 3. 플러그인 관리 (lazy.nvim)
-- lazy.nvim이 없으면 자동으로 설치
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- 최신 안정 릴리스
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- lazy.nvim 설정
require("lazy").setup({
  -- 예시 플러그인: TokyoNight 테마
  {
    "folke/tokyonight.nvim",
    lazy = false, -- 시작 시 이 플러그인 로드
    priority = 1000, -- 가장 먼저 로드되도록 설정
    opts = {}, -- 빈 옵션 테이블
    config = function()
      vim.cmd.colorscheme("tokyonight-night")
    end,
  },
  -- nvim-surround: 텍스트 쌍을 쉽게 추가/변경/삭제
  {
    "kylechui/nvim-surround",
    version = "*", -- 최신 안정 릴리스 사용
    event = "VeryLazy", -- 다른 대부분의 것들 이후에 로드
    config = function()
        require("nvim-surround").setup({})
    end
  },
  -- vim-fugitive: Git 래퍼
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gdiff", "Gblame", "Glog", "Gpush", "Gpull", "Gfetch" }, -- 이 명령어들에서 로드
  },
  -- 여기에 더 많은 플러그인 추가
  -- { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  -- { 'neovim/nvim-lspconfig' },
})