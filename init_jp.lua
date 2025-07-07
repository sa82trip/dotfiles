-- Neovim 設定ファイル

-- 1. 基本エディタ設定
-- 行番号表示
vim.opt.nu = true
vim.opt.relativenumber = true

-- タブとインデント設定
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true

-- マウスサポート有効化
vim.opt.mouse = 'a'

-- クリップボード統合有効化
vim.opt.clipboard = 'unnamedplus'

-- トゥルーカラーサポート有効化
vim.opt.termguicolors = true

-- スクロールオフセット設定 (カーソル上下に残す行数)
vim.opt.scrolloff = 8

-- 折り返し設定
vim.opt.wrap = false

-- エンコーディング設定
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

-- リーダーキー設定
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- 2. キーバインディング (例)
-- ファイル保存
vim.keymap.set('n', '<leader>w', ':w<CR>', { desc = 'Save file' })
-- Neovim 終了
vim.keymap.set('n', '<leader>q', ':q<CR>', { desc = 'Quit Neovim' })
-- 保存して終了
vim.keymap.set('n', '<leader>wq', ':wq<CR>', { desc = 'Save and Quit Neovim' })
-- 相対行番号切り替え
vim.keymap.set('n', '<leader>rn', ':set relativenumber!<CR>', { desc = 'Toggle relative number' })

-- 挿入モードからjkまたはjjでノーマルモードに戻る
vim.keymap.set('i', 'jk', '<ESC>', { desc = 'Exit Insert mode with jk' })
vim.keymap.set('i', 'jj', '<ESC>', { desc = 'Exit Insert mode with jj' })

-- 3. プラグイン管理 (lazy.nvim)
-- lazy.nvim が見つからない場合は自動的にインストール
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- 最新の安定版リリース
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- lazy.nvim 設定
require("lazy").setup({
  -- 例のプラグイン: TokyoNight テーマ
  {
    "folke/tokyonight.nvim",
    lazy = false, -- 起動時にこのプラグインをロード
    priority = 1000, -- 最初にロードされるように設定
    opts = {}, -- 空のオプションテーブル
    config = function()
      vim.cmd.colorscheme("tokyonight-night")
    end,
  },
  -- nvim-surround: テキストのペアを簡単に追加/変更/削除
  {
    "kylechui/nvim-surround",
    version = "*", -- 最新の安定版リリースを使用
    event = "VeryLazy", -- 他のほとんどのものの後にロード
    config = function()
        require("nvim-surround").setup({})
    end
  },
  -- vim-fugitive: Git ラッパー
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gdiff", "Gblame", "Glog", "Gpush", "Gpull", "Gfetch" }, -- これらのコマンドでロード
  },
  -- ここにさらにプラグインを追加
  -- { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  -- { 'neovim/nvim-lspconfig' },
})