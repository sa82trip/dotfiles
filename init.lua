-- Neovim configuration file

-- 1. Basic Editor Settings
-- Set line numbers
vim.opt.nu = true
vim.opt.relativenumber = true

-- Set tab and indent settings
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true

-- Enable mouse support
vim.opt.mouse = 'a'

-- Enable clipboard integration
vim.opt.clipboard = 'unnamedplus'

-- Enable true color support
vim.opt.termguicolors = true

-- Set scrolloff (lines to keep above/below cursor)
vim.opt.scrolloff = 8

-- Set wrap (wrap long lines)
vim.opt.wrap = false

-- Set encoding
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

-- Set leader key
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- 2. Keybindings (Examples)
-- Save file
vim.keymap.set('n', '<leader>w', ':w<CR>', { desc = 'Save file' })
-- Quit Neovim
vim.keymap.set('n', '<leader>q', ':q<CR>', { desc = 'Quit Neovim' })
-- Save and quit
vim.keymap.set('n', '<leader>wq', ':wq<CR>', { desc = 'Save and Quit Neovim' })
-- Toggle relative number
vim.keymap.set('n', '<leader>rn', ':set relativenumber!<CR>', { desc = 'Toggle relative number' })

-- Return to Normal mode from Insert mode with jk or jj
vim.keymap.set('i', 'jk', '<ESC>', { desc = 'Exit Insert mode with jk' })
vim.keymap.set('i', 'jj', '<ESC>', { desc = 'Exit Insert mode with jj' })

-- 3. Plugin Management (lazy.nvim)
-- Automatically install lazy.nvim if not found
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Configure lazy.nvim
require("lazy").setup({
  -- Example plugin: TokyoNight theme
  {
    "folke/tokyonight.nvim",
    lazy = false, -- Load this plugin on startup
    priority = 1000, -- Make sure it loads first
    opts = {}, -- Empty options table
    config = function()
      vim.cmd.colorscheme("tokyonight-night")
    end,
  },
  -- nvim-surround: Easily add/change/delete surrounding pairs
  {
    "kylechui/nvim-surround",
    version = "*", -- Use latest stable release
    event = "VeryLazy", -- Load after most other things
    config = function()
        require("nvim-surround").setup({})
    end
  },
  -- vim-fugitive: Git wrapper
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gdiff", "Gblame", "Glog", "Gpush", "Gpull", "Gfetch" }, -- Load on these commands
  },
  -- Add more plugins here
  -- { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  -- { 'neovim/nvim-lspconfig' },
})