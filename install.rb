#!/usr/bin/env ruby

HOME = File.expand_path('~')

def symlink_files
  Dir['*'].each do |file|
    target = File.join(HOME, ".#{file}")
    unless File.exists? target
      `ln -s #{File.expand_path file} #{target}`
    end
  end

  install_files = Dir['install/{.**,**}/**/*'].reject{ |x| x =~ /(\.\.|\.$)/ }.reject{ |x| File.directory?(x) }
  install_files.each do |f|
    install_path = f[8..-1]
    target = File.join(HOME, install_path)
    unless File.exists?(target)
      `mkdir -p #{File.dirname(target)}`
      `ln -s #{File.expand_path f} #{target}`
    end
  end
end

def update_submodules
  submodules = ['oh-my-zsh', 'vim/bundle/vundle']
  submodules.each do |s|
    `git submodule init #{s}`
    `git submodule update #{s}`
  end
end

def make_vim_temp
  vim_temp = File.join(HOME, '.vim-tmp')
  unless File.exists? vim_temp
    `mkdir #{vim_temp}`
  end
end

make_vim_temp
update_submodules
symlink_files

exec "vim -u #{File.expand_path('~/.vim/vundle.vim')} +BundleInstall +q +q; zsh"
