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

symlink_files

