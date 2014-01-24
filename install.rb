#!/usr/bin/env ruby

HOME = File.expand_path('~')

def symlink_files
  Dir['*'].each do |file|
    target = File.join(HOME, ".#{file}")
    unless File.exists? target
      `ln -s #{File.expand_path file} #{target}`
    end
  end
end

symlink_files
