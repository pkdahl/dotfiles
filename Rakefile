# coding: utf-8
require 'rake'

DOTFILES_DIR = File.dirname(__FILE__)
HOME_DIR = ENV['HOME']
CONFIG_DIR = HOME_DIR + "/.config"

desc "Setup Zsh"
task :zsh do
  zsh = Zsh.new
  zsh.setup
end

class Zsh

  def initialize
    @symlinks = set_symlinks
   end

  def set_symlinks
    symlinks = Hash.new
    source_dir  = "#{DOTFILES_DIR}/zsh"
    target_dir = "#{CONFIG_DIR}/zsh"

    Dir.entries(source_dir).reject{ |f| f =~ /^.{1,2}$/ }.each do |f|
      if File.fnmatch("*.zsh", f)
        symlinks[:"#{source_dir}/#{f}"] = "#{target_dir}/#{f}"
      else
        symlinks[:"#{source_dir}/#{f}"] = "#{HOME_DIR}/.#{f}"
      end
    end

    return symlinks
  end

  def setup
    len = @symlinks.keys.map{|k| k.length}.max - 2

    @symlinks.each do |source, target|
      if !File.exist?(target)
        puts sprintf("linking %-#{len}s -> %s", target, source)
        File.symlink("#{source}", "#{target}")
      else
        puts sprintf("link    %-#{len}s    already exists", target)
      end
    end
  end

  def remove
    @symlinks.each do |_, target|
      if File.exists?(target)
        puts "unlinking #{target}"
        File.delete(target)
      end
    end
  end

end

def installed?(pkg)
  system "brew list #{pkg} &> /dev/null"
end
