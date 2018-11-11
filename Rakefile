# coding: utf-8
require 'rake'

DOTFILES_DIR = File.dirname(__FILE__)
HOME_DIR = ENV['HOME']
CONFIG_DIR = HOME_DIR + "/.config"
DATA_DIR = HOME_DIR + "/.local/share"
CACHE_DIR = HOME_DIR + "/.cache"

desc "Test"
task :test do
  zsh = Zsh.new
  zsh.remove
end

# * Emacs

desc "Setup Emacs"
task :emacs do
  emacs = Emacs.new
  emacs.setup
end

desc "Remove Emacs"
task :emacs_remove do
  emacs = Emacs.new
  emacs.remove
end

class Emacs

  def initialize
    @source_dir = "#{DOTFILES_DIR}/emacs"

    @config_dir = "#{HOME_DIR}/emacs"
    @data_dir = "#{DATA_DIR}/emacs"
    @cache_dir = "#{CACHE_DIR}/emacs"

    @links = Hash.new
    @dirs = Array.new

    @links[@source_dir] = @config_dir
    @dirs << @data_dir << @cache_dir
  end

  def setup
    mk_dirs(@dirs)
    mk_links(@links)
  end

  def remove
    rm_links(@links)
    rm_dirs(@dirs.reverse)
  end
    
end

# * Zsh

desc "Setup Zsh"
task :zsh do
  zsh = Zsh.new
  zsh.setup
end

desc "Remove Zsh"
task :zsh_remove do
  zsh = Zsh.new
  zsh.remove
end

class Zsh

  def initialize
    @source_dir = "#{DOTFILES_DIR}/zsh"

    @config_dir = "#{CONFIG_DIR}/zsh"
    @data_dir = "#{DATA_DIR}/zsh"
    @cache_dir = "#{CACHE_DIR}/zsh"

    @symlinks = Hash.new
    @dirs = Array.new

    @dirs << @config_dir << @data_dir << @cache_dir
    add_startup_files
    add_spaceship_prompt
   end

  def setup
    mk_dirs(@dirs)
    mk_links(@symlinks)
  end

  def remove
    rm_links(@symlinks)
    rm_dirs(@dirs.reverse)
  end

  def add_startup_files
    Dir.entries(@source_dir).reject{ |f| f =~ /^.{1,2}$/ }.each do |f|
      if File.fnmatch("*.zsh", f)
        @symlinks[:"#{@source_dir}/#{f}"] = "#{@config_dir}/#{f}"
      else
        @symlinks[:"#{@source_dir}/#{f}"] = "#{HOME_DIR}/.#{f}"
      end
    end
  end
  
  def add_spaceship_prompt
    target_dir = "#{@data_dir}/site-functions"
    target = "#{target_dir}/prompt_spaceship_setup"
    source = "#{DOTFILES_DIR}/lib/spaceship-prompt/spaceship.zsh"

    @dirs << target_dir
    @symlinks[source] = target
  end
  
end

# * Auxiliary methods

def mk_dirs(dirs)
  dirs.each do |d|
    if !File.exists?(d)
      puts "making directory #{d}"
      FileUtils.mkdir_p(d)
    end
  end
end

def rm_dirs(dirs)
  dirs.each do |d|
    if File.exists?(d)
      puts "removing directory #{d}"
      FileUtils.rmdir(d)
    end
  end
end

def mk_links(links)
  len = links.keys.map{|k| k.length}.max - 2

  links.each do |source, target|
    if !File.exist?(target)
      puts sprintf("linking %-#{len}s -> %s", target, source)
      File.symlink("#{source}", "#{target}")
    end
  end
end

def rm_links(links)
  links.each do |_, target|
    if File.exists?(target)
      puts "unlinking #{target}"
      File.delete(target)
    end
  end
end

def installed?(pkg)
  system "brew list #{pkg} &> /dev/null"
end
