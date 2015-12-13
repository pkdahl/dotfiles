ENV['HOMEBREW_CASK_OPTS'] = "--appdir=/Applications"

def internet_available?
  puts "Implement internet_available?"
end

def homebrew_available?
  return true if system("which brew &> /dev/null")
end

def homebrew_outdated?
  # True if older than 24 hours
  Dir.chdir("/usr/local/") do
    homebrew_head_timestamp = `git log -1 --format="%ct" HEAD`.to_i
    return true if Time.now.to_i - homebrew_head_timestamp > 60 * 60 * 24
  end
end

def homebrew_self_install
  homebrew_install_command = "ruby - e \"$(curl -fsSL " \
                             "https://raw.githubusercontent.com/" \
                             "Homebrew/install/master/install)\""
  exec("#{homebrew_install_command}") unless homebrew_available?
end

def homebrew_self_update
  exec("brew update") if homebrew_outdated?
end

def homebrew_install(formula, *args)
  versions = `brew list #{formula} --versions`
  return unless versions.empty?
  homebrew_self_update
  # installing formula message...
  exec("brew install #{formula} #{args.join(' ')}") if versions.empty?
end

def homebrew_cask_available?
  return true if system("brew cask > /dev/null")
end

def homebrew_cask_self_install
  if homebrew_available?
    exec("brew tap caskroom/cask") unless homebrew_cask_available?
  end
end

def homebrew_cask_install(formula, *options)
  formula_info = `brew cask info #{formula}`
  return unless formula_info.include?("Not installed")
  homebrew_self_update
  # installing formula message...
  exec("brew cask install #{formula} #{options.join(' ')}")
end

def xcode_available?
  return true if system("xcode-select --print-path &> /dev/null")
end

def stow_install(package)
  stow_cmd = "stow --ignore \".DS_Store\" --target $HOME -R"
  exec("#{stow_cmd} #{package}")
end

#-- Rake tasks --

desc "Setup up base system"
task :base do
  xcode_not_available_message = <<EOS
It is necessary to have Xcode installed to proceed
EOS
  abort(xcode_not_available_message) unless xcode_available?

  homebrew_self_install
  homebrew_cask_self_install
  homebrew_install "coreutils", "--with-gmp"
  homebrew_install "stow"
end

desc "Setup Emacs"
task :emacs => [:base, :xquartz] do
  homebrew_install "emacs", "--with-cocoa", "--with-gnutls",
                   "--with--imagemagick", "--with-x11"
  stow_install "emacs"
  stow_install "authinfo"
end

desc "Install XQuartz"
task :xquartz => [:base] do
  homebrew_cask_install "xquartz"
end

desc "Setup finger"
task :finger => [:base] do
  stow_install "finger"
end

desc "Setup Vim"
task :vim => [:base] do
  stow_install "vim"
end
