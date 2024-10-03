#!/bin/bash
# [[file:setup.org::*link neccessary config files][link neccessary config files:1]]
ln --symbolic inputrc $HOME/.inputrc
ln --symbolic clang-format $HOME/.clang-format
ln --symbolic ./mimeapps/emacsclient.desktop $HOME/.local/share/applicatios/emacsclient.desktop
# link neccessary config files:1 ends here

# [[file:setup.org::*setup][setup:1]]
sudo pacman -Syyu
# setup:1 ends here

# [[file:setup.org::*terminal, shell, scripting][terminal, shell, scripting:1]]
yay --noconfirm --sync alacritty curl libtool fzf fd ripgrep xclip xdg-utils cmake bat git gcc make xdotool htop-vim ttf-iosevka-comfy tmux tldr
# terminal, shell, scripting:1 ends here

# [[file:setup.org::*interactive shell: fish][interactive shell: fish:1]]
yay --noconfirm --sync fish fisher zoxide
fisher install jorgebucaran/autopair.fish patrickf1/fzf.fish

zoxide init fish | source

sudo chsh --shell /usr/bin/fish root
chsh --shell /usr/bin/fish
# interactive shell: fish:1 ends here

# [[file:setup.org::*scripting: ruby][scripting: ruby:1]]
yay --noconfirm --sync ruby
gem install solargraph
bundle install --gemfile $HOME/.config/bin/Gemfile
# scripting: ruby:1 ends here

# [[file:setup.org::*never sudo-password-prompt][never sudo-password-prompt:1]]
echo "$USER ALL=(ALL:ALL) NOPASSWD: ALL" | sudo tee "/etc/sudoers.d/$USER"
# never sudo-password-prompt:1 ends here

# [[file:setup.org::*make system-scripts executeable][make system-scripts executeable:1]]
fd . $HOME/.config/bin --type file --exec chmod +x {}
# make system-scripts executeable:1 ends here

# [[file:setup.org::*editor: doom emacs][editor: doom emacs:1]]
yay --noconfirm --sync emacs-nativecomp

git clone --depth 1 https://github.com/doomemacs/doomemacs $HOME/.config/emacs
$HOME/.config/emacs/bin/doom install
# editor: doom emacs:1 ends here

# [[file:setup.org::*gui apps & packages][gui apps & packages:1]]
yay --noconfirm --sync i3 i3lock zathura zathura-pdf-mupdf arandr mpv yt-dlp brightnessctl unclutter firefox playerctl bluetoothctl xorg-xprop xremap-x11-bin mpd dunst nsxiv maim xorg-xset xorg-xsetroot batsignal pamixer rofi tlp
# gui apps & packages:1 ends here

# [[file:setup.org::*enable daemons][enable daemons:1]]
sudo systemctl enable sshd
sudo systemctl enable mpd
sudo systemctl enable bluetooth
# enable daemons:1 ends here
