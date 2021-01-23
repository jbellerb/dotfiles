# dotfiles

My personal configuration files for my OpenBSD laptop.

## Usage

Files are managed through various BSD Makefiles. To install all modules, simply run `make`. To install a specific module, run `make [module]`. Modules can be uninstalled with `make clean_[module]` for each module or `make clean` to remove all modules.

## Dependencies

- Fish: `fish`
- Vim: `vim`, download submodules with `git submodule update`
- Xmonad: `xmonad`, `lemonbar-xft`, [pywal](https://github.com/dylanaraps/pywal), and [Roboto Mono](https://github.com/googlefonts/RobotoMono)

<br />

#### License

<sup>
Copyright (C) Jared Beller, 2020-2021.
</sup>
<br />
<sup>
Released under the <a href="https://www.gnu.org/licenses/gpl-3.0.txt">GNU General Public License, Version 3</a> or later. See <a href="LICENSE">LICENSE</a> for more information.
</sup>
