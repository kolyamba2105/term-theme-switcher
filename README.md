# term-theme-switcher

CLI tool for applying `theme` to existing Alacritty config file. Themes can be
found [here](https://github.com/eendroroy/alacritty-theme) and
[there](https://github.com/aarowill/base16-alacritty).

## Usage

There are two available commands:

1. Local - apply local theme to existing config

```sh
Usage: term-theme-switcher-exe local (-c|--config STRING) (-p|--path STRING)

Available options:
  -h,--help                Show this help text
  -c,--config STRING       Path to initial config file
  -p,--path STRING         Path to theme file
```

2. Remote - fetch theme from remote server and apply it to existing config

```sh
Usage: term-theme-switcher-exe remote (-c|--config STRING) (-u|--url STRING)

Available options:
  -h,--help                Show this help text
  -c,--config STRING       Path to initial config file
  -u,--url STRING          URL to theme file
```
