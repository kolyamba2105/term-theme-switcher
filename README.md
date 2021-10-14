# term-theme-switcher

CLI tool for applying `theme` to existing Alacritty config file. Themes can be
found [here](https://github.com/eendroroy/alacritty-theme) and
[there](https://github.com/aarowill/base16-alacritty).

## Usage

There are three available commands:

```sh
Alacritty theme swither

Usage: term-theme-switcher-exe (local | remote | reset)

Available options:
  -h,--help                Show this help text

Available commands:
  local
  remote
  reset
```

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

3. Reset - reset to default Alacritty theme

```sh
Usage: term-theme-switcher-exe reset [-c|--config STRING]

Available options:
  -h,--help                Show this help text
  -c,--config STRING       Path to initial config file
```

## Demo

https://user-images.githubusercontent.com/33033411/137287888-5106749a-e8ba-41ab-a3cb-7dad9dae5172.mov
