.PHONY: utils config
all: utils config

utils:
	mkdir -p "$(HOME)/.bin"
	install -Dm755 "utils/cats" "$(HOME)/.bin/cats"
	install -Dm755 "utils/rgbriks" "$(HOME)/.bin/rgbriks"
	install -Dm755 "utils/rndchar" "$(HOME)/.bin/rndchar"
	install -Dm755 "utils/screenshot" "$(HOME)/.bin/screenshot"

config: wm bar terminal shell
	$(info done)

wm:
	cp -rvf "config/xmonad" "$(HOME)/.config/"

bar:
	cp -rvf "config/xmobar/.xmobarrc" "$(HOME)/"

terminal:
	cp -rvf "config/termonad" "$(HOME)/.config/"

shell:
	cp -rvf "config/zsh/.zshrc" "$(HOME)/"
	cp -rvf "config/zsh/.zsh" "$(HOME)/"
	cp -rvf "config/zsh/manjaro-zsh-config" "$(HOME)/.zsh/"
