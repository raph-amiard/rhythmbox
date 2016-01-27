all: ada-nanovg ada-synth-lib

ada-nanovg:
	git clone https://github.com/raph-amiard/ada-nanovg/
	cd ada-nanovg && make -f setup.makefile

ada-synth-lib:
	git clone https://github.com/raph-amiard/ada-synth-lib/
	cd ada-synth-lib && make -f setup.makefile
