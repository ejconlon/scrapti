include Makefile.base

.PHONY: exe
exe:
	stack build --test --no-run-tests --exec scrapti-exe

.PHONY: sines
sines:
	sox -V -r 44100 -n -b 16 -c 2 testdata/sin_stereo.wav synth 1 sin 500 vol -10dB
	sox -V -r 44100 -n -b 16 -c 1 testdata/sin_mono.wav synth 1 sin 500 vol -10dB
	sox -V -r 44100 -n -b 16 -c 2 testdata/sin_stereo.aif synth 1 sin 500 vol -10dB
	sox -V -r 44100 -n -b 16 -c 1 testdata/sin_mono.aif synth 1 sin 500 vol -10dB
