# scrapti

WORK IN PROGRESS - Utility to extract/convert Polyend Tracker PTI files

Right now there is code to parse/render Wav/Soundfont/PTI files. The Soundfont to PTI conversion is next.

## License

This project is BSD-licensed. Some soundfont parser code is derived from [HCodecs](https://github.com/Mokosha/HCodecs/blob/master/LICENSE), which is licensed the same.

## Related projects

* [pti-file-format](https://github.com/jaap3/pti-file-format)
* [polyendtracker-midi-export](https://github.com/DataGreed/polyendtracker-midi-export)

## SFZ format restrictions

* Should have `<control>` section
  * Optional `default_path` for relative path to samples
  * Optional `hint_tempo` for Hz to cycles/step conversion
* Show have `<global>` section
  * Automation mapping:
    * lfo/eg01 -> volume
    * lfo/eg02 -> panning
    * lfo/eg03 -> cutoff
    * lfo/eg04 -> pitch
  * LFO attrs:
    * lfoXX_wave = wav type as int
    * lfoXX_freq = 0 to 20 in Hz
    * lfoXX_{cat}={depth}
    * Calculate cycles/step by 60 * freq / tempo (e.g. 60 s/min * 20 cycles/sec / 120 BPM = 10 cycles/beat)
  * Envelope attrs:
    * egXX_attack = 0 to 100 float in seconds
    * egXX_decay = 0 to 100 float in seconds
    * egXX_sustain = 0 to 100 float in %
    * egXX_release = 0 to 100 float in seconds
    * egXX_{cat}={depth}
  * Only ONE automation can be defined per category
  * If any attrs are defined, then lfo/eg_{cat}={depth} must also be defined and all attrs must be defined with it
* Must have at least one `<region>` section
  * Non-overlapping key ranges with `lokey=0` on lowest and `hikey=127` on highest for full coverage
