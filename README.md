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

## Sampling workflow

* Auto sampler settings:
  * Range start: Cx
  * Range end: Cy
  * Sample every: 12 semi
  * Round robin: No
  * Sustain: 2.0 s
  * Velocity layers: 1
  * Velocity response: Custom
  * Auto loop: Search with
  * Auto loop start: 20
  * Auto loop end: 90
  * Input gain: -7 dB (depends on instrument)
* Take a patch and remove all LFO/Env/Filter (pure oscillators)
* Pick a middle C (one where 2 oct below and 2 oct above sound ok)
* Play some notes and set the input gain so it doesn't clip
* Name the patch InstName
* Sample those five notes for 3 seconds at velocity 100
* Initialize the patch (TODO add command to scrapti exe)
* Open in Sfortzando and edit according to model
* Convert to PTI (TODO add command to scrapti exe)

## Commands

Can set `REBUILD=1` to force builds.

    ./scrapti.sh init ../slaw-packs/packs/SS01 Saucer

## Example global section

    <global>
    // General params
    pan=0
    tune=0

    // Filter
    fil_type=lpf_2p
    cutoff=22050
    resonance=0

    // Volume Env
    eg01_volume=0
    eg01_attack=0
    eg01_decay=0
    eg01_sustain=100
    eg01_release=0

    // Volume LFO
    lfo01_volume=0
    lfo01_wave=0
    lfo01_freq=0

    // Panning Env
    eg02_panning=0
    eg02_attack=0
    eg02_decay=0
    eg02_sustain=100
    eg02_release=0

    // Panning LFO
    lfo02_panning=0
    lfo02_wave=0
    lfo02_freq=0

    // Cutoff Env
    eg03_cutoff=0
    eg03_attack=0
    eg03_decay=0
    eg03_sustain=100
    eg03_release=0

    // Cutoff LFO
    lfo03_cutoff=0
    lfo03_wave=0
    lfo03_freq=0

    // Pitch Env
    eg04_pitch=0
    eg04_attack=0
    eg04_decay=0
    eg04_sustain=100
    eg04_release=0

    // Pitch LFO
    lfo04_pitch=0
    lfo04_wave=0
    lfo04_freq=0
