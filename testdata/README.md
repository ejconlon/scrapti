## testdata

### External

* [drums.wav](https://freewavesamples.com/ensoniq-zr-76-03-dope-85-bpm)
* [timpani.sf2](https://sites.google.com/site/soundfonts4u/)

### Internal

* testproj - empty project with `drums.wav` as first instrument
* testlen - project with `drums.wav` as first instrument and 2 patterns
  * 1 - len 4, track 1 has C3 on step 1 and C4 on step 4, track 8 has D3 on step1 and D4 on step 4
  * 2 - len 128, similarly (steps 1 and 128 on tracks 1 and 8)
* testempty - a truly empty project
* testfx - test all the effects
  * C3 repeated on all 44 steps of track 1, pattern 1
  * First step - off, then all fx in order
  * Step 43+44 no FX1, FX2 off and vol effects
* testproj16 - same as testproj but created on patch 1.6
