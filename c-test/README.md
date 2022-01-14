# c-test : A crude test-suite

The do-it script builds the tool, generates C arrays, compiles them,
and finally generates WAV files for each message.

Messages are specified in morse.cfg

The audio is an 800Hz tone, with 50ms dits. The WAV files are
sampled at 48kHz, and constructed with tinywav library ripped
from https://github.com/mhroth/tinywav. 

