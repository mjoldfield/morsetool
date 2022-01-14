# morse-tool

Code to transform text into Morse Code, encoded both in
the traditional dot-and-dash representation and as the
times for a series of marks and spaces.

Key files:

  * app/Main.hs       <- The main application file.

  * src/ParseInput.hs <- Crude parser for the input file.

  * src/Morse.hs      <- Test to Morse converter.

  * src/ToC.hs        <- Rendering to C arrays.

There's also a small C test example in c-test. The do-it
script builds the tool, generates C arrays, compiles them,
and finally generates WAV files for each message.

## Build instructions

The tool is written in Haskell, and is designed to be
built with stack.

Building the tool is simply a case of:

   % stack build

Once built, run it thus:

   % stack exec morse-tool

## The toy C example

There’s a simple bash script to build and run the tool, then
compile to resulting C, and finally execute that to generate
some audio files.

It assumes that gcc is on the command line:

	% cd c-test
	% ./do-it
	+ rm -f morseout-0.wav morseout-1.wav morseout.c morseout.h
	+ stack build
	morse-tool-0.1.0.0: unregistering (old configure information not found)
	morse-tool> configure (lib + exe)
	Configuring morse-tool-0.1.0.0...
	morse-tool> build (lib + exe)
	Preprocessing library for morse-tool-0.1.0.0..
	Building library for morse-tool-0.1.0.0..
	[1 of 4] Compiling Morse
	[2 of 4] Compiling ParseInput
	[3 of 4] Compiling Paths_morse_tool
	[4 of 4] Compiling ToC
	Preprocessing executable 'morse-tool' for morse-tool-0.1.0.0..
	Building executable 'morse-tool' for morse-tool-0.1.0.0..
	[2 of 2] Compiling Paths_morse_tool
	Linking .stack-work/dist/aarch64-osx/Cabal-3.2.1.0/build/morse-tool/morse-tool ...
	morse-tool> copy/register
	Installing library in ...
	Installing executable morse-tool in ...
	Registering library for morse-tool-0.1.0.0..
	+ stack exec morse-tool
	+ gcc -Wall -pedantic -std=c99 main.c morseout.c tinywav.c -o main
	+ ./main
	Hello World
	Found 2 messages
	 message 0
	   64 steps, total time 111 dits
	   allocated buffer for 266400 samples
	   filled buffer with 266400 samples
	 message 1
	   78 steps, total time 151 dits
	   allocated buffer for 362400 samples
	   filled buffer with 362400 samples
	+ ls -l morseout-0.wav morseout-1.wav
	-rw-r--r--  1 mjo  staff  1065644 14 Jan 22:42 morseout-0.wav
	-rw-r--r--  1 mjo  staff  1449644 14 Jan 22:42 morseout-1.wav

You can then listen to the messages in the two .wav files.

## morse.cfg

The program reads the messages to send from a config file.

	# Here is a test file for the Morse Code compiler

	Hello World

	The battery is OK.

As you can see it is very simple:

 - messages are separated by a blank line;

 - any line starting with # is taken as a comment.
