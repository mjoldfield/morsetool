# morse-tool

Code to transform text into Morse Code represented as a list
of times for alternating beep and silence. The quantum of time
is the length of a dit, so A which is .- in Morse, is encoded
as:

  - 1 : the dit;
  - 1 : a dit-length space between letters;
  - 3 : the dah.

You can see that a dah is three dits long. Wikipedia has a good
section on [timings and
speeds](https://en.wikipedia.org/wiki/Morse_code#Representation,_timing,_and_speeds)
if you want to know more.

The tool might be useful if you want to broadcast text in Morse Code
particularly if the device doing the broadcasting doesn’t have many
resources.

Key files:

  * app/Main.hs       <- The main application file.

  * src/ParseInput.hs <- Crude parser for the input file.

  * src/Morse.hs      <- Test to Morse converter.

  * src/ToC.hs        <- Rendering to C arrays.

There’s also a small C test example in c-test. The do-it
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

## The C API

### const uint32_t n_morse_messages

This is the number of messages available.

### uint32_t morse_get_next_time(uint32_t n_msg, uint32_t * const msg_i)

This function is called repeatedly returning a new time interval every
time. On the first call, `*msg_i` should be set to 0. The function
will increment `*msg_i` internally until the end of the message
whereupon it is set to 0. Thus repeated calls will loop over the
message, though a padding space will probably be required.

It is worth noting the `*msg_i` should either be set to zero or
compared to zero by user code. No other operations are supported.

### An example

The morse.cfg file above contains two messages, so

	n_morse_messages = 2

To read the timing data, code like this could be used:

	uint32_t i = 0;
	bool ms = true;
	do
	  {
		const uint32_t dt = morse_get_next_time(0, &i);
		printf("%-7s %d\n", (ms ? "beep" : "silence"), dt);

		ms = !ms;
	  }
	while (i != 0);

The first message begins `H` then `e` then `l` which in Morse is
represented as `.... . .-..`. Running the code above gives:

	beep    1
	silence 1
	beep    1
	silence 1
	beep    1
	silence 1
	beep    1
	silence 3
	beep    1
	silence 3
	beep    1
	silence 1
	beep    3
	silence 1
	beep    1
	silence 1
	beep    1
	silence 3
