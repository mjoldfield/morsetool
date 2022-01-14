# morse-tool

Code to transform text into Morse Code, encoded both in
the traditional dot-and-ash representation and as the
times for a series of marks and spaces.

Key files:

  * app/Main.hs       <- The main application file.

  * src/ParseInput.hs <- Crude parser for the input file.

  * src/Morse.hs      <- Test to Morse converter.

  * src/ToC.hs        <- Rendering to C arrays.

There's also a small C test example in c-test. The do-it
script builds the tool, generates C arrays, compiles them,
and finally generates WAV files for each message.


