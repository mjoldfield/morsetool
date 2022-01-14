#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "tinywav.h"

#include "morseout.h"

const uint32_t f_sample = 48000; // Hz
const uint32_t f_tone   = 800;   // Hz
const uint32_t t_dit    = 50;    // in ms : NOTE NOT s

int main(int argc, char *argv[])
{
  printf("Hello World\n");

  printf("Found %d messages\n", n_morse_messages);

  for(uint32_t i = 0; i < n_morse_messages; i++)
    {
      printf("  message %d\n", i);

      uint32_t j = 0, n = 0, t = 0;
      do
	{
	  const uint32_t dt = morse_get_next_time(i, &j);
	  n += 1;
	  t += dt;
	} while (j != 0);
      printf("    %d steps, total time %d dits\n", n, t);

      uint32_t samples_per_dit = f_sample * t_dit / 1000; // div by 1000 bcs t_dit in ms

      uint32_t n_samples       = t * samples_per_dit;

      float *samples = malloc(n_samples * sizeof(float));
      printf("    allocated buffer for %d samples\n", n_samples);

      uint32_t it = 0;
      bool tone = true;
      do
	{
	  const uint32_t dt = morse_get_next_time(i, &j);
	  for(uint32_t k = 0; k < dt * samples_per_dit; k++)
	    {
	      double t   = (double)it / (double)f_sample;
	      double phi = 2.0 * M_PI * t * (double)f_tone; // 800Hz tone

	      samples[it++] = tone ? sin(phi) : 0.0;
	    }
	  tone = !tone;
	} while (j != 0);

      printf("    filled buffer with %d samples\n", it);

      char filename[32];
      snprintf(filename, 32, "morseout-%d.wav", i);

      TinyWav tw;
      tinywav_open_write(&tw, 1, f_sample, TW_FLOAT32, TW_INLINE, filename);
      tinywav_write_f(&tw, samples, n_samples);
      tinywav_close_write(&tw);
    }

  return 0;
}
