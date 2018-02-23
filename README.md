# haskino-examples

This repository contains 5 example programs for use with the [Haskino](https://github.com/ku-fpg/haskino-examples) library.

The examples are as follows, with the name show being the stack build target name:

* [haskino-blink](https://github.com/ku-fpg/haskino-examples/tree/master/blink) - Standard Arduino "Hello World", blinks the onboard LED.
* [haskino-hello](https://github.com/ku-fpg/haskino-examples/tree/master/hello-lawrence) - A Haskino "Hello World" for KU alumni everywhere for use with an LCD display.
* [haskino-lcdcounter](https://github.com/ku-fpg/haskino-examples/tree/master/led-counter) - A Haskino example with a OSEPP LCD shield with uses the LCD display and buttons to implement an up/down counter.
* [haskino-multiled](https://github.com/ku-fpg/haskino-examples/tree/master/multi-led) - A Haskino example demonstrating the multi-tasking kernel with 3 LEDs.
* [haskino-semexample](https://github.com/ku-fpg/haskino-examples/tree/master/sem-example) - A Haskino example demonstrating the multi-tasking kernel and semaphore communication with a flashing LED.
* [haskino-state](https://github.com/ku-fpg/haskino-examples/tree/master/state) - A Haskino example demonstrating mutual recurion transformation implmenting a state machine using the OSEPP LCD shiel..

To build an example with shallow to deep translation (the default) (using haskino-blink as an example):

```
stack build haskino-blink
```

To build an example using the shallow DSL directly (using haskino-blink as an example):

```
stack build haskino-blink --flag haskino-blink:-deep
```

When moving between the shallow and deep versions, make sure and clean the example in between:

```
stack clean haskino-blink
```

To run the compiler in the resulting executable, make sure the compileProgram line is uncommented in the main function in the example, and the first line of main is commented.

To run the interpreter in the resulting executable, make sure the compileProgram line is commented in the main function in the example, and the first line of main is uncommented.

To execute the built example in the compiler or interpreter:

```
stack exec haskino-blink
```

To execute the built example in ghci (Object code flag required for shallow to deep translation):

```
stack ghci haskino-blink  --ghci-options -fobject-code
```

When building with the compiler, the result .ino C language file should be copied into the haskino/firmware/HaskinoRuntime directory, and the 'make upload' command used to program the Arduino board.

**Note**:
The haskino-multiled and haskino-semexample programs use the multi-tasking portions of the Haskino runtime, and in running Haskino tasks on the Arduino, require the deep DSL, and will not work with the compiler or interpreter in the shallow version.

