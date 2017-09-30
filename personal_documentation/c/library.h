// These are "include guards" (or "inclusion guards" or "header
// guards" etc...) which check if the header file has been included
// before and if so then don't include it again because that could
// lead to conflicts (e.g. if a header file XYZ defines a type and
// gets included multiple times then the type would be defined
// multiple times and you'd have a conflict). They are necessary
// because the C compiler is dumb and does not attempt to keep track
// of which header files have been included before. The fact that this
// is (pretty much always) necessary but the compiler doesn't take
// care of it for you is a perfect example of manual work that C makes
// you do. It's not hard to do but it sucks that you have to do it at
// all and if you aren't aware of it it can lead to problems. The name
// of the include guard can be anything but typically it is
// "<file-name>_H".
#ifndef LIBRARY_H
#define LIBRARY_H

// Include any other header files necessary for this header file. You
// can see how easy it would be to forget to remove this if you no
// longer needed it anymore.
#include <stdbool.h>		/* Defines the "bool" type */

// A new type.
typedef struct {
  char x;
  char y;
} dir;

// When declaring a variable you have to add the word "extern" to it.
// Typically we think of "int myVar;" as "declaring" the variable but
// in C this also "defines" it. I think the real difference between
// "declaring" and "defining" is that "declaring" means you know what
// the types are and "defining" means that memory was allocated for
// the symbol. If you do "int myVar;" then myVar is "declared" since
// you know it's type but it is also "defined" since memory was
// allocated for it. But prepending "extern" like "extern int myVar"
// makes it so "myVar" is only declared and not defined. By default
// you do not need to put "extern" for function prototypes. In many
// cases global variables in a library are probably a bad idea but if
// they are useful/necessary then this is how you would do it. More on
// "extern" here:
// http://www.geeksforgeeks.org/understanding-extern-keyword-in-c/.
extern const dir UP;
extern const dir DOWN;
extern const dir LEFT;
extern const dir RIGHT;

// An example of a function declaration (also called a function
// prototype).
bool dir_equal(dir a, dir b);

// The end of the include guard. Conventionally you include a comment
// indicating the name of the macro guard that was defined.
#endif /* LIBRARY_H */
