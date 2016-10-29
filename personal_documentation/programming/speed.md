Speed
=====

Speed from code will come mainly from:

1. Reduce the number of memory allocations. Memory allocations take time
   because available unused memory has to be searched for. I think maybe even
   the OS has to get involved which means you trap out to the OS, taking even
   more time. If you know how much memory you need it is much faster to
   allocate it all at once.
2. Reduce the number of calls which trap to the OS. Such as reading from
   stdin, a file, a socket, etc... Doing this less I think essentially means
   less round trips which means less time. Strategies for doing less of these
   "round trips" include using a buffered reader when reading from files
   (buffered readers read more than you need and then on future requests to
   read it reads from an in memory buffer).
3. Reduce repetative computation. If you have computation which is repetative,
   maybe you can create another variable or something which keeps track of
   computation as you go so you don't need to redo a lot of work.
