#ifndef RUNTIME_H
#define RUNTIME_H

//*************************************
// edited: 5/16/23
//int64_t entry();
val_vect_t* entry();


extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
#define heap_size 10000
extern int64_t *heap;
#endif /* RUNTIME_H */
