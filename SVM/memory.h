#ifndef _memory_h
#define _memory_h

typedef struct{
	int *cell;
	char init;	
} Memory;

Memory **create_memory(int *);
int read_memory(Memory **, unsigned int, int);
void write_memory(Memory **, unsigned int, int, int *, int *);
void free_memory(Memory **, int);
#endif
