#include <stdlib.h>
#include <stdio.h>
#include "memory.h"

#define SIZE_CELL 8192
#define MIN_INCREASE 1024


Memory **create_memory(int *size_memory)
{
	Memory **memory;
	int i;
	memory = (Memory **)malloc(sizeof(Memory *));
	*memory = (Memory *)malloc(sizeof(Memory) * MIN_INCREASE);
	for (i = 0; i < MIN_INCREASE; ++i) (*memory)[i].init = 0;
	*size_memory = MIN_INCREASE;
	return memory;	
}

int read_memory(Memory **memory, unsigned int address, int size_memory)
{
	unsigned int address_cell = address / SIZE_CELL; 
	if (address_cell >= size_memory) return 0;
	if ((*memory)[address_cell].init == 0) return 0;
	return (*memory)[address_cell].cell[address % SIZE_CELL];
}

void write_memory(Memory **memory, unsigned int address, int number, int *size_memory, int *err)
{
	unsigned int address_cell = address / SIZE_CELL;
	if (address_cell >= *size_memory)
	{
		int i;
		Memory *new_memory; 
		unsigned int new_size_memory = ((address_cell / MIN_INCREASE) + 1) * MIN_INCREASE;
		
		new_memory = (Memory *)realloc(*memory, sizeof(Memory) * new_size_memory);
		if (new_memory != NULL) *memory = new_memory;
		else
		{
			*err = 4;
			return;
		}
		for (i = *size_memory; i < new_size_memory; ++i) (*memory)[i].init = 0; 
		*size_memory = new_size_memory;		
	}
	if ((*memory)[address_cell].init == 0)
	{
		(*memory)[address_cell].cell = (int *)calloc(SIZE_CELL, sizeof(int));
		if ((*memory)[address_cell].cell != NULL) (*memory)[address_cell].init = 1;
		else
		{	*err = 4;
			return;
		}
	}
	(*memory)[address_cell].cell[address % SIZE_CELL] = number;
} 

void free_memory(Memory **memory, int size_memory)
{
       	int i;                                                                               
	for (i = 0; i < size_memory; ++i) if ((*memory)[i].init == 1) free((*memory)[i].cell);
	free(*memory);
        free(memory);
	memory = NULL;
}
