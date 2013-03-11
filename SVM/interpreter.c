#include <stdio.h>
#include "opcode.h"
#include "stack.h"
#include "memory.h"

int interpreter(Command *commands, int size_com, int *num_str, int *err)
{
	unsigned int  point = 0;
	int result, size_memory;
	Stack **stack;
	Memory **memory;
	memory = create_memory(&size_memory);
	stack = new_stack();
	while (commands[point].opcode != HLT)
	{
		if (commands[point].opcode == LDC) 
			push(stack, commands[point++].number, err);
		else if (commands[point].opcode == ST)
		{
			if (top_of_clean(stack) != 0)
				write_memory(memory, commands[point++].address, pop(stack), &size_memory, err);
			else *err = 1;
		}	
		else if (commands[point].opcode == LD) 
				push(stack, read_memory(memory, commands[point++].address, size_memory), err);
		else if (commands[point].opcode == ADD)
		{
			if (top_of_clean(stack) == 2)
			{
				push(stack, pop(stack) + pop(stack), err);
				++point;
			}
			else *err = 1;
		}
		else if (commands[point].opcode == SUB)
		{
			if (top_of_clean(stack) == 2)
			{
				int summand = pop(stack);
				push(stack, pop(stack) - summand, err);
				++point;
			}
			else *err = 1;
		}
		else if (commands[point].opcode == MUL)
		{
			if (top_of_clean(stack) == 2)
			{
				push(stack, pop(stack) * pop(stack), err);
				++point;
			}
			else *err = 1;
		}
		else if (commands[point].opcode == DIV)
		{
			if (top_of_clean(stack) == 2)
			{
				int summand = pop(stack);
				if (summand != 0)
				{
					push(stack, pop(stack) / summand, err);
					++point;
				}
				else *err = 2;
			}
			else *err = 1;
		}
                else if (commands[point].opcode == MOD)
		{
			if (top_of_clean(stack) == 2)
			{
				int summand = pop(stack);
				if (summand != 0)
				{
					push(stack, pop(stack) % summand, err);
					++point;
				}
				else *err = 2;
			}
			else *err = 1;
		}
		else if (commands[point].opcode == CMP)
		{
			if (top_of_clean(stack) == 2)
			{
				int a = pop(stack), b = pop(stack);
			        if (a > b) push(stack, -1, err);
				else if (a < b) push(stack, 1, err);
				else push(stack, 0, err);
				++point;
			}
			else *err = 1;
		}
		else if (commands[point].opcode == JMP)
			point = commands[point].num_com;
		else if (commands[point].opcode == BR)
		{
			if (top_of_clean(stack) != 0)
			{
				if (pop(stack) == 0) ++point;
				else point = commands[point].num_com;
			}
			else *err = 1;	
		}
		else if (commands[point].opcode == LDI)
		{
			if (top_of_clean(stack) != 0)
			{
				push(stack, read_memory(memory, pop(stack), size_memory), err);
				++point;
			}
			else *err = 1;	
		}
		else if (commands[point].opcode == STI)
		{
			if (top_of_clean(stack) == 2)
			{
				unsigned int address = pop(stack);
				write_memory(memory, address, pop(stack), &size_memory, err);
				++point;
			}
			else *err = 1;
		}
		else if (commands[point].opcode == DUP)
		{
			if (top_of_clean(stack) != 0)
			{
				int a = pop(stack);
				push(stack, a, err);
				push(stack, a, err);
				++point;
			}
			else *err = 1;	
		}
		else if (commands[point].opcode == SWP)
		{
			if (top_of_clean(stack) == 2)
			{
				int a = pop(stack), b = pop(stack);
				push(stack, a, err);
				push(stack, b, err);
				++point;
			}
			else *err = 1;	
		}
		else if (commands[point].opcode == POP)
		{
		 	if (top_of_clean(stack) != 0)
			{
				pop(stack);
				++point;
			}
			else *err = 1;
		}
		else if (commands[point].opcode == INP)
		{
			int inp;
			printf("Input number: ");
			scanf("%d", &inp);
			push(stack, inp, err);
			++point;
		}
		if (point >= size_com) *err = 3;
		if (*err != 0)
		{
			*num_str = commands[point].num_str;
			free_stack(stack);
			free_memory(memory, size_memory);
			return 0;
		}
	}
	free_memory(memory, size_memory);
	if (top_of_clean(stack) == 0)
	{
		free_stack(stack);
		*err = 1;
		return 0;	
	}
	result = pop(stack);
	free_stack(stack);
	return result;
}       
