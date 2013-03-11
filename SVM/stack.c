#include <stdio.h>
#include <stdlib.h>
#include "stack.h"

Stack **new_stack()
{
	Stack **stack;
	stack = (Stack **)malloc(sizeof(Stack *));
	*stack = NULL;
	return stack;
}

void push(Stack **stack, int n, int *err)
{
	Stack *element;
	element = (Stack *)malloc(sizeof(Stack));
	if (element == NULL)
	{
		*err = 4;
		return;
	}
	(*element).value = n;
	(*element).next = *stack;
	*stack = element;	
}

int pop(Stack **stack)
{
	int n =(**stack).value;
	Stack *element = (**stack).next;
	free(*stack);
	*stack = element;
	return n;
}

void free_stack(Stack **stack)
{
	while (*stack != NULL)
	{
		Stack *element = (**stack).next;
		free(*stack);
		*stack = element;		
	}   
	free(stack);
	stack = NULL;
}

int top_of_clean(Stack **stack)
{
	if (*stack == NULL) return 0;
	if ((**stack).next == NULL) return 1;
	return 2;	
}             
