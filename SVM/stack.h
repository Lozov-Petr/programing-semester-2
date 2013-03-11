#ifndef _stack_h
#define _stack_h

typedef struct stack
{
	int value;
	struct stack *next;
}
Stack;

Stack **new_stack();
void push(Stack **, int, int *);
int pop(Stack **);
void free_stack(Stack **);
int top_of_clean(Stack **);
#endif

