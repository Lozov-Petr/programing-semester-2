#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "opcode.h"
#include "parser.h"
#include "interpreter.h"

char *search_path(int, char **, int *);

int main(int argc, char **argv)
{
	char *path;
	int size_com, err = 0, answer, num_str_err;
	Command *commands;
	path = search_path(argc, argv, &err);
	if (err == 1) printf("Entered multiple paths.");
	else if (err == 2) printf("Path is not entered.");
	else if (err == 3) printf("Incorrect format file.");
	else if (err == 4) printf("file with same name does not exist.");
	if (err != 0) return err;
	commands = parser(path, &size_com, &num_str_err, &err);
	if (err != 0)
	{
		printf("String %d: ", num_str_err);
		if (err == 1) printf("Incorrect command/label.");
		else if (err == 2) printf("To long label.");
		else if (err == 3) printf("Incorrect argument, expected number.");
		else if (err == 4) printf("Incorrect argument, expected address.");
		else if (err == 5) printf("Incorrect argument, expected label.");
		else if (err == 6) printf("Incorrect command, or expected ':'.");
		else if (err == 7) printf("The second label.");
                else if (err == 8) printf("After command/label something superfluous.");
		else if (err == 9) printf("No label.");
		else if (err == 10) printf("After label is no commands.");
		return err;
	}      
	answer = interpreter(commands, size_com, &num_str_err, &err);
	if (err == 0) printf("Result: %d.", answer);
	else 
	{
		if (err < 3) 
		{    	
                        printf("String %d: ", num_str_err);
			if (err == 1) printf("Not enough elements in stack.");
			else if (err == 2) printf("Division by zero impossible.");
		}
		else if (err == 3) printf("Expected command 'HLT'.");
		else if (err == 4) printf("Unable to allocate memory.");
	} 
	free(commands);
	commands = NULL;
	return err;
}

char *search_path(int argc, char **argv, int *err)
{
	FILE *input;
	char *path = NULL;
	int i;
	for (i = 0; i < argc - 1; ++i)
	{
		if (!strcmp(argv[i], "-path"))
		{
			if (path == NULL) path = argv[i + 1];
			else 
			{
				*err = 1;
				return NULL;
			} 
		}
	}
	if (!strcmp(argv[argc - 1], "-path")) 
	{
		if (path == NULL) *err = 2;
		else *err = 1; 
		return NULL;
	}
	if (path == NULL)
	{
		*err = 2;
		return NULL;
	}
	for (i = 0; path[i] != 0; ++i);
	if (i < 5) *err = 3;	
	else if (((path[i - 1] != 'm') && (path[i - 1] != 'M')) || ((path[i - 2] != 'v') & (path[i - 2] != 'V')) ||
			((path[i - 3] != 's') && (path[i - 3] != 'S')) || (path[i - 4] != '.')) *err = 3;
	if (*err != 0) return NULL;
	input = fopen(path, "r");
	if (input == NULL)
	{
		*err = 4;
		return NULL;
	}
	fclose(input);
	return path;
}

