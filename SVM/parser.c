#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "opcode.h"

#define SIZE_COMMAND 10
#define SIZE_LABEL 10
#define SIZE_WORD 48
#define SIZE_STRING 256

typedef struct {
	char *lbl;
	int num;
	int num_str;
} Label;

char *read_word(char *, int *, int *);
int read_number(char *, int *, int *);
unsigned int read_address(char *, int *, int *);
Opcode is_command(char *);
void put_in_array(Label *, int *, char *, int, int, int *);
void replacement(Command *, int, Label *, int, int *, int *);
void clean_arrays(Command *, int, Label *, int);


Command *parser(char *path, int *size_com, int *num_str, int *err)
{
	Command *commands;
	Label *labels;
	int point_com = 0, point_lbl = 0;
	FILE *input;
	*num_str = 0;
	input = fopen(path, "r");
	commands = (Command *)malloc(sizeof(Command) * SIZE_COMMAND);
	labels = (Label *)malloc(sizeof(Label) * SIZE_LABEL);
	while(!feof(input))
	{
		char str[SIZE_STRING], *lexeme;
		int point_str = 0;
		str[0] = 0;
		++(*num_str);
		if ((point_com - 1) % SIZE_COMMAND == 0)
			commands = (Command *)realloc(commands, sizeof(Command) * (point_com + SIZE_COMMAND));
                if ((point_lbl - 1) % SIZE_LABEL == 0)
			labels = (Label *)realloc(labels, sizeof(Label) * (point_lbl + SIZE_LABEL));
		fgets(str, SIZE_STRING - 1, input);
		lexeme = read_word(str, &point_str, err);
		if (*err != 0)
		{
			clean_arrays(commands, point_com, labels, point_lbl);	
			return NULL;
		}
		commands[point_com].opcode = is_command(lexeme);
		if (commands[point_com].opcode == LDC) 
			commands[point_com].number = read_number(str, &point_str, err);
		else if ((commands[point_com].opcode == LD) || (commands[point_com].opcode == ST))
			commands[point_com].address = read_address(str, &point_str, err);
		else if ((commands[point_com].opcode == BR) || (commands[point_com].opcode == JMP))
		{
			commands[point_com].label = read_word(str, &point_str, err);
			if (*err == 1) *err = 5;
			if (*err == 0)
			{
				if (is_command(commands[point_com].label) == LBL)
					put_in_array(labels, &point_lbl, commands[point_com].label, -1, *num_str, err);	
				else *err = 5; 
			}
		}
		else if (commands[point_com].opcode == LBL)
		{
			while ((str[point_str] == ' ') || (str[point_str] == 9)) ++point_str;
			if (str[point_str++] == ':')
				put_in_array(labels, &point_lbl, lexeme, point_com, *num_str, err);
			else *err = 6;
		}
		if (*err != 0)
		{
			clean_arrays(commands, point_com + 1, labels, point_lbl);
			return NULL;
		}
		while ((str[point_str] == ' ') || (str[point_str] == 9)) ++point_str;
		if ((str[point_str] != ';') && (str[point_str] != 0) && (str[point_str] != '\n'))
		{
			*err = 8;
			clean_arrays(commands, point_com + 1, labels, point_lbl);
			return NULL;
		}
		if ((commands[point_com].opcode != LBL) && (commands[point_com].opcode != NUL)) 
		{
			commands[point_com].num_str = *num_str;
			++point_com;
		}
	}  
	replacement(commands, point_com, labels, point_lbl, num_str, err);
	if (*err != 0)
	{
		clean_arrays(commands, point_com, labels, point_lbl);
		return NULL;
	}	
	else 
	{
		*size_com = point_com;
		return commands;
	}	
}

char *read_word(char *str, int *point_str, int *err)
{
	int j = 0;
	char *word;
	word = (char *)malloc(sizeof(char) * SIZE_WORD);
	while ((str[*point_str] == ' ') || (str[*point_str] == 9)) ++(*point_str);
	while ((str[*point_str] != ' ') && (str[*point_str] != 9) && (str[*point_str] != 0) &&
		(str[*point_str] != '\n') && (str[*point_str] != ';') && (str[*point_str] != ':'))
	{
		if (((((str[*point_str] >= 48) && (str[*point_str] <= 57)) || (str[*point_str] == 95)) && 
			(j != 0)) || ((str[*point_str] >= 65) && (str[*point_str] <= 90)) || 
			((str[*point_str] >= 97) && (str[*point_str] <= 122)))
		{
			if (str[*point_str] >= 97) word[j++] = str[(*point_str)++] - 32;
			else word[j++] = str[(*point_str)++]; 
		}
		else
		{
			*err = 1;
			free(word);
			return NULL;
		}
		if (j == SIZE_WORD - 1)
		{
			*err = 2;
			free(word);
			return NULL;
		}	
	}
	word[j] = 0;
	return word;		
} 

int read_number(char *str, int *point_str, int *err)
{
	int number = 0, sign = 1;
	while ((str[*point_str] == ' ') || (str[*point_str] == 9)) ++(*point_str);
	if (str[*point_str] == '-')
	{
		sign = -1;
		++(*point_str);
	}
	do
	{
		if ((str[*point_str] >= 48) && (str[*point_str] <= 57))
			number = 10 * number + str[(*point_str)++] - '0';
		else
		{
			*err = 3;
			return 0;
		}
	}
	while ((str[*point_str] != ' ') && (str[*point_str] != 9) && (str[*point_str] != 0) &&
		(str[*point_str] != '\n') && (str[*point_str] != ';')); 
	return number * sign;
}

unsigned int read_address(char *str, int *point_str, int *err)
{
	int number = 0;
	while ((str[*point_str] == ' ') || (str[*point_str] == 9)) ++(*point_str);
	do
	{
		if ((str[*point_str] >= 48) && (str[*point_str] <= 57))
			number = 10 * number + str[(*point_str)++] - '0';
		else
		{
			*err = 4;
			return 0;
		}
	}
	while ((str[*point_str] != ' ') && (str[*point_str] != 9) && (str[*point_str] != 0) &&
		(str[*point_str] != '\n') && (str[*point_str] != ';')); 
	return number;	
}

Opcode is_command(char *lexeme)
{
	Opcode opcode;
	int len = strlen(lexeme);
	if ((lexeme[0] == 0) || (lexeme == NULL)) opcode = NUL;
	else if (len == 2)
	{
		if (!stricmp(lexeme, "LD")) opcode = LD;
		else if (!strcmp(lexeme, "ST")) opcode = ST;
		else if (!strcmp(lexeme, "BR")) opcode = BR;
		else opcode = LBL;
	}
	else if (len == 3)
	{
		if (!stricmp(lexeme, "LDC")) opcode = LDC;
		else if (!strcmp(lexeme, "ADD")) opcode = ADD;
		else if (!strcmp(lexeme, "SUB")) opcode = SUB;
		else if (!strcmp(lexeme, "MUL")) opcode = MUL;
		else if (!strcmp(lexeme, "DIV")) opcode = DIV;
		else if (!strcmp(lexeme, "MOD")) opcode = MOD;
		else if (!strcmp(lexeme, "CMP")) opcode = CMP;
		else if (!strcmp(lexeme, "JMP")) opcode = JMP;
        	else if (!strcmp(lexeme, "DUP")) opcode = DUP;
		else if (!strcmp(lexeme, "POP")) opcode = POP;
		else if (!strcmp(lexeme, "SWP")) opcode = SWP;
		else if (!strcmp(lexeme, "LDI")) opcode = LDI;
		else if (!strcmp(lexeme, "STI")) opcode = STI;
		else if (!strcmp(lexeme, "HLT")) opcode = HLT;
		else if (!strcmp(lexeme, "INP")) opcode = INP;
		else opcode = LBL;
	}
	else opcode = LBL;
	if (opcode != LBL) 
	{
		free(lexeme);
		lexeme = NULL;
	}
	return opcode;
}

void put_in_array(Label *labels, int *point_lbl, char *label, int num_com, int num_str, int *err)
{
  	int i;
	for (i = 0; i < *point_lbl; ++i)
	{
		if (!stricmp(label, labels[i].lbl))
		{
			if (labels[i].num != -1)
			{
				if (num_com != -1)
				{
					free(label);
					label = NULL;
					*err = 7;
					return;
				}
			}
			else
			{
				labels[i].num = num_com;
				labels[i].lbl = label;
				labels[i].num_str = num_str;
			}
			return;
		}		
	}
	labels[*point_lbl].num = num_com;
	labels[*point_lbl].num_str = num_str;
	labels[(*point_lbl)++].lbl = label;
}

void replacement(Command *commands, int size_com, Label *labels, int size_lbl,  int *num_str, int *err)
{
	int i, j;
	for (j = 0; j < size_lbl; ++j)
	{
		if (labels[j].num == -1)
		{
			*num_str = labels[j].num_str;
			*err = 9;
			return;	
		}
		if (labels[j].num >= size_com)
		{
			*num_str = labels[j].num_str;
			*err = 10;
			return;
		} 
	}	
	for (i = 0; i < size_com; ++i)
	{
		if ((commands[i].opcode == BR) || (commands[i].opcode == JMP))
		{
			for (j = 0; j < size_lbl; ++j)
			{
		        	if (!strcmp(labels[j].lbl, commands[i].label))
				{
	                        	free(commands[i].label);
					commands[i].num_com = labels[j].num;
					break;
				}
			} 
		}
	}
	for (j = 0; j < size_lbl; ++j) free(labels[j].lbl);
	free(labels);
	labels = NULL;
}

void clean_arrays(Command *commands, int point_com, Label *labels, int point_lbl)
{
	int i;
	for (i = 0; i < point_com; ++i)
		if ((commands[i].opcode == BR) || (commands[i].opcode == JMP)) free(commands[i].label);
	free(commands);
	commands = NULL;
	for (i = 0; i < point_lbl; ++i)
		if (labels[i].num != -1) free(labels[i].lbl);
	free(labels);
	labels = NULL;
}
