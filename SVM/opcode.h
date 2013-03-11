#ifndef _opcode_h
#define _opcode_h

typedef enum {
	LDC,
	LD,
	ST,
	ADD,
	SUB,
	MUL,
	DIV,
	MOD,
	CMP,
	JMP,
	BR,
	LDI,
	STI,
	DUP,
	SWP,
	POP,
	HLT,
	INP,
	LBL,
	NUL
} Opcode;

typedef struct {
	Opcode opcode;
	int num_str;
	union {
		char * label;
		int number;
		unsigned int address;
		unsigned int num_com;
	};
} Command;	
	           
#endif