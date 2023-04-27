global _start

section .text
_start:
	MOV	EAX, [number]
	CALL	iprintLF
	
end:
	MOV	EAX, 1
	MOV	EBX, 0
	INT	80H
	
changetoletters:
	ADD	EDX, 55
	PUSH	EDX
	CMP	EAX, 0
	JNZ	divideLoop
	JMP	printLoop
	
	
iprint:
	PUSH	EAX
	PUSH	ECX
	PUSH	EDX
	PUSH	ESI
	MOV	ECX, 0
	
divideLoop:
	INC	ECX
	MOV	EDX, 0
	MOV	ESI, 16
	DIV	ESI
	
	CMP	EDX, 10
	JGE	changetoletters
	
	ADD	EDX, 48
	PUSH	EDX
	CMP	EAX, 0
	JNZ	divideLoop
	
	
printLoop:
	DEC	ECX
	MOV	EAX, ESP
	CALL	sprint
	POP	EAX
	CMP	ECX, 0
	JNZ	printLoop
	
	POP	ESI
	POP	EDX
	POP	ECX
	POP	EAX
	RET
	
slen:
	PUSH	EBX
	MOV	EBX, EAX
	
nextchar:
	CMP	byte [EAX], 0
	JZ	finished
	INC	EAX
	JMP	nextchar
	
finished:
	SUB	EAX, EBX
	POP	EBX
	RET
	
sprint:
	PUSH	EDX
	PUSH	ECX
	PUSH	EBX
	PUSH	EAX
	CALL	slen
	
	MOV	EDX, EAX
	POP	EAX
	MOV	ECX, EAX
	MOV	EBX, 1
	MOV	EAX, 4
	INT	80H
	
	POP	EBX
	POP	ECX
	POP	EDX
	RET

iprintLF:
	CALL	iprint
	
	PUSH	EAX
	MOV	EAX, 0aH
	PUSH	EAX
	MOV	EAX, ESP
	CALL	sprint
	
	POP	EAX
	POP	EAX
	RET
	
section .data
number: DD 123456
