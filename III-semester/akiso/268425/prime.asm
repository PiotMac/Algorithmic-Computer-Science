global _start

section .text
_start:
	XOR	ECX, ECX			;ECX is a primary counter
	
fillArray:
	MOV	EAX, ECX			;Moving counter value to EAX
	ADD	EAX, 2				;We start from 2
	MOV	[numbers+4*ECX], EAX		;Array of numbers from 2 to arraySize
	INC	ECX
	CMP	ECX, arraySize			;Filling the whole array with numbers
	JB	fillArray
	
	XOR	ECX, ECX			;ECX = 0
	
loop1:
	MOV	EBX, ECX			;EBX is a counter in loop2
	INC	EBX				;It starts with 1
	CMP	dword [numbers+4*ECX], -1	;Checking if a value is set as non-prime
	JNE	loop2				;If not go to loop2
	
	innerLoop1:				;Point of resuming loop1
		INC	ECX
		CMP	ECX, arraySize		;All values are checked?
		JB	loop1			;If not continue checking
		JMP	print			;If yes go to printing
		
loop2:
	CMP	dword [numbers+4*ECX], -1	;Is value set as non-prime?
	JNE	loop3				;If not go to loop3
	
	innerLoop2:				;Point of resuming loop2
		INC	EBX			;Incrementing counter for loop2
		CMP	EBX, arraySize		;Is every value checked?
		JB	loop2			;If not continue checking
		JMP	innerLoop1		;If yes resume loop1
		
loop3:						;Testing if a value is prime
	XOR	EDX, EDX			;EDX = 0
	XOR	EAX, EAX			;EAX = 0
	MOV	EAX, [numbers+4*EBX]		;Putting number to divide in EAX
	DIV	dword [numbers+4*ECX]		;Dividing the number with the value from outer loop
	CMP	EDX, 0				;Checking for remainder
	JE	notPrime			;If remainder = 0 then the number is not prime
	
	innerLoop3:
		JMP	innerLoop2		;Done, resume loop2
		
notPrime:					;Setting values in an array as non-prime
	MOV	dword [numbers+4*EBX], -1	;Replacing the number with -1
	JMP	innerLoop3			;Resume loop3
	
printPrime:
	PUSH	ECX				;Preserving ECX value
	
	MOV	EAX, [numbers+4*ECX]		;Moving a prime value to EAX
	CALL	iprintLF			;Printing it with linefeed
	
	INC	EBX				
	POP	ECX				
	JMP	resumePrimesArray		
	
print:
	XOR	ECX, ECX				;ECX = 0
	XOR	EBX, EBX				;EBX = 0
	
	printPrimesArray:				;If a value is prime, then print it
		CMP	dword [numbers+4*ECX], -1	
		JNE	printPrime
		
		resumePrimesArray:			;If a value is non-prime, increment index and continue checking
			INC	ECX
			CMP	ECX, arraySize		;Checking if the whole array was searched
			JB	printPrimesArray
		
end:					;End of the program
	MOV	EBX, 0
	MOV	EAX, 1
	INT	80H
	
iprint:					;Preserving all information before changing registers
	PUSH	EAX
	PUSH	ECX
	PUSH	EDX
	PUSH	ESI
	MOV	ECX, 0
	
divideLoop:				;Dividing an integer and converting it to ASCII
	INC	ECX					
	MOV	EDX, 0
	MOV	ESI, 10
	DIV	ESI
	ADD	EDX, 48
	PUSH	EDX
	CMP	EAX, 0
	JNZ	divideLoop

printLoop:				;Printing values from the top of the stack to the last digit of the number
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
	
slen:					;Function to calculate string's length
	PUSH	EBX
	MOV	EBX, EAX
	
nextchar:				;Counting EAX till the NULL pointer
	CMP	byte [EAX], 0
	JZ	finished
	INC	EAX
	JMP	nextchar
	
finished:				;Calculating length
	SUB	EAX, EBX
	POP	EBX
	RET
	
sprint:					;Printing string
	PUSH	EDX
	PUSH	ECX
	PUSH	EBX
	PUSH	EAX
	CALL	slen			;Calculating it's length
	
	MOV	EDX, EAX		;Using sys_write
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
	CALL	iprint			;Printing an integer
	
	PUSH	EAX			;Printing an integer with linefeed
	MOV	EAX, 0aH
	PUSH	EAX
	MOV	EAX, ESP
	CALL	sprint
	
	POP	EAX
	POP	EAX
	RET

section .data
arraySize: EQU 100000
numbers: TIMES arraySize DD 0
