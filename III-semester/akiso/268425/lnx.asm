global lnx

section .text
lnx:
	FLDLN2			;st(0) = ln(2)
	FLD	DWORD[ESP+4] 	;st(0) = x, st(1) = ln(2)
	FYL2X			;st(0) = ln(2)*log2(x) = ln(x)
	RET
