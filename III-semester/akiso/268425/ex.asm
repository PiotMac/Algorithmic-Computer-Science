global ex

section .text
ex:
	FLD 	DWORD[ESP+4]	;x
	FLDL2E			;log2(e), x
	FMULP 	st1,st0       	;y = log2(e)*x
	FLD1			;1, y
	FSCALE              	;2^(y), y
	FXCH			;y, 2^(y)
	FLD1			;1, y, 2^(y)
	FXCH                	;y, 1, 2^(y)
	;FPREM	= st(0) <- st(0)/st(1)
	FPREM               	;st(0) = fract(y) = z
	F2XM1               	;st(0) = 2^(z) - 1
	FADDP 	st1,st0       	;st(0) = 2^(z), st(1) = 2^(y)
	FMULP 	st1,st0       	;st(0) = 2^(y) + 2^fract(y) = 2^(x*log2(e))
	FSTP	st1       
	RET
