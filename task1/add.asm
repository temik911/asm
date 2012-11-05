		org 100h

		mov	di, num1
		mov	cx, numsize
		call	read_long
		mov	si, di

		mov	di, num2
		call	read_long
	 
		call	add_2long             ; add and print
	
		mov	si, string_buf
		add	si, buf_size
		call	print_long

		mov	ax, 4c00h
		int	21h


;Input
;	ds:di - start of long number A
;	ds:si - start of long numner B
;
;Output:
;	set flags like `cmp A, B`
cmp_2long:
		push	di
		push	si
		push	ax
		push	bx

		mov	ax, [di]
		cmp word	ax, [si]		; if len1 != len2 then finish
		jne	cmp_2long_end

		mov	ax, [di]                 ;set counter = len and mov pointers to the end
		add	di, ax
		add	di, ax
		add	si, ax
		add	si, ax
@@:
		mov	bx, [di]
		cmp	bx, [si]               ;finish if different values
		jne	cmp_2long_end

		sub	di, 2                    ; decrease pointers and counter
		sub	si, 2
		dec	ax

		cmp	ax, 0                    ; finish if counter = 0
		jne	@b	
cmp_2long_end:
		pop	bx
		pop	ax
		pop	si
		pop	di
		ret

;Input:
;	ds:di - start of long number
;	ds:si - end of string buffer (last byte, that I can write).
;Output:
;	print numer to display
;	form buffer
;	breake the number
print_long:
		push		si
		push		bx
		push		ax
		push		dx

		mov		bx, 10

@@:
		or word		[di], 0
		jz		@f

		call		div_long

		add		dl, '0'
		mov		[si], dl
		dec		si
		jmp		@b
@@:
		inc	si
		mov	ah, 9
		mov	dx, si
		int	21h

		pop	dx
		pop	ax
		pop	bx
		pop	si
		ret

;Input:
;	ds:di - start of long number
;       bx    - divisor
;
;Output:
;	div long numner
;	dx - remainder
div_long:
		push	si
		push	ax

		mov	si, di                 ; mov si to the last digit
		add	si, [di]
		add	si, [di]

		xor	dx, dx
@@:
		cmp	si, di                 ; finish if we go to the start
		jz	@f

		mov	ax, [si]               ; div current digit
		div	bx
		mov	[si], ax

		sub	si, 2
		jmp	@b
@@:
		pop	ax
		pop	si
		call	update_len
		ret

; input:
;	ds:di - start of long number A
;	ds:si - start of long numner B
;
; output:
;	A -= B if A >= B, and something unvalid if A < B.

add_2long:
		push	dx
		push	bx
		push	bp
		push	di
		push	si
		push	ax

		mov	bx, [di]
		mov	dx, di
		add	dx, bx
		add	dx, bx
	
		mov	bp, si                 ; mov bp to the end of B
		add	bp, [si]
		add	bp, [si]

		clc
		pushf
add_2long_loop:
		add	si, 2
		add	di, 2
		
		cmp	di, dx	
		jbe	add_2long2
		inc	bx
		mov word	[di], 0
add_2long2:

		mov	ax, [si]
		cmp	si, bp
		jbe	@f
		mov	ax, 0
@@:
		popf
		adc	[di], ax
		pushf

		cmp	si, bp
		jb	add_2long_loop
		jc	add_2long_loop         ; repeat if overloading or if not end

		popf
		pop	ax
		pop	si
		pop	di
		mov	[di], bx
		pop	bp
		pop	bx
		pop	dx
		call	update_len
		ret

;Input:
;	ds:di - start of long number;
;
;Output:
;	Decrease lenght of long numner if hight order digits is zeros.
update_len:
		push	si
		push	ax

		mov	si, di                 ; mov si to the end of the number
		mov	ax, [di]
		add	si, [di]
		add	si, [di]

@@:
		or	ax, 0                  ; finish if len is zero or if current digit is not zero
		jz	@f
	
		cmp word	[si], 0
		jnz	@f

		dec	ax                     ; decrease len and repeat
		sub	si, 2
		jmp	@b
	
@@:
		mov	[di], ax
	
		pop	ax
		pop	si
		ret

;Input:
;	ds:di - start of plase to put
;	cx    - size of this place in words
;
;Output:
;	[size of number in words (one word), N words from low order digits to hight] <- ds:di
read_long:
		pusha
	
		mov	word	[di], 0

read_long_read_char:
		dec	cx
		mov	ah, 8h                 ;read char in AL
		int	21h

		cmp	al, 13                 ;repeat if <Enter>
		jz	read_long_finish

		or cx, cx                 ;beep if no enought space
		jnz	@f
		call make_beep
		jmp	read_long_read_char
@@:
		cmp	al, '0'                ;repeat if not digit
		jb	read_long_read_char

		cmp	al, '9'
		jg	read_long_read_char

		mov	dl, al
		mov	ah, 2
		int	21h
	
		mov	bx, 10                 ; add digit in al to long digit
		call	mul_long

		sub	al, '0'
		mov	ah, 0
		mov	bx, ax
		call	add_long
		jmp	read_long_read_char

read_long_finish:
		mov	ah, 9
		mov	dx, crlf
		int	21h

		popa
		ret

;Input:
;	ds:di - start of plase to put
;	bx    - summand
;
;Output:
;	change the source long number
add_long:
		push	si
		push	bx

		mov	si, di                 ; add zero to the end of long number
		add	si, [di]
		add	si, [di]
		add	si, 2
		mov word	[si], 0

		mov	si, di

		clc
@@:
		inc	si
		inc	si
		adc	[si], bx               ; add bx to [si] 
		mov	bx, 0
	
		jc	@b

		mov	bx, di                 ; inc lenght if SI == DI + lenght + 2
		add	bx, [di]
		add	bx, [di]
		add	bx, 2
		cmp	bx, si
		jne	@f
		inc word	[di]
@@:
		pop	bx
		pop	si
		ret


;Input:
;	ds:di - start of plase to put
;	bx    - multipler
;
;Output:
;	change the source long number
mul_long:
		pusha

		mov	si, di                 ;si to start, di to finish, add zero to end, and push start of place
		add	di, 2
		add	di, [si]
		add	di, [si]
		mov	bp, si
		mov word	[di], 0
	
		xor	dx, dx
		clc
		pushf
@@:
		add	si, 2
		mov	cx, dx

		mov	ax, [si]
		mul	bx
		popf
		adc	ax, cx
		pushf
		mov	[si], ax

		cmp	di, si                 ; if si != di repeat
		jnz	@b
	
		or word	[si], 0                ; if end of number is not zero then inc lenght
		jz	@f
		inc word	[bp]
@@:
		popf
		popa
		ret


;Input: nothing
;Output: make beep signal
make_beep:
		push	dx
		push	ax

		mov	dl, 7
		mov	ah, 2
		int	21h

		pop	ax
		pop	dx
		ret


; data sector
crlf:		db	13,10,'$'
minus_string:	db	'-','$'
numsize = 1024
num1:		rw	numsize
num2:		rw	numsize

buf_size = 8192

string_buf:	rb	buf_size
db	13,10,'$'
