[org 0x0100]

jmp start

oldisr: dd 0 ; space for saving old isr 
oldTisr: dd 0 ; space for saving old isr 
location:dw 3050; location of fish
initialrow:dw 19
upperboundary:dw 2880
lowerboundary: dw 3840
fish:dw 0x17E0
flagtimer: db 1
flag3: db 1
flag4: db 0

flag_quit: db 0

flag_removered: db 0
flag_removegreen: db 0

counter_red: dw 0
counter_green: dw 0

flag_coin: db 0			;---------------- 0=red , 1=green

prevlocation_red: dw 0
prevlocation_green: dw 0

greenCoin: dw 0x12E9
redCoin: dw 0x14E9

score: dd 0

tickcount: dw 0
randnum1: dw 0

esc_flag: db 0

message_score: db 'SCORE = '

message_load1: db 'Loading....'
message1:	db 'Enter your name:$'
message2:	db 'Hello $'
message3:	db 'Welcome to SWIM TO WIN $'
message4:	db 'Instructions: $'
message5:	db '*Score will be displayed at top right corner of screen $'
message6:	db '*You can move your fish by up,down,left and right keys of keyboard $'
message7:	db 'Press $'
message8:	db ' ENTER $' 
message9:	db 'to continue or $'
message10:	db ' ESC $'
message11:	db 'to exit $'
message12:	db 'Developed by: $'
message13:	db 'Ariba Arshad (21L-5381) $'
message14:	db 'Do you want to quit? $'
message15:	db ' Yes (y) $'
message16:	db ' No (n) $'

message_i:	db 'SWIM TO WIN $'

buffer:		db 80 								; Byte # 0: Max length of buffer
db 0 											; Byte # 1: number of characters on return
times 80 db 0 									; 80 Bytes for actual buffer space
;buffer ends

buffer2: times 4000 db 0 ; space for 4000 bytes
;------------------------------------------------------------------------------------
sound: push ax
	push cx

mov cx, 5

loop1:         mov al, 0b6h
out 43h, al

;load the counter 2 value for d3
mov ax, 1fb4h
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay

;load the counter 2 value for a3
mov ax, 152fh
out 42h, al
mov al, ah
out 42h, al

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay
	
;load the counter 2 value for a4
mov ax, 0A97h
out 42h, al
mov al, ah
out 42h, al
	
;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
call delay
mov al, ah
out 61h, al

call delay
 
 loop loop1

pop cx
pop ax
ret

;-------------------------------------------------------------------
; keyboard interrupt service routine
;-------------------------------------------------------------------
kbisr:		push es
			push ax
			push cx
			push si
			push di
			push bx
			push dx
			mov ax, 0xb800
			mov es, ax ; point es to video memory

			in al, 0x60 		; read a char from keyboard port

;----------------------------------------------(LEFT)-----------------------------------------------------------------------
			cmp al, 0x4b 		; has the left shift pressed
			jne nextcmp

			mov ax,[cs:initialrow]		;calculate cell number 
			mov cx,80
			mul cx
			shl ax,1
		
			mov di,[cs:location]	;load location of fish in di
			mov word[es:di],0x1020		;store fish in new location
			cmp di,ax
			je samerow

			sub di,2		;for left subtract one cell
	
			mov cx,di
			push cx
			call CheckCoin
			
			mov ax,[cs:fish]		;load fish
			mov [es:di],ax		;store fish in new location
			mov [cs:location],di	;update location

			jmp exit		; leave interrupt routine

samerow:		mov ax,[cs:fish]		;load fish
			add di,158
			
			mov [es:di],ax		;store fish in new location
			mov [cs:location],di	;update location

			jmp exit		; leave interrupt routine

nextcmp:	cmp al, 0xcb			 ; has the left released
			jne nextcmp1 ; no, try next comparison

			jmp exit ; leave interrupt routine 
;----------------------------------------------(RIGHT)-----------------------------------------------------------------------
nextcmp1:			cmp al, 0x4d 		; has the right shift pressed
			jne nextcmp2

			mov ax,[cs:initialrow]
			mov cx,80
			mul cx
			shl ax,1
			add ax,158
			
			mov di,[cs:location]	;load location of fish in di
			mov word[es:di],0x1020		;store fish in new location
			cmp di,ax
			je samerow2

			add di,2		;for right add one cell
			
			mov cx,di
			push cx
			call CheckCoin
			
			mov ax,[cs:fish]		;load fish
			mov [es:di],ax		;store fish in new location
			mov [cs:location],di	;update location

			jmp exit		; leave interrupt routine

samerow2:		mov ax,[cs:fish]		;load fish
			sub di,158
			
			mov [es:di],ax		;store fish in new location
			mov [cs:location],di	;update location

			jmp exit		; leave interrupt routine

nextcmp2:	cmp al, 0xcd			 ; has the right released
			jne nextcmp3 ; no, try next comparison

		jmp exit		; leave interrupt routine
;-----------------------------------------------(UP)----------------------------------------------------------------------
nextcmp3:			cmp al, 0x48 		; has the left shift pressed
			jne nextcmp4

			mov ax,[cs:upperboundary]
			
			mov di,[cs:location]	;load location of fish in di
			cmp di,ax
			jbe notification
			
			mov word[es:di],0x1020		;store fish in new location
			sub di,160		;for left subtract one cell
			
			mov cx,di
			push cx
			call CheckCoin
			
			mov ax,[cs:fish]		;load fish
			mov [es:di],ax		;store fish in new location
			mov [cs:location],di	;update location

			mov cx,[cs:initialrow]
			sub cx,1
			mov [cs:initialrow],cx

			jmp exit		; leave interrupt routine

notification:	call sound
			jmp exit		; leave interrupt routine

nextcmp4:	cmp al, 0xc8			 ; has the left released
			jne nextcmp5 ; no, try next comparison

			jmp exit ; leave interrupt routine 

;---------------------------------------------------(DOWN)------------------------------------------------------------------
nextcmp5:			cmp al, 0x50 		; has the pressed
			jne nextcmp6

			mov ax,[cs:lowerboundary]
			
			mov di,[cs:location]	;load location of fish in di
			cmp di,ax
			jae notification2
			
			mov word[es:di],0x1020		;store fish in new location
			add di,160		;for left subtract one cell
			
			mov cx,di
			push cx
			call CheckCoin
			
			mov ax,[cs:fish]		;load fish
			mov [es:di],ax		;store fish in new location
			mov [cs:location],di	;update location

			mov cx,[cs:initialrow]
			add cx,1
			mov [cs:initialrow],cx

			jmp exit		; leave interrupt routine

notification2:		call sound
			jmp exit		; leave interrupt routine

nextcmp6:	cmp al, 0xd0			 ; has the left released
		jne EscKey
			jmp exit		; leave interrupt routine
				
EscKey: cmp al, 0x01
	jne Compare_yes
	mov byte[cs:esc_flag],1
	mov byte[cs:flagtimer],0

	call saveScreen
	call clrscr
	call warning
	
	jmp exit
	
Compare_yes:	cmp byte[cs:esc_flag],1
				jne exit
		cmp al,0x15 ; y
		jne check
		mov byte[cs:flag3],0
		jmp exit
		
check:		cmp al,0x31 ; n
			jne exit
			mov byte[cs:flag3],1
	
			mov byte[cs:flagtimer],1	
			call restoreScreen

exit:			pop dx
			pop bx
			pop di
			pop si
			pop cx
			pop ax
			pop es

			jmp far [cs:oldisr]	 ; call the original ISR

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again
mov di, 140 ; point di to 70th column
nextpos: pop dx ; remove a digit from the stack
mov dh, 0x30 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax 
pop es
pop bp
ret 2

;------------------------------------------------------------------------------------------
;------------------------------------------------------
; timer interrupt service routine
;------------------------------------------------------
timer:
	push ax
	cmp byte[cs:flagtimer],1
	jne skipall
	
	call movescreen
	
	call CoinFunction
	
skipall:	mov al, 0x20
	out 0x20, al ; send EOI to PIC
	pop ax
	
	iret
;------------------------------------------------------------------------------------------

CoinFunction:	push ax
		push bx
		push dx
		push cx
		push di
		push si
		
	add word[cs:tickcount],2
	
	inc word[cs:counter_green]
	cmp word[cs:counter_green],182
	jne checkredCoin
	
	mov byte[cs:flag_removegreen],1
	jmp generaterandnum
	
checkredCoin:	inc word[cs:counter_red]
	cmp word[cs:counter_red],91
	jne exit4
	mov byte[cs:flag_removered],1	
	
generaterandnum:	call randomnum
	
	push word[cs:randnum1]		
	call printcoins
	
	;---------------------------------------
exit4:			pop si
		pop di
		pop cx
		pop dx
		pop bx
		pop ax

		ret

;------------------------------------------------------------------------------------------

randomnum:	push ax
		push bx
		push dx
		push cx
		push di
		push si

cmp byte[cs:flag_removered],1
jne checkgreen
mov word[cs:counter_red],0
jmp sysTime

checkgreen:	cmp byte[cs:flag_removegreen],1
mov word[cs:counter_green],0

generate_again:  
sysTime:	mov ah, 2ch           ;Get time
int 21h   
mov [cs:randnum1],dh 

combination1:	shl word[cs:randnum1],3
				mov bx,[cs:tickcount]
				add [cs:randnum1],bx
				jmp compare1
				
compare1:	cmp word[cs:randnum1],4000
			jb compare2
			jmp	decrement

compare2:	cmp word[cs:randnum1],2880
			jb increment
			jmp	exit2

decrement:	sub word[cs:randnum1],1000
			cmp word[cs:randnum1],4000
			ja decrement
			
			jmp exit2
			
increment:	add word[cs:randnum1],1000
			cmp word[cs:randnum1],2880
			jb increment
		
exit2:	mov ax,[cs:randnum1]
		cmp word[cs:location],ax
		je generate_again	
		pop si
		pop di
		pop cx
		pop dx
		pop bx
		pop ax

		ret

;--------------------------------------------------------------------------------------------

RemoveCoin:	push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

	mov ax,0xb800
mov es,ax
xor ax,ax

mov di,[bp+4]

mov word[es:di],0x1020

exit3:	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	
	ret 2
	
;---------------------------------------------------------------------------------------

printcoins:	push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		
	mov ax,0xb800
mov es,ax
xor ax,ax
mov di,[bp+4]	

cmp byte[cs:flag_removegreen],1
jne CheckRed

push word[cs:prevlocation_green]
call RemoveCoin

mov ax,[cs:greenCoin]
mov word[es:di],ax

mov word[cs:prevlocation_green],di

jmp exit5

CheckRed:	cmp byte[cs:flag_removered],1
			jne exit5
push word[cs:prevlocation_red]
call RemoveCoin

mov ax,[cs:redCoin]

mov word[es:di],ax

mov word[cs:prevlocation_red],di

exit5:	mov byte[cs:flag_removegreen],0
mov byte[cs:flag_removered],0

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2

;----------------------------------------------------------------------------

FirstPrint:		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		
	mov byte[cs:flag_coin],0
	call randomnum	
	push word[cs:randnum1]
	push word[cs:redCoin]
	call printcoins_1
	
	mov word[cs:counter_red],0
	
greenCoinEaten:	call randomnum	
	mov byte[cs:flag_coin],1
	push word[cs:randnum1]
	push word[cs:greenCoin]
	call printcoins_1
	
	mov word[cs:counter_green],0

done:	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es

	ret
;---------------------------------------------------	
printcoins_1: push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

mov ax,0xb800
mov es,ax
xor ax,ax	

mov ax,[bp+4]

mov di,[bp+6]
cmp byte[cs:flag_coin],0
jne initialPrint_G
mov word[cs:prevlocation_red],di
mov word[es:di],ax
jmp exit6

initialPrint_G:	
mov word[es:di],ax
mov word[cs:prevlocation_green],di

exit6:	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 4
	
;------------------------------------------------------------------------------------------
movescreen:	
			push es
			push ax
			push cx
			push si
			push di
			push bx
			push dx
			
			
		mov ax,160
		push ax			;starting point .....[bp+6]
		mov ax,7
		push ax			;ending point.....[bp+4]
 		
		call moveleft

		mov ax,1280
		push ax			;starting point .....[bp+6]
		mov ax,9
		push ax			;ending point.....[bp+4]
 		
		call moveright
		
			
			pop dx
			pop bx
			pop di
			pop si
			pop cx
			pop ax
			pop es

			ret 

;------------------------------------------------------------------------------------------
delay:      push cx
			mov cx, 0xFFFF
l1:		loop l1
		mov cx, 0xFFFF
l2:		loop l2
			
			pop cx
			ret
;------------------------------------------------------------------------------------------
moveleft:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di
			push bx
			push dx

		
		mov ax, 0xb800
		mov es, ax			; point es to video base
		
		mov di,[bp+6]			;starting point
		
		mov cx,[bp+4]			;ending row

externalLoop: mov dx,di		;save initial value of di
		add di,158			;move di to last cell of that row
		mov ax,word[es:di]	;save cell value in ax
		sub di,2			;move di to previous cell
internalLoop: 
		
		mov bx,word[es:di]	;save cell value in bx
		mov word[es:di],ax	; move saved value of next cell in previous cell

		sub di,2			; move di one cell back
		
		mov ax,word[es:di]	;save cell value in ax
		mov word[es:di],bx	; move saved value of next cell in previous cell

		sub di,2			; move di one cell back
		cmp di,dx			; compare if di has reached the start of that row
		jne internalLoop

update:		
		mov bx,word[es:di]	;save value of first cell of that row
		mov word[es:di],ax	; move saved value of last cell in first cell 
		add di,158			;move di to last cell
		mov word[es:di],bx	;move saved value of first cell in last cell 
		add di,2			;move di to next row
		loop externalLoop

			pop dx
			pop bx
			pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp

			ret 4

;------------------------------------------------------------------------------------------

moveright:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di
			push bx
			push dx

		
		mov ax, 0xb800
		mov es, ax			; point es to video base
		
		mov di,[bp+6]			;starting point
		
		mov cx,[bp+4]			;ending row

externalLoop1: mov dx,di	;save initial value of di
		add dx,158			;move dx to last value
		mov ax,word[es:di]	;save cell value in ax
		add di,2			;move di to next cell
internalLoop1: 
		
		mov bx,word[es:di]	;save cell value in bx
		mov word[es:di],ax	; move saved value of previous cell in next cell

		add di,2			;move di to next cell
		
		mov ax,word[es:di]	;save cell value in ax
		mov word[es:di],bx	; move saved value of previous cell in next cell

		add di,2			;move di to next cell
		cmp di,dx			; compare if di has reached end of that row
		jne internalLoop1

update1:		
		mov bx,word[es:di]	;save cell value in bx
		mov word[es:di],ax	; move saved value of first cell in last cell 
		sub di,158			;move di to first cell of that row
		mov word[es:di],bx	; move saved value of last cell in first cell 
		add di,160			;move di to next row
		loop externalLoop1

			pop dx
			pop bx
			pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp

			ret 4

;------------------------------------------------------------------------------------------
setbackground:	push bp
		mov bp, sp
		push es
		push ax
		push cx
		push di
	
		mov ax, 0xb800
		mov es, ax			; point es to video base
		mov di,[bp+8]		;starting point
		mov ax,[bp+4]		;colour for sky
		mov cx,[bp+6]

	cld						;set auto-increment mode
	rep stosw				;mov word[es:di],ax  
							;add di,2
							;dec cx
							;cmp cx,0	
						
	
			pop di
			pop cx
			pop ax
			pop es
			pop bp
			ret 6

;------------------------------------------------------------------------------------------
printMountains:	push bp
				mov bp,sp
				push es
				push ax
				push cx
				push bx
				push dx
				push si
				push di
mov ax,0xb800
mov es,ax

mov ax,[bp+4]
mov cx,80
mul cx

mov cx,[bp+6]
add ax,cx

shl ax,1		; cell loaction

mov si,ax
mov cx,32

mov al,0x66
mov dx,9

outerloop:	mov di,si
			mov bx,cx

innerloop_m:	cld 
				rep stosb				; ax=0x6720 moved in [es:di] and di=di+1 will run until cx=0
				
			
			sub bx,4
			mov cx,bx
			sub si,158
			dec dx
			jnz outerloop

				
pop di
pop si
pop dx
pop bx
pop cx
pop ax
pop es
pop bp

ret 4
;------------------------------------------------------------------------------------------
printship:	push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di
		push bx
		push dx

		mov ax, 0xb800
		mov es, ax			; point es to video base

		
mov ax,[bp+6]	;row
mov cx,80
mul cx

mov cx,[bp+4]	;column
add ax,cx

shl ax,1		; cell loaction

mov si,ax
mov ax,[bp+14]
		
mov bx,[bp+10]			;length of ship		
mov dx,3

outerloop2:	mov di,si
			mov cx,bx

innerloop_s: cld 
			rep stosw				; ax=0x6720 moved in [es:di] and di=di+1 will run until cx=0
			
		sub bx,2
		add si,162
		
		dec dx
		jnz outerloop2		
		
mov ax,[bp+6]
mov cx,80
mul cx

mov cx,[bp+4]
add ax,cx

shl ax,1		; cell loaction	

mov si,ax
	
mov ax,[bp+10]
shr ax,1

add si,ax
add si,2
mov di,si

sub di,160

mov ax,0x7020	
mov si,[bp+8]			;length of pole

mov cx,si				;cx = length of pole

printpole:  stosw
	   sub di,162
	   loop printpole

flag: xor ax,ax				;print flag at top of pole(current di value)
	mov ax,0x173C
	add di,162
	sub di,4
	stosw
	
mov ax,[bp+6]	;column
mov cx,80
mul cx

mov cx,[bp+4]	;row
add ax,cx

shl ax,1		; cell loaction

mov si,ax	
	

windows: mov ax,[bp+14]
		mov al,0x2A
	mov cx,[bp+12]			;number of windows to print
	mov di,si			;row
	add di,164

printwindows:	 stosw
		add di,2
		loop printwindows
		
			pop dx
			pop bx
			pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp

			ret 12

;------------------------------------------------------------------------------------------
printsea:	push bp
		mov bp, sp
		push es
		push ax
		push cx
		push di

		mov ax, 0xb800
		mov es, ax			; point es to video base
		mov di,[bp+8]		;starting point
		mov ax,[bp+4]		;colour 
		mov cx,[bp+6]		;ending point

	cld						;set auto-increment mode
	rep stosw				;mov word[es:di],ax  
							;add di,2
							;dec cx
							;cmp cx,0

			pop di
			pop cx
			pop ax
			pop es
			pop bp
			ret 6
;------------------------------------------------------------------------------------------

initialize: pusha

mov ax,0
	push ax			;starting point.....[bp+8]
	mov ax,1280
	push ax			;ending point.....[bp+6]
 	
	mov ax,0x3720
	push ax			;colour for sky.....[bp+4]

	call setbackground
	
	call player_score

	;----------Mountain 1------------------
			
				mov ax,0
		push ax		;------------[bp+6] column

		mov ax,8	
		push ax		;-------------[bp+4] row
		
		call printMountains

	;----------Mountain 2------------------
		
				
		mov ax,16
		push ax		;------------[bp+6] column

		mov ax,8	
		push ax		;-------------[bp+4] row
		
		call printMountains

	;----------Mountain 3------------------
		
		mov ax,32
		push ax		;------------[bp+6] column

		mov ax,8	
		push ax		;-------------[bp+4] row
		
		call printMountains

	;----------Mountain 4------------------
		
		mov ax,48
		push ax		;------------[bp+6] column

		mov ax,8	
		push ax		;-------------[bp+4] row
		
		call printMountains

	;----------Mountain 5------------------
		
		mov ax,64
		push ax		;------------[bp+6] column

		mov ax,8	
		push ax		;-------------[bp+4] row
		
		call printMountains

	;----------------------------------------------------------

	mov ax,1280
	push ax			;starting point.....[bp+8]
	mov ax,1440
	push ax			;ending point.....[bp+6]
 	
	mov ax,0x1020
	push ax			;colour for sea.....[bp+4]

	call printsea
	;----------------------------------------------------------
		;----------Ship 1------------------
		
		mov ax,0x5720			
		push ax				; ............[bp+14]
		
		mov ax,7			
		push ax				; push number of windows............[bp+12]

		mov ax,16			
		push ax				; push length of ship............[bp+10]

		mov ax,2			
		push ax				; push length of pole............[bp+8]

		mov ax,10			
		push ax				; push row............[bp+6]

		mov ax,64
		push ax				; push column............[bp+4]

		call printship

		;----------Ship 2------------------
		mov ax,0x4720			
		push ax				; ............[bp+14]
		
		mov ax,13			
		push ax				; push number of windows............[bp+12]

		mov ax,28			
		push ax				; push length of ship............[bp+10]

		mov ax,2			
		push ax				; push length of pole............[bp+8]

		mov ax,11			
		push ax				; push row............[bp+6]

		mov ax,20
		push ax				; push column............[bp+4]

		call printship

	popa
	ret

;------------------------------------------------------------------------------------------

printfish:		push es
			push ax
			push di

			mov ax, 0xb800 				; load video base in ax
			mov es, ax 					; point es to video base
			mov di,[location] 						    ; es:di pointint to --> 0xB800:0000 (B8000)	
		mov bx,[fish]
		mov word [es:di], bx 	
			
			pop di
			pop ax
			pop es
			ret
				
;------------------------------------------------------------------------------------------
;---------------- PHASE 4 --------------------------------------------------
;------------------------------------------------------------------------------------------------

;----------------------------------------------------------------------------

player_score:	push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		
mov ax,0xb800
mov es,ax

mov di,0

mov cx,80
mov ax,0x3720

cld
rep stosw

		mov ah, 0x13			; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 

		mov bh, 0			; output on page 0
		
		mov bl,0x30			; normal attrib
		mov cx, 8			; length of string
		mov dx, 0x003E			; row 10 column 3
		
		push ds
		pop es				; es=ds segment of string
		mov bp, message_score		; bp = offset of string
		
		INT 0x10			; call BIOS video service

		push word[cs:score]
		call printnum

mov ah,0x02
	mov bh,0
	mov dx,0x1900
	int 0x10			
		
		pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	ret 
		
		
;----------------------------------------------------------------------

incrementscore:	push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di		
		
	mov ax,[bp+4]
	add [cs:score],ax	

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2

;------------------------------------------------------------------------

CheckCoin:	push bp
		mov bp,sp
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di	

	mov ax,0xb800
	mov es,ax
	
	mov di,[bp+4]
	
	mov ax,[es:di]
	
	cmp ax,0x14E9
	jne greencoin_check
	
	mov cx,50		; if coin is red
	push cx
	call incrementscore
	call player_score
	
	call print_red

	jmp nomatch	
	
greencoin_check:	cmp ax,0x12E9
	jne nomatch
	
	mov cx,10		;if coin is green
	push cx
	call incrementscore
	call player_score
	
	call print_green

nomatch:	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
	
;----------------------------------------------------------------------------------------
print_green:	push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

	mov ax,0xb800
	mov es,ax
	
	call randomnum
	
	mov di,[cs:randnum1]
	mov word[es:di],0x12E9
	mov [cs:prevlocation_green],di
	mov word[cs:counter_green],0

exit_g:	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	ret 

;------------------------------------------------------------------------------------------
print_red:	push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

	mov ax,0xb800
	mov es,ax
	
	call randomnum
	
	mov di,[cs:randnum1]
	mov word[es:di],0x14E9
	mov [cs:prevlocation_red],di
	mov word[cs:counter_red],0

exit_r:	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	ret 

;------------------------------------------------------------------------------------------

 ;-----------------------------------------------------------------
; subroutine to save the screen
;-----------------------------------------------------------------
saveScreen:	push ax
			push bx
			push cx
			push dx
			push si
			push di
			push ds
			push es

			mov cx, 4000 ; number of screen locations
			mov ax, 0xb800
			mov ds,ax ; ds = 0xb800

			push cs
			pop es
		
			mov si, 0
			mov di, buffer2

			cld ; set auto increment mode
			rep movsb ; save screen

			;[es:di] = [ds:si]
pop es			
pop ds
pop di
pop si
pop dx
pop cx
pop bx
pop ax

			ret
;-----------------------------------------------------------------
;-----------------------------------------------------------------
; subroutine to restore the screen
;-----------------------------------------------------------------
restoreScreen:		push ax
			push bx
			push cx
			push dx
			push si
			push di
			push ds
			push es

			mov cx, 4000 ; number of screen locations
			mov ax, 0xb800
			mov es, ax ; ds = 0xb800

			push cs
			pop ds
		
			mov si, buffer2
			mov di, 0

			cld ; set auto increment mode
			rep movsb ; save screen

			;[es:di] = [ds:si]	
pop es			
pop ds
pop di
pop si
pop dx
pop cx
pop bx
pop ax

			ret
;-----------------------------------------------------------------	
	
;------------------------------------------------------------------------------------------
;---------------- PHASE 5 --------------------------------------------------
;------------------------------------------------------------------------------------------------

clrscr:	push es
	push ax
	push cx
	push di

	mov ax, 0xb800
	mov es, ax ; point es to video base
	xor di, di ; point di to top left column ... es:di-->b800:0000

	mov ax, 0x0720 ; space char in normal attribute
	mov cx, 2000 ; number of screen locations
	
	cld ; auto increment mode
	rep stosw ; clear the whole screen

	pop di 
	pop cx
	pop ax
	pop es
	ret

;---------------------------------------------------------------------------------------------------------
printrope:	push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

mov ax,0x374F
mov di,126
mov cx,5
rope2:	mov word[es:di],ax
		add di,158

loop rope2

add di,4
mov word[es:di],ax


sub di,158
mov word[es:di],ax

sub di,162
mov cx,4
rope3:	mov word[es:di],ax
		sub di,162

loop rope3

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	
	ret
	
;---------------------------------------------------------------------------------------------------------

printanchor:	push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

mov ax,0xb800
mov es,ax

mov di,130
mov ax,0x374F
mov cx,6

rope:	mov word[es:di],ax
		add di,160

loop rope

mov di,930
mov ax,0x0720
mov cx,4

anchorloop:	mov word[es:di],ax
		add di,160

loop anchorloop


mov ax,0x301F
mov word[es:di],ax

sub di,160
mov si,di

mov ax,0x0720
mov cx,4
sub di,2
anchorloop2:	mov word[es:di],ax
		sub di,2

loop anchorloop2

sub di,158
mov ax,0x301E
mov word[es:di],ax

mov di,si
mov cx,4
add di,2
mov ax,0x0720
anchorloop3:	mov word[es:di],ax
		add di,2

loop anchorloop3

sub di,162
mov ax,0x301E
mov word[es:di],ax

mov di,1090
mov si,di
add di,2
mov ax,0x305F
mov cx,2

line1:	mov word[es:di],ax
	add di,2
loop line1

mov di,si
sub di,2

mov cx,2

line2:	mov word[es:di],ax
	sub di,2
loop line2


	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	ret

;--------------------------------------------------------------------

printchain:	push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di

mov ax,0xb800
mov es,ax
mov di,0
add di,158

mov cx,24

chainloop1:	mov ax,0x6720
		mov word[es:di],ax

		add di,160

loop chainloop1

mov cx,80
chainloop2:	mov ax,0x6720
		mov word[es:di],ax

		sub di,2

loop chainloop2

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	
	ret
;-----------------------------------------------------------------------------------------------

setintrobackground:	push es
		push ax
		push cx
		push di

		mov ax, 0xb800
		mov es, ax			; point es to video base
		mov di,0		;starting point
		mov ax,0x3720		;colour for sky
		mov cx,2000

	cld						;set auto-increment mode
	rep stosw				;mov word[es:di],ax  
							;add di,2
							;dec cx
							;cmp cx,0

		mov di,0		;starting point
		mov si,di
		mov ax,0x36B2
		mov bx,8
outermeshloop:	mov cx,24
		mov di,si
		add si,2
		
meshloop:	mov word[es:di],ax
		add di,160
		loop meshloop

		dec bx
		jnz outermeshloop
			
	call printchain
	call printanchor
	call printrope
			pop di
			pop cx
			pop ax
			pop es
			ret 

;------------------------------------------------------------------------------------------

printRectangle:	push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di

		mov ax, 0xb800
		mov es, ax 		; point es to video base

		mov al, 80 		; load al with columns per row
		mul byte [bp+12] 	; multiply with row number
		add ax, [bp+10] 	; add col
		shl ax, 1 		; turn into byte offset
		mov di, ax 		; point di to required location
		mov ah, [bp+4] 		; load attribute in ah
		mov cx, [bp+6]
		sub cx, [bp+10]

topLine:	mov al, 0x2D 		; ASCII of '-'
		mov [es:di], ax 	; show this char on screen
		add di, 2 		; move to next screen location 
		call delay
		loop topLine		; repeat the operation cx times

		mov cx, [bp+8]
		sub cx, [bp+12]
		add di, 160

rightLine:	mov al, 0x7c 		; ASCII of '|'
		mov [es:di], ax 	; show this char on screen
		add di, 160 		; move to next screen location 		
		call delay
		loop rightLine		; repeat the operation cx times
		
		mov cx, [bp+6]
		sub cx, [bp+10]
		sub di, 2

bottomLine:	mov al, 0x2D 		; ASCII of '-'
		mov [es:di], ax 	; show this char on screen
		sub di, 2 		; move to next screen location 
		call delay
		loop bottomLine		; repeat the operation cx times

		mov cx, [bp+8]
		sub cx, [bp+12]
		sub di, 160

leftLine:	mov al, 0x7c 		; ASCII of '|'
		mov [es:di], ax 	; show this char on screen
		sub di, 160 		; move to next screen location 		
		call delay
		loop leftLine		; repeat the operation cx times

		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10

;-------------------------------------------------------------------------------

loadscr:	push ax
			push bx
			push cx
			push dx
			push cx
			push si
			push di
			push es
			
		
		mov si,3

		mov ah, 0x13			; service 13 - print string
		mov al, 1			; subservice 01 – update cursor 
		mov bh, 0			; output on page 0
		mov bl,0x36			; normal attrib
		mov cx, 11			; length of string
		mov dx, 0x071E			; row 10 column 3
		push ds
		pop es				; es=ds segment of string
		mov bp, message_load1		; bp = offset of string
		INT 0x10			; call BIOS video service
	
		
				
pop es			
pop di
pop si
pop cx
pop dx
pop cx
pop bx
pop ax

ret			

;------------------------------------------------------------------------------------

PrintLoadscr:	push ax
			push bx
			push cx
			push dx
			push cx
			push si
			push di
			push es

call clrscr 		; call the clrscr subroutine
call loadscr_2
		
		mov ax, 2
		push ax 		; push top
		mov ax, 24
		push ax 		; push left

		mov ax, 10
		push ax 		; push bottom
		mov ax, 46
		push ax 		; push right number
		
		mov ax, 0x66 		; Red FG
		push ax 		; push attribute
		call printRectangle	; call the printstr subroutine

call loadscr

mov cx,20

waitLoop: call delay
          loop waitLoop

pop es			
pop di
pop si
pop cx
pop dx
pop cx
pop bx
pop ax

ret	

;------------------------------------------------------------------------------------------

introScreen:		push bp
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		
		call PrintLoadscr
		call clrscr
		call loadscr_2
		
		mov ah,0x02
		mov bh,0
		mov dx,0x0A28
		int 0x10

		mov ah, 0x13			; service 13 - print string
		mov al, 1			; subservice 01 – update cursor
		mov bh, 0			; output on page 0
		mov bl,0x36			; normal attrib
		mov cx, 16			; length of string
		mov dx, 0x0A0D			; row 10 column 3
		push ds
		pop es				; es=ds segment of string
		mov bp, message1		; bp = offset of string
		int 0x10			; call BIOS video service

		mov dx, buffer 			; input buffer (ds:dx pointing to input buffer)
		mov ah, 0x0A 			; DOS' service A – buffered input
		int 0x21 			; dos services call
	
		mov bh, 0
		mov bl, [buffer+1] 		; read actual size in bx i.e. no of characters user entered
		mov byte [buffer+2+bx], '$' 	; append $ at the end of user input

notquit:		mov byte[cs:flag_quit],0	
		call setintrobackground

		mov ah, 0x13			; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 

		mov bh, 0			; output on page 0
		
		mov bl,0x36			; normal attrib
		mov cx, 6			; length of string
		mov dx, 0x0713			; row 10 column 3
		
		push ds
		pop es				; es=ds segment of string
		mov bp, message2		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------	
		
		mov dx, buffer+2 		; user input buffer
		mov ah, 9 			; service 9 – write string
		int 0x21
	;------------------------------------------------------------------

		mov ah, 0x13			; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 

		mov bh, 0			; output on page 0
		
		mov bl,0x36			; normal attrib
		mov cx, 23			; length of string
		mov dx, 0x0913			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message3		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

		mov bl,0xE7			; normal attrib
		mov cx, 14			; length of string
		mov dx, 0x0C20			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message4		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

		mov bl,0x30			; normal attrib
		mov cx, 55			; length of string
		mov dx, 0x0D0B			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message5		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

		mov bl,0x30			; normal attrib
		mov cx, 67			; length of string
		mov dx, 0x0E0B			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message6		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

		mov bl,0xB0			; normal attrib
		mov cx, 6			; length of string
		mov dx, 0x1113			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message7		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

		mov bl,0xE7			; normal attrib
		mov cx, 7			; length of string
		mov dx, 0x1119			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message8		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;-----------------------------------------------------------------

		mov bl,0xB0			; normal attrib
		mov cx, 15			; length of string
		mov dx, 0x1120			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message9		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

		mov bl,0xE7			; normal attrib
		mov cx, 5			; length of string
		mov dx, 0x112F			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message10		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

		mov bl,0xB0			; normal attrib
		mov cx, 8			; length of string
		mov dx, 0x1134			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message11		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------
	
	mov bl,0x67			; normal attrib
		mov cx, 14			; length of string
		mov dx, 0x14C8			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message12		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------
	
	mov bl,0x30			; normal attrib
		mov cx, 24			; length of string
		mov dx, 0x14D7			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message13		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;------------------------------------------------------------------

	mov ah,0x02
	mov bh,0
	mov dx,0x1900
	int 0x10	
	
input:	mov ah,0x00
	int 0x16
	
	cmp ah,0x1
	jne EnterKey
	;------------------------------------------------------------------
	call warning_2
	;-------------------------------------------------------------------
	
	cmp byte[cs:flag4],1
	je notquit
	mov byte[cs:flag_quit],1
	jmp quitgame
	
EnterKey:	cmp ah,0x1C
	jne input
	mov byte[cs:flag_quit],0
	
quitgame:	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret	
		
;------------------------------------------------------------------------------------------

warning:	push bp
			push ax
			push bx
			push cx
			push dx
			push di
			push si	
			
		call loadscr_2
		mov ah, 0x13			; service 13 - print string	
		mov al, 1			; subservice 01 – update cursor 

		mov bh, 0			; output on page 0
		mov bl,0x34			; normal attrib
		mov cx, 21			; length of string
		mov dx,0x0613			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message14		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;-------------------------------------------------------------------
	
		mov bl,0x20			; normal attrib
		mov cx, 9			; length of string
		mov dx,0x0815			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message15		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;-------------------------------------------------------------------
	
		mov bl,0x40			; normal attrib
		mov cx, 8			; length of string
		mov dx,0x0821			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message16		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;-------------------------------------------------------------------		
			
	mov ah,0x02
	mov bh,0
	mov dx,0x1800
	int 0x10	
	;-------------------------------------------------------------------	
		
exit1:			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			pop bp
			ret
			
;------------------------------------------------------------------------------------------

warning_2:	push bp
			push ax
			push bx
			push cx
			push dx
			push di
			push si	
			
		call loadscr_2
		mov ah, 0x13			; service 13 - print string	
		mov al, 1			; subservice 01 – update cursor 

		mov bh, 0			; output on page 0
		mov bl,0x34			; normal attrib
		mov cx, 21			; length of string
		mov dx,0x0613			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message14		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;-------------------------------------------------------------------
	
		mov bl,0x20			; normal attrib
		mov cx, 9			; length of string
		mov dx,0x0815			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message15		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;-------------------------------------------------------------------
	
		mov bl,0x40			; normal attrib
		mov cx, 8			; length of string
		mov dx,0x0821			; row 10 column 3
			
		push ds
		pop es				; es=ds segment of string
		mov bp, message16		; bp = offset of string
		
		INT 0x10			; call BIOS video service
	;-------------------------------------------------------------------		
			
	mov ah,0x02
	mov bh,0
	mov dx,0x1800
	int 0x10	
	;-------------------------------------------------------------------		
			
input_again:	mov ah,0x00
	int 0x16
		
		cmp al,'y'
		jne check2
		mov byte[cs:flag4],0
		jmp exit_w
		
check2:		cmp al,'n'
			jne input_again
			mov byte[cs:flag4],1
		
exit_w:			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			pop bp
			ret


;----------------------------------------------------------------------------------------

loadscr_2: 	push es
		push ax
		push cx
		push di

call setintrobackground
;---------------------------

mov ax,3200
push ax

mov ax,3838
push ax

mov ax,0x1020
push ax

call printsea

;--------------------------
mov ax,24
push ax

mov ax,19
push ax

call printMountains

mov ax,44
push ax

mov ax,19
push ax

call printMountains

mov ax,64
push ax

mov ax,19
push ax

call printMountains
;-----------------------------

		mov ax,0x5720			
		push ax				; ............[bp+14]
		
		mov ax,7			
		push ax				; push number of windows............[bp+12]

		mov ax,16			
		push ax				; push length of ship............[bp+10]

		mov ax,2			
		push ax				; push length of pole............[bp+8]

		mov ax,22			
		push ax				; push row............[bp+6]

		mov ax,10
		push ax				; push column............[bp+4]

		call printship
		
pop di
pop cx
pop ax
pop es

ret		


				
;------------------------------------------------------------------------------------------
;* 										         										   *
;*									Bonus Part 		     								   *
;*                                                                                         *
;------------------------------------------------------------------------------------------

;-----------------------------------------------------------
printsky_i:	push bp
			mov bp,sp
			push ax
			push bx
			push cx
			push dx
			push si
			push di
			
mov ax, 0x0C0B 			; put pixel in white color
xor bx, bx 			; page number 0
mov si,0
mov cx,[bp+6] 			; column
mov dx,[bp+4]		; row

mov di,320

mov bx,61

outerloopi2:
li2: int 0x10 			; bios video services
inc cx 				; decrease y position
cmp cx,di
jne li2 			; decrease x position and repeat

add dx,1
mov cx,[bp+6]
dec bx
jnz outerloopi2

pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp

ret 4

;---------------------------------------------------------------------------

printsea_i: push bp
			mov bp,sp
			push ax
			push bx
			push cx
			push dx
			push si
			push di
			
			
				
mov ax, 0x0C01 			; put pixel in white color
xor bx, bx 			; page number 0
mov si,0
mov cx,[bp+6] 			; column
mov dx,[bp+4]		; row

mov di,320

mov bx,140

outerloopi_2:
li3: int 0x10 			; bios video services
inc cx 				; decrease y position
cmp cx,di
jne li3 			; decrease x position and repeat

add dx,1
mov cx,[bp+6]
dec bx
jnz outerloopi_2		
			
			
	pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp

ret 4		

;---------------------------------------------------------------------------
printMountains_i:	push bp
				mov bp,sp
				push ax
				push bx
				push cx
				push dx
				push si
				push di
				
mov ax, 0x0C06 			; put pixel in white color
xor bx, bx 			; page number 0
mov si,0
mov cx,[bp+6] 			; column
mov dx,[bp+4]		; row

mov di,dx
add di,40
mov bx,[bp+6]

outerloopi:	
li1: int 0x10 			; bios video services
inc cx 				; decrease y position
inc si
cmp si,di
jne li1 			; decrease x position and repeat

inc bx
mov cx,bx 
mov si,0
sub dx,1
sub di,2
cmp di,0
jne outerloopi

pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp

ret 4

;-------------------------------------------------------------------

printships_i: push bp
				mov bp,sp
				push ax
				push bx
				push cx
				push dx
				push si
				push di

mov ax, [bp+10] 			; put pixel in white color
xor bx, bx 			; page number 0
mov si,0
mov cx,[bp+6] 			; column
mov dx,[bp+4]		; row

mov di,[bp+8]

mov bx,15

outerloopi_3: push di
			push cx
li4: int 0x10 			; bios video services
inc cx 				; decrease y position
dec di
cmp di,0
jne li4 			; decrease x position and repeat

pop cx
add cx,1

add dx,1
pop di
sub di,2

dec bx
cmp bx,0
jne outerloopi_3

pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp

ret 8


;-------------------------------------------------------------------

print_pixelscr: push bp
			push ax
			push bx
			push cx
			push dx
			push di
			push si	

	mov ax, 0x000D 			; set 320x200 graphics mode
int 0x10 			; bios video services
 
 
 mov ax,0
 push ax		;------------[bp+6] column

 mov ax,0	
 push ax		;-------------[bp+4] row
 call printsky_i
 
	mov ax,0
		push ax		;------------[bp+6] column

		mov ax,60	
		push ax		;-------------[bp+4] row
		
		call printMountains_i
		
		mov ax,103
		push ax		;------------[bp+6] column

		mov ax,60	
		push ax		;-------------[bp+4] row
		
		call printMountains_i
		
		mov ax,206
		push ax		;------------[bp+6] column

		mov ax,60	
		push ax		;-------------[bp+4] row
		
		call printMountains_i
		
		
		
		mov ax,0
 push ax		;------------[bp+6] column

 mov ax,60	
 push ax		;-------------[bp+4] row
 call printsea_i
 
 
 mov ax,0x0C04
 push ax		;------------[bp+10] length
 
 mov ax,80
 push ax		;------------[bp+8] length
 
 mov ax,54
 push ax		;------------[bp+6] column

 mov ax,120	
 push ax		;-------------[bp+4] row
  call printships_i
  ;-------------------------------------------
  mov ax,0x0C02
 push ax		;------------[bp+10] length
 
 mov ax,100
 push ax		;------------[bp+8] length
 
 mov ax,180
 push ax		;------------[bp+6] column

 mov ax,150	
 push ax		;-------------[bp+4] row
  call printships_i
   ;-------------------------------------------
  mov ax,0x0C0D
 push ax		;------------[bp+10] length
 
 mov ax,60
 push ax		;------------[bp+8] length
 
 mov ax,220
 push ax		;------------[bp+6] column

 mov ax,100	
 push ax		;-------------[bp+4] row
  call printships_i

mov ah, 0x13			; service 13 - print string
		
		mov al, 1			; subservice 01 – update cursor 

		mov bh, 0			; output on page 0
		
		mov bl,0x3D			; normal attrib
		mov cx, 11			; length of string
		mov dx, 0x0A0C			; row 10 column 3
		
		push ds
		pop es				; es=ds segment of string
		mov bp, message_i		; bp = offset of string
		
		INT 0x10			; call BIOS video service
		
		
mov ah, 0 			; service 0 – get keystroke
int 0x16
					;bios keyboard services
mov ax, 0x0003 			; 80x25 text mode
int 0x10 			; bios video services


pop si
pop di
pop dx
pop cx
pop bx
pop ax
pop bp

ret

;---------------------------------------------------------------------------------------


;----------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------

start: call print_pixelscr

	call introScreen
	
	cmp byte[cs:flag_quit],1
	je quit
	
	call initialize
  	call printfish
	 
notend:	call FirstPrint
	xor ax,ax
	mov es,ax

	mov ax, [es:9*4]
	mov [oldisr], ax 			; save offset of old routine
	mov ax, [es:9*4+2]
	mov [oldisr+2], ax 			; save segment of old routine
	
	mov ax, [es:8*4]
	mov [oldTisr], ax 			; save offset of old routine
	mov ax, [es:8*4+2]
	mov [oldTisr+2], ax 			; save segment of old routine
	
	cli
	mov word[es:9*4],kbisr
	mov word[es:9*4+2],cs
	mov word[es:8*4],timer
	mov word[es:8*4+2],cs
	sti	

a1:		cmp byte[cs:flagtimer],0
		jne a1
		
		cmp byte[cs:flag3],0
		jne a1
		
		
unhook:		mov ax,[oldTisr]				;unhooking kbisr
		mov bx,[oldTisr+2]
		cli
		mov [es:8*4],ax
		mov [es:8*4+2],bx
		sti
			
		mov ax,[oldisr]				;unhooking kbisr
		mov bx,[oldisr+2]
		cli
		mov [es:9*4],ax
		mov [es:9*4+2],bx
		sti
		
quit: mov ax,0x4c00
int 0x21