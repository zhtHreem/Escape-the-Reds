; hello world printing with length calculation subroutine
[org 0x0100]
jmp start

oldkb: dd 0
oldisr: dd 0
startcon: dw 1
upcolumn: dw 0
downcolumn: dw 0
rightrow: dw 0
rr: dw 0
ll: dw 0
uplr: dw 0                    ; which key is pressed after up
leftrow: dw 0
tickcount: dw 0
tempstar: dw 0                                                         ; prev value/location of star
timerflag: dw 0
rowNum: dw 0
message: db '*'
sec: dw 0
star: dw 0
Rcol: dw 24
Brow: dw 0
Lcol: dw 25
flag: dw 0

green: push es
push ax
push cx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
xor di, di ; point di to top left column
mov ax, 0x2220 ; space char in normal attribute
mov cx, 2000 ; number of screen locations
cld ; auto increment mode
rep stosw ; clear the whole screen
pop di
pop cx
pop ax
pop es
ret


clrscr:		push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax					                               ; point es to video base
			mov di, 0					                               ; point di to top left column

nextloc:	mov word [es:di], 0x0720	                               ; clear next char on screen
			add di, 2					                               ; move to next screen location
			cmp di, 4000				                               ; has the whole screen cleared
			jne nextloc					                               ; if no clear next position

			pop di
			pop ax
			pop es
			ret

;------------------------------------------------------
; subroutine to print a number at top left of screen
; takes the number to be printed as its parameter
;------------------------------------------------------
printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax                                                              ; point es to video base
mov ax, [bp+4]                                                          ; load number in ax
mov bx, 10                                                              ; use base 10 for division
mov cx, 0                                                               ; initialize count of digits
nextdigit: mov dx, 0                                                    ; zero upper half of dividend
div bx                                                                  ; divide by 10
add dl, 0x30                                                            ; convert digit into ascii value
push dx                                                                 ; save ascii value on stack
inc cx                                                                  ; increment count of values
cmp ax, 0                                                               ; is the quotient zero
jnz nextdigit                                                           ; if no divide it again
mov di, 140                                                             ; point di to 70th column
nextpos: pop dx                                                         ; remove a digit from the stack
mov dh, 0x07                                                            ; use normal attribute
mov [es:di], dx                                                         ; print char on screen
add di, 2                                                               ; move to next screen location
loop nextpos                                                            ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax 
pop es
pop bp
ret 2

			
printstar:
         push bp
		 mov bp,sp
		 push es
		 push ax
		 push di
		 push bx

         mov ax,0xb800
		 mov es,ax
		 mov ds,ax
		 mov bx,[bp+4]                                             ;location
		 mov di,bx
		 mov si,di
		 mov ax, 0x0720
		 cmp word[es:di],4C20h                                     ;red screen
		 je terminatered
		 

green1:	    cmp word[ds:si],0x2220
		   jne  notgreenScr
           inc word [cs:tickcount]                                ; increment tick count
notgreenScr: mov al,message
		 mov ah,1

		 mov [es:di],ax

		 pop bx
		 pop di
		 pop ax
		 pop es
		 pop bp
		 ret 2

terminatered:
mov al,0x20
out 0x20,al
cli
mov ax, 0
mov es, ax
mov ax, [oldisr]
mov bx, [oldisr+2]
mov word [es:8*4], ax
mov word [es:8*4+2], bx
sti
cli
mov ax, 0
mov es, ax
mov ax, [oldkb]
mov bx, [oldkb+2]
mov word [es:9*4], ax
mov word [es:9*4+2], bx
sti
 mov ax, 0x4c00
 int 21h


PrevStar:
         push bp
		 mov bp,sp
		 push es
		 push ax
		 push di
		 push bx

         mov ax,0xb800
		 mov es,ax
		 mov bx,[bp+4]         ;location
		 mov di,bx

		 mov ax, 0x0720
		 cmp	byte[cs:leftrow],1 
		 je l
			
		cmp	byte[cs:rightrow],1 
		je r
			  
		 cmp byte[cs:upcolumn],1 
		je u
				

		cmp	byte[cs:downcolumn],1
		je d
			 
		; sub di,2  ; will make prev byte/word black

l:   
 ;cmp byte[cs:ll],1
 ; jne lp
 ; sub di, 160
 ;mov [es:di],ax
 ; mov byte[cs:ll],0
 ; lp:
  add di,2 
  jmp prscr
r:   
;  cmp byte[cs:rr],1               ;   neechy jatay hue right click
;  jne rp
;  sub di, 160                       ; uper k liay add 160
;  mov [es:di],ax
;  mov byte[cs:rr],0
;  rp:
  sub di,2
  jmp prscr

u:   add di,160 
  jmp prscr

d:   sub di,160 

prscr:
		 
		 mov [es:di],ax

		 pop bx
		 pop di
		 pop ax
		 pop es
		 pop bp
		 ret 2


kbisr:
         push ax
		 in al,0x60


		  mov bx, word[cs:star]
	      mov word[cs:tempstar],bx                                 ;stores prev value/loc of asterik in tempstar
		  mov cx,0
		  mov bx,0


		 cmp al, 0x48                                                                          ;Up key scancode
		 je upKey
	 

		 cmp al, 0x50                                                                         ;Down key scancode
		 je downKey
  


		 cmp al, 0x4B                                                                          ;Left key scancode
		 je leftKey
	

 		 cmp al, 0x4D                                                                          ;Right key scancode
		 je rightKey
	

		 jne nomatch

  upKey: 
	     
	         cmp word[cs:rowNum],0
		     jne moveup
		     add word[cs:star],2000                                   ;when row is zero and user press up-key it will move to last row
		     mov word[cs:rowNum],26                                   ;row  will be 25  on next instruction
		 ;    mov byte[cs:upcolumn],1  
		 
	  moveup:	
	         mov bh,1
	         dec word[cs:rowNum]
		     sub word[cs:star],80                                     ; Moves to upper row  ;80 or 81?
		     jmp exitK

  downKey:
	    
		     cmp word[cs:rowNum],25
		     jne movedown
		     sub word[cs:star],2000                                   ;when row is zero and user press down-key it will move to 1st row	
		     mov word[cs:rowNum],-1                                   ;row  will be inc to 0 on next instruction
		  ;   mov byte[cs:downcolumn],1  
		  
      movedown:
	         mov bl,1
             inc word[cs:rowNum]
		     add word[cs:star],80                                     ;Moves to Lower row  	
	         jmp exitK

 leftKey:     
	  
	         dec word[cs:star]                               ; prev column [the asterik alreadmy moved toward next row that is why there is space]
		  ;   mov byte[cs:leftrow],1  
		     mov ch,1
			 jmp exitK

 rightKey:
	        mov cl,1                                          ; next column [the asterik alreadmy moved toward next row that is why there is space]
	        inc word[cs:star]  
	     ;   mov byte[cs:rightrow],1  
		    jmp exitK
			

 nomatch: 
            pop ax
            jmp far [cs:oldkb]                                 ; call original ISR
 exitK:
           mov byte[cs:upcolumn],bh                           ; it will pass 1 to row/col according to their key 
		   mov byte[cs:downcolumn],bl                         ; While 0 will be passed to others 
		    mov byte[cs:leftrow],ch                   
	       mov byte[cs:rightrow],cl                    


		  mov ax,[cs:tempstar]
			shl ax,1
	;		push ax
	;		call PrevStar                                       ;REMOVE PREV location "prints blank screen"

		 

	;	   cmp cl,1
	;      je rightc
	;	   cmp ch,1 
	;	   je leftc
	;	   jmp e
 ;     rightc:
 ;	      mov byte[cs:rr],1
 ;		  jmp e

 ;    leftc:
    ;	  mov byte[cs:ll],1
    ;     jmp e


    e:
           mov word [cs:timerflag], 1                                  ; set flag to start printing
           mov al, 0x20
		   out 0x20,al
		   pop ax
		   iret


timer:		
			push bp
			mov bp,sp
			push ax
            push es
			push dx

			cmp word [cs:timerflag], 1                             ; is the printing flag set
                   
			mov bx, word[cs:star]
	       mov word[cs:tempstar],bx                                 ;stores prev value/loc of asterik in tempstar	   

			mov ax, word [cs:tickcount]                                ; increment green count
			push ax
			call printnum  
			
			; print tick count
			inc word[cs:sec]

			

			cmp word[cs:sec],18                                    ;Bcz timer interrupt comes 18 time per sec
			jne exit1
			sub word[cs:sec],18
		                                                 

			mov ax,[cs:star]
			shl ax,1
			push ax
			call printstar


			mov ax,[cs:tempstar]
			shl ax,1
			push ax
			call PrevStar                                       ;REMOVE PREV location "prints blank screen"

	;		cmp byte[cs:startcon],1
	;		je rightkey
      
		cmp	byte[cs:leftrow],1 
		je leftkey

		cmp	byte[cs:rightrow],1 
		je rightkey

		 cmp byte[cs:upcolumn],1 
		je upkey
			

		cmp	byte[cs:downcolumn],1
		je downkey
			
exit1:
      jmp exit


 upkey: 
          ;   sub word[cs:star],80                                    ; Moves to upper row  ;80 or 81?          
	         cmp word[cs:rowNum],0
             jne moveup1
		     add word[cs:star],2000                                   ;when row is zero and user press up-key it will move to last row
		     mov word[cs:rowNum],26                                   ;row  will be 25  on next instruction
		     mov byte[cs:upcolumn],1  
	  moveup1:	   
	         dec word[cs:rowNum]
		    sub word[cs:star],80                                     ; Moves to upper row  ;80 or 81?
		     jmp exit

  downkey:
		     cmp word[cs:rowNum],25
		     jne movedown1
		     sub word[cs:star],2000                                   ;when row is zero and user press down-key it will move to 1st row	
		     mov word[cs:rowNum],-1                                   ;row  will be inc to 0 on next instruction
		     mov byte[cs:downcolumn],1  
      movedown1:
             inc word[cs:rowNum]
		    add word[cs:star],80                                      ;Moves to Lower row  	
	         jmp exit

 leftkey:     
	         dec word[cs:star]                                        ; prev column 
             jmp exit
 rightkey:
        ;    mov word[cs:startcon],0
	        inc word[cs:star]  
			cmp word[cs:Brow],79
			je l2
			inc word[cs:Brow]	  
			jmp exit

			l2:
            mov word[cs:Brow],0
			sub word[cs:star],79
            jmp exit

exit:
			mov al, 0x20
			out 0x20, al ; end of interrupt
			pop dx
			pop bp
			pop es
			pop ax
			iret ; return from interrupt

start: 
call green




mov ax, 0xb800
mov es, ax
mov di,360
mov ax, 0x4C20
mov cx, 40
cld
rep stosw

mov di,1260
mov ax, 0x4C20
mov cx, 40
cld
rep stosw

mov di,2160
mov ax, 0x4C20
mov cx, 40
cld
rep stosw


mov di,3160
mov ax, 0x4C20
mov cx, 20
cld
rep stosw



cli
 xor     ax, ax
 mov     es, ax
mov ax, [es:8*4]
mov [oldisr], ax                                                                  ; save offset of old routine
mov ax, [es:8*4+2]
mov [oldisr+2], ax
 mov     word [es: 8*4], timer
 mov     [es: 8*4+2], cs
 sti
                                                                                   ;hook keyboard interrupt
 cli                                                                               ; disable interrupts
 xor ax, ax
mov es, ax                                                                         ; point es to IVT base
mov ax, [es:9*4]
mov [oldkb], ax                                                                    ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldkb+2], ax                                                                  ; save segment of old routine
mov word [es:9*4], kbisr                                                           ; store offset at n*4
mov [es:9*4+2], cs                                                                 ; store segment at n*4+2
sti
labelrun:
jmp labelrun



;mov ax, 0x4c00                                                                    ; terminate program
;int 0x21


; red color:  4c20h
;green color : 0x2220