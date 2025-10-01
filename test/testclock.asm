 ;Define constants for LCD commands
entrymode     EQU    00000110B
displayon     EQU    00001100B
displayof     EQU    00001000B
cursoroff     EQU    00001100B
cursoronb     EQU    00001111B
cursoronn     EQU    00001110B
functions     EQU    00111000B
cgram         EQU    01000000B
ddram         EQU    10000000B

; Define LCD and ADC ports
lcdport       EQU    p3_data
RS            bit    p4_data.5
e             bit    p4_data.6

; Data storage
RAM_ADDR            EQU 30h
battery_percentage  EQU RAM_ADDR
loopl equ 020h ; used as a memory running light
div1 equ 021h ;auxiliary registers introut
div2 equ 022h ; ditto
counter equ 023h ;register to store counter
hh equ 024h ;save hours
mm equ 025h ;save minutes
ss equ 026h ;save seconds

             org 0000h
            mov     sp,#7fh
          
            lcall initlcd
            lcall initp4
            lcall INIT
            
        
    mov ss,#09h
     mov hh,#09h
      mov mm,#Dh
loop:
             mov a,#98h
            lcall COMNWRT

           
            mov a,hh
            lcall hexbcd8
            lcall lcdoutbyte
            


           mov a,#':'
           lcall DATAWRT
           

          
            mov a,mm
            lcall hexbcd8
            lcall lcdoutbyte

             mov a,#':'
           lcall DATAWRT

             
            mov a,ss
            lcall hexbcd8
            lcall lcdoutbyte
            
		inc	SS		;start with adjusting the seconds
		mov	a,SS		;test for 60 (contents of seconds are hex)
		cjne	a,#60,continue	

		mov	SS,#00h		;reset value to 0
        inc MM

        mov	a,MM		;test for 60 (contents of seconds are hex)
		cjne	a,#60,continue	;If value not equal to 60 skip adjusting minutes and hours
					
		mov	MM,#00h		;reset value to 0
        inc HH

;
        mov	a,HH		
		cjne	a,#24,continue	

		mov	HH,#00h		;reset value to 0
continue:	;use 1 second delay
        mov a,#20
        lcall delaya0k05s


ljmp loop
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialization routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INIT:	
    lcall delay10us
    mov A,#functions
    lcall COMNWRT
    lcall delay2ms  
    mov A,#cursoroff
    lcall COMNWRT 
    lcall delay2ms  
    mov A,#displayon
    lcall COMNWRT 
    lcall delay2ms  
    mov A,#entrymode
    lcall COMNWRT 
    lcall delay2ms  
    mov A,#80H 
    lcall COMNWRT
    ret

COMNWRT: ; Send command to LCD
    CLR RS ; RS=0 for command
    SETB e ; E=1 for high pulse
    MOV lcdport,A ; Copy reg A to port
    lcall DELAY
    CLR e ; E=0 for H-to-L pulse
    ret

DATAWRT: ; Write data to LCD
    SETB RS ; RS=1 for data
    SETB e ; E=1 for high pulse
    MOV lcdport,A ; Copy reg A to port
    lcall DELAY
    CLR e
    ret
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LCD output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lcdoutmsga:

outmsgalcd1:
    clr a
    movc a, @a+dptr
    jz outmsgalcde
    lcall DATAWRT
    inc dptr
    sjmp outmsgalcd1
outmsgalcde:
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; outniblcd will convert the low nibble into an ascii code and print it
; on the LCD screen.
; the routine does not use registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lcdoutnib:
outniblcd: 
    push acc
    push psw
    anl a,#00fh ;leave the lowest 4 bits
    add a,#030h ;convert to ascii codes 0-9
    cjne a,#03ah,outniblcd1 ;if larger than this, add another 7
outniblcd1: 
    jc outniblcde ;if carry then conversion done
    add a,#007h ;convert to uppercase
outniblcde: 
    lcall DATAWRT ;to the screen
    pop psw
    pop acc
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lcdoutbyte will print the contents of the battery on the LCD screen.
; the routine does not use any registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lcdoutbyte:
outbytelcd: 
    push acc
    push psw
    swap a ;exchange bits
    lcall outniblcd
    swap a ;again
    lcall outniblcd
    pop psw
    pop acc
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Delay routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

delaya0k05s: 
    push acc
    push psw
    push b
delaya0k05s2: 
    mov b,#050
delaya0k05s1: 
    lcall delay1ms
    djnz b,delaya0k05s1
    djnz acc,delaya0k05s2
    pop b
    pop psw
    pop acc
    ret

delay10us:    
    push acc
    push psw
    mov acc,#01ah
    djnz acc,$
    pop psw
    pop acc
    ret

delay2ms:     
    lcall delay1ms
    lcall delay1ms 
    ret

delay1ms:     
    push acc
    push psw
    mov acc,#100
delay1ms1:    
    lcall delay10us
    djnz acc,delay1ms1
    pop psw
    pop acc
    ret

delay50us:
    push acc
    push psw
    mov acc, #11h
delay_loop:
    djnz acc, delay_loop
    pop psw
    pop acc
    ret

;-----------delay subroutine 12.75 milliseconds--------------------	
DELAY: 	MOV R3,#50
DEL2: 	MOV R4,#255
DEL: 	DJNZ R4,DEL
	DJNZ R3,DEL2
	RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialization of LCD and Ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

initlcd:
    push syscon0
    mov syscon0, #004h
    push port_page
    mov port_page, #000h
    orl p3_dir, #0ffh
    pop port_page
    pop syscon0
    ret

initp4:
    push syscon0
    mov syscon0, #004h
    push port_page
    mov port_page, #000h
    mov p4_dir, #0ffh
    pop port_page
    pop syscon0
    ret
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; hexbcd32    zet een 32 bit hex getal om in een 32 bit bcd getal (maximale bcd waarde 99999999)
; input       r3,r2,r1,r0 = 32 bit hex getal (r3=msb)
; output      r3,r2,r1,r0 = 32 bit bcd getal (r3=msb)
;
; de routine gebruikt alleen r3,r2,r1,r0 van bank 0 (moet geselecteerd zijn)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

hexbcd32:	push	acc			;werkregisters op de stack
		push	psw
		push	07h
		push	06h
		push	05h
		push	04h
		mov	r7,#00h		;delen door 10000000
		mov	r6,#98h
		mov	r5,#96h
		mov	r4,#80h
		lcall	div32			;rest kan verder gedeeld worden, q hebben we nodig (4 geldige bits)
		mov	a,r4
		swap	a			;bits klaar voor resultaat
		mov	r7,#00h		;delen door 1000000
		mov	r6,#0fh
		mov	r5,#42h
		mov	r4,#40h
		lcall	div32			;rest kan verder gedeeld worden, q hebben we nodig (4 geldige bits)
		orl	a,r4			;hoge byte staat al klaar, effe bewaren op stack
		push	acc
		mov	r7,#00h		;delen door 100000
		mov	r6,#01h
		mov	r5,#86h
		mov	r4,#a0h
		lcall	div32			;rest kan verder gedeeld worden, q hebben we nodig (4 geldige bits)
		mov	a,r4
		swap	a			;bits klaar voor resultaat
		mov	r7,#00h		;delen door 10000
		mov	r6,#00h
		mov	r5,#27h
		mov	r4,#10h
		lcall	div32			;rest kan verder gedeeld worden, q hebben we nodig (4 geldige bits)
		orl	a,r4			;hoge byte staat al klaar, effe bewaren op stack
		push	acc 		
		mov	r7,#00h		;delen door 1000
		mov	r6,#00h
		mov	r5,#03h
		mov	r4,#e8h
		lcall	div32			;rest kan verder gedeeld worden, q hebben we nodig (4 geldige bits)
		mov	a,r4
		swap	a			;bits klaar voor resultaat
		mov	r7,#00h		;delen door 100
		mov	r6,#00h
		mov	r5,#00h
		mov	r4,#64h
		lcall	div32			;rest kan verder gedeeld worden, q hebben we nodig (4 geldige bits)
		orl	a,r4			;hoge byte staat al klaar, effe bewaren op stack
		push	acc 		
		mov	r7,#00h		;delen door 10
		mov	r6,#00h
		mov	r5,#00h
		mov	r4,#0ah
		lcall	div32			;rest kan verder gedeeld worden, q hebben we nodig (4 geldige bits)
		mov	a,r4
		swap	a			;bits klaar voor resultaat
		orl	a,r0			;hoge byte staat al klaar, effe bewaren op stack
		mov	r0,a      		;resultaten van stack naar juiste registers
		pop	acc
		mov	r1,a
		pop	acc
		mov	r2,a
		pop	acc
		mov	r3,a
		pop	04h
		pop	05h
		pop	06h
		pop	07h
		pop	psw
		pop	acc
		ret


#include "c:\xcez1.inc"










