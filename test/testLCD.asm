
       ;Program for LCD intefacing to 8051 Microcontroller
RS      bit     p4_data.5    ; Register select pin
e      bit      p4_data.6    ; Enable pin

entrymode     EQU    00000110B            ;CURSOR NAAR RECHTS, SCHERM VAST
displayon     EQU    00000001B            ;HOMEN EN CLEAR CURSOR
displayof     EQU    00001000B            ;DISPLAY OFF, CURSOR OFF
cursoroff     EQU    00001100B            ;DISPLAY ON ZONDER CURSOR
cursoronb     EQU    00001111B            ;CURSOR ON AND BLINK
cursoronn     EQU    00001110B            ;CURSOR ON NO BLINK
functions     EQU    00111000B            ;INTERFACE LENGTE EN KARAKTER FONT
cgram         EQU    01000000B            ;SELECTIE KARAKTER GENERATOR RAM
ddram         EQU    10000000B            ;SELECTIE DATA DISPLAY RAM

lcdport       EQU    p3_data                   


            org 0000h
            mov     sp,#7fh
          
            lcall initlcd
             lcall initp4
            lcall INIT
          
            
            
         
            loop:
             mov a,#80h
             lcall COMNWRT

            mov a,#'a'            
            lcall DATAWRT 
              mov a,#'a'            
            lcall DATAWRT  
            mov a,#'a'            
            lcall DATAWRT
               mov a,#'a'            
            lcall DATAWRT 
             cont:
            
       

        mov a,#1
        lcall delaya0k05s


        


               ljmp loop





initp4:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p4_dir,#0ffh         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              
            ret

;----------------------------------------------
INIT:	
   
    lcall DELAY_40MS

    MOV A,#functions       ; initialize LCD
	ACALL COMNWRT
    lcall delay2ms  
   
	MOV A,#cursoronb        ;display on, cursor on
	ACALL COMNWRT 
    lcall delay2ms  

	MOV A,#displayon          ;clear LCD
	ACALL COMNWRT 
	lcall delay2ms  
 
	MOV A,#entrymode         ;Entry mode
    ACALL COMNWRT 
	lcall delay2ms  
    
	MOV A,#80H 
	ACALL COMNWRT
	RET

COMNWRT: 		;send command to LCD
	CLR RS 		;RS=0 for command

	SETB e 	;E=1 for high pulse
	MOV lcdport,A 	;copy reg A to port 1
	lcall DELAY
	CLR e 		;E=0 for H-to-L pulse
	RET

DATAWRT: 		;write data to LCD

	SETB RS 	;RS=1 for data
	SETB e 	;E=1 for high pulse
    MOV lcdport,A 	;copy reg A to port 1
    lcall DELAY
	CLR e
	RET

;-----------delay subroutine 12.75 milliseconds--------------------	
DELAY: 	MOV R3,#50
DEL2: 	MOV R4,#255
DEL: 	DJNZ R4,DEL
	DJNZ R3,DEL2
	RET






initp2:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p2_dir,#0ffh         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              ret
initlcd:
               push syscon0                  ;set folder correctly
               mov syscon0,#004h                 ;select mainddir
               push port_page                          ;save port page
               mov port_page,#000h                            ;select home page
               orl p3_dir,#0ffh                          ;set port pin as output
               pop    port_page            ;pagina herstellen
               pop    syscon0              ;map herstellen
               ret
initp0:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p0_dir,#0ffh         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              ret


  initp1:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p1_dir,#00h         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              ret             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

delaya0k05s:  push   acc                  ;registers bewaren
              push   psw
              push   b
delaya0k05s2: mov    b,#050               ;loopcounter
delaya0k05s1: lcall  delay1ms
              djnz   b,delaya0k05s1
              djnz   acc,delaya0k05s2
              pop    b                    ;registers herstellen
              pop    psw
              pop    acc
              ret

delay10us:    push   acc                  ;8*42ns=330ns
              push   psw                  ;8*42ns=330ns
              mov    acc,#01ah            ;420ns
              djnz   acc,$                ;420ns*aantal keer doorlopen
              pop    psw                  ;8*42ns=330ns
              pop    acc                  ;8*42ns=330ns
              ret   
              ;165ns
delay2ms:     lcall  delay1ms
              lcall  delay1ms
              ret

delay1ms:     push   acc                  ;8*42ns=330ns
              push   psw                  ;8*42ns=330ns
              mov    acc,#100             ;420ns
delay1ms1:    lcall  delay10us            ;10us
              djnz   acc,delay1ms1        ;420ns*aantal keer doorlopen
              pop    psw                  ;8*42ns=330ns
              pop    acc                  ;8*42ns=330ns
              ret                         ;165ns
delay50us:
    push   acc                  ; 8 cycles * 42 ns = 336 ns
    push   psw                  ; 8 cycles * 42 ns = 336 ns
    mov    acc, #11h            ; 420 ns (adjust for 50 us delay)
delay_loop:
    djnz   acc, delay_loop      ; 420 ns * aantal keer doorlopen
    pop    psw                  ; 8 cycles * 42 ns = 336 ns
    pop    acc                  ; 8 cycles * 42 ns = 336 ns
    ret                          ; Return from subroutine

DELAY_40MS:
    MOV R3, #39          ; Set outer loop count to achieve approximately 40 ms delay
OUTER_LOOP:
    MOV R4, #255         ; Set inner loop count
INNER_LOOP:
    NOP                  ; No operation (adjust as needed for your processor)
    NOP
    NOP
    NOP
    DJNZ R4, INNER_LOOP  ; Decrement inner loop count and repeat if not zero
    DJNZ R3, OUTER_LOOP  ; Decrement outer loop count and repeat if not zero
    RET                  ; Return from subroutine
    
    delay_300ns:
    NOP          ; Adjust the number of NOP instructions as needed
    NOP
    NOP
    NOP
    NOP
    NOP          ; 10 NOP instructions, assuming each takes around 30 ns on average
    NOP
    NOP
    NOP
    NOP
    NOP          ; Total delay: 10 NOP instructions * 30 ns = 300 ns
    RET
                lcdoutmsga:
                outmsgalcd: 
               
                outmsgalcd1:
                                clr a                                                     ;value is in our way
                               movc a,@a+dptr                           ;read value
                               jz outmsgalcde                             ;if 0 then jump to end
                               lcall DATAWRT                             ;get rid of it
                               inc dptr                                            ;point to next ascii code
                               ljmp outmsgalcd1                     ;close loop
                
                outmsgalcde:
                               ret
                
                
#include	"c:\xcez1.inc"