;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Written by VoltVersa (VV) ;;;;;
;;;;;;;;  May 2023 ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; include file xcez1 by Mr.Roggemans M. (MGM) ;;;;;;

; Define constants for LCD commands
entrymode     EQU    00000110B
displayon     EQU    00001100B
displayof     EQU    00001000B
cursoroff     EQU    00001100B
cursoronb     EQU    00001111B
cursoronn     EQU    00001110B
functions     EQU    00111000B
cgram         EQU    01000000B
ddram         EQU    10000000B
cleardisplay  EQU    00000001B

; Define LCD and ADC ports
lcdport       EQU    p3_data
RS            bit    p4_data.5
e             bit    p4_data.6

; Data storage
RAM_ADDR            EQU 30h
battery_percentage  EQU RAM_ADDR
loopl              EQU 020h ; Used as a memory running light
div1               EQU 021h ; Auxiliary registers
div2               EQU 022h ; Ditto
counter            EQU 023h ; Register to store counter
hh                 EQU 024h ; Save hours
mm                 EQU 025h ; Save minutes
ss                 EQU 026h ; Save seconds
LCD_CMD_ADDRESS    EQU 0FCh ; Replace with actual LCD command address
LCD_DATA_ADDRESS   EQU 0FDh ; Replace with actual LCD data address
cleardisp          EQU    001H ; Clear display command
cursathom          EQU    002H ; Cursor home command

KBD_DATA  bit P1_data.4
KBD_CLK   bit P1_data.3     

ascii     EQU 10h
TEMPVAR   EQU 11h
count     EQU 12h

; Start of code
org 0000h
    ljmp start1

org 000Bh
    ljmp TIMER0_ISR  ; Timer 0 interrupt vector

start1:
    mov sp,#7fh
    lcall initlcd
    lcall initp4
    lcall INIT
    lcall initadc_battery
    lcall init_timer0
    mov a,#cleardisplay
    lcall COMNWRT
    lcall DELAY

    mov a,#c6h
    lcall COMNWRT
    mov dptr,#welcome
    lcall lcdoutmsga
    mov a,#55
    lcall delaya0k05s

    mov a,#cleardisplay
    lcall COMNWRT
    lcall DELAY

start:              
    mov a,#88h
    lcall COMNWRT
    mov dptr,#chat
    lcall lcdoutmsga

    mov a,#c8h
    lcall COMNWRT
    mov dptr,#batt
    lcall lcdoutmsga

    mov a,#9ch
    lcall COMNWRT
    mov dptr,#clock
    lcall lcdoutmsga

    mov a,#d4h
    lcall COMNWRT
    mov dptr,#choose
    lcall lcdoutmsga

    lcall READ_KEY
    lJMP start

TIMER0_ISR:
    clr tf0             ; Clear Timer 0 overflow flag
    lcall CHECK_ESC_KEY ; Check for ESC key
    reti                ; Return from interrupt

CHECK_ESC_KEY:
    clr c
    jb KBD_CLK, no_key_check
    mov r2, #08h        ; 8 bits per byte
    mov a, #00h
read_loop:
    jnb KBD_CLK, read_loop  ; Wait for KBD_CLK to go low
    mov c, KBD_DATA         ; Read data bit
    rlc a                   ; Rotate data bit into accumulator
    jb KBD_CLK, $           ; Wait for KBD_CLK to go high
    jnb KBD_CLK, $          ; Wait for KBD_CLK to go low
    djnz r2, read_loop

    cjne a, #01101110b, no_key_check
    mov a, #cleardisplay
    lcall COMNWRT
    lcall DELAY
    ljmp start

no_key_check:
    ret

init_timer0:
    mov tmod, #01h      ; Timer 0 mode 1 (16-bit timer)
    ; For 1/4 second (250 ms), the required count is 65536 - 250000 = -194464 (0xF060)
    mov th0, #0F0h      ; Load high byte
    mov tl0, #060h      ; Load low byte
    setb et0            ; Enable Timer 0 interrupt
    setb ea             ; Enable global interrupt
    setb tr0            ; Start Timer 0
    ret

READ_KEY:
    MOV TEMPVAR,#03H
READ_KEY_AGAIN:
    MOV A,#00H
    CLR C
    JB KBD_CLK, $                                 
    MOV R2, #08h    ; 8 bits per byte
    JB KBD_CLK,$
    JNB KBD_CLK,$
    JB KBD_CLK,$
    JNB KBD_CLK,$
ReadByteLoop:
    MOV C, KBD_DATA; Read data bit
    RLC A               ; Rotate data bit into accumulator
    JB KBD_CLK,$
    JNB KBD_CLK,$
    DJNZ R2, ReadByteLoop   ; Loop until all bits are read
    JNB KBD_CLK,$
    JB KBD_CLK,$
    JNB KBD_CLK,$
    DJNZ TEMPVAR,READ_KEY_AGAIN

asciiconv:
PRESS_11:
    CJNE A,#'h',PRESS_22
    mov a,#cleardisplay
    lcall COMNWRT
    LCALL DELAY
    mov a,#80h
    lcall COMNWRT
    ljmp start_read_3

PRESS_22:
    CJNE A,#'x',PRESS_33
    mov a,#cleardisplay
    lcall COMNWRT
    LCALL DELAY
    ljmp batt_loop

PRESS_33:
    CJNE A,#'d', EXIT                    
    ljmp loop_clock

EXIT:
    ret

loop_clock:
    mov ss,#09h
    mov hh,#09h
    mov mm,#Dh
    mov a,#cleardisplay
    lcall COMNWRT
    lcall DELAY
loop:
    mov a,#c3h
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

    inc ss      ; Start with adjusting the seconds
    mov a,ss    ; Test for 60 (contents of seconds are hex)
    cjne a,#60,continue

    mov ss,#00h ; Reset value to 0
    inc mm

    mov a,mm    ; Test for 60 (contents of minutes are hex)
    cjne a,#60,continue

    mov mm,#00h ; Reset value to 0
    inc hh

    mov a,hh
    cjne a,#24,continue

    mov hh,#00h ; Reset value to 0
continue:
    mov a,#2
    lcall delaya0k05s

    mov a,#Dbh
    lcall COMNWRT

    mov dptr,#back
    lcall lcdoutmsga

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    anl a,#00fh ; Leave the lowest 4 bits
    add a,#030h ; Convert to ascii codes 0-9
    cjne a,#03ah,outniblcd1 ; If larger than this, add another 7
outniblcd1: 
    jc outniblcde ; If carry then conversion done
    add a,#007h ; Convert to uppercase
outniblcde: 
    lcall DATAWRT ; To the screen
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
    swap a ; Exchange bits
    lcall outniblcd
    swap a ; Again
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
DELAY:    
    MOV R3,#50
DEL2:    
    MOV R4,#255
DEL:    
    DJNZ R4,DEL
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
; hexbcd32 converts a 32-bit hex number into a 32-bit BCD number
; input: r3,r2,r1,r0 = 32-bit hex number (r3=msb)
; output: r3,r2,r1,r0 = 32-bit BCD number (r3=msb)
; The routine uses only r3,r2,r1,r0 of bank 0 (must be selected)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

hexbcd32:
    push acc           ; Save working registers on the stack
    push psw
    push 07h
    push 06h
    push 05h
    push 04h
    mov r7,#00h        ; Dividing by 10000000
    mov r6,#98h
    mov r5,#96h
    mov r4,#80h
    lcall div32        ; The remainder can be further divided, quotient needed (4 valid bits)
    mov a,r4
    swap a             ; Bits ready for result
    mov r7,#00h        ; Dividing by 1000000
    mov r6,#0fh
    mov r5,#42h
    mov r4,#40h
    lcall div32        ; The remainder can be further divided, quotient needed (4 valid bits)
    orl a,r4           ; The high byte is ready, temporarily saved on the stack
    push acc
    mov r7,#00h        ; Dividing by 100000
    mov r6,#01h
    mov r5,#86h
    mov r4,#a0h
    lcall div32        ; The remainder can be further divided, quotient needed (4 valid bits)
    mov a,r4
    swap a             ; Bits ready for result
    mov r7,#00h        ; Dividing by 10000
    mov r6,#00h
    mov r5,#27h
    mov r4,#10h
    lcall div32        ; The remainder can be further divided, quotient needed (4 valid bits)
    orl a,r4           ; The high byte is ready, temporarily saved on the stack
    push acc         
    mov r7,#00h        ; Dividing by 1000
    mov r6,#00h
    mov r5,#03h
    mov r4,#e8h
    lcall div32        ; The remainder can be further divided, quotient needed (4 valid bits)
    mov a,r4
    swap a             ; Bits ready for result
    mov r7,#00h        ; Dividing by 100
    mov r6,#00h
    mov r5,#00h
    mov r4,#64h
    lcall div32        ; The remainder can be further divided, quotient needed (4 valid bits)
    orl a,r4           ; The high byte is ready, temporarily saved on the stack
    push acc         
    mov r7,#00h        ; Dividing by 10
    mov r6,#00h
    mov r5,#00h
    mov r4,#0ah
    lcall div32        ; The remainder can be further divided, quotient needed (4 valid bits)
    mov a,r4
    swap a             ; Bits ready for result
    orl a,r0           ; The high byte is ready, temporarily saved on the stack
    mov r0,a           ; Transfer results from stack to appropriate registers
    pop acc
    mov r1,a
    pop acc
    mov r2,a
    pop acc
    mov r3,a
    pop 04h
    pop 05h
    pop 06h
    pop 07h
    pop psw
    pop acc
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

batt_loop:    
    lcall measure       ; Measure the selected channel
    lcall print
    mov a, #20
    lcall delaya0k05s   ; Add a small delay
    ljmp batt_loop      ; Endless loop

measure:
    lcall adcbattery
    anl b, #c0h
    push acc
    anl a, #3fh
    orl a, b
    rl a
    rl a
    mov r0, a
    pop acc
    anl a, #c0h
    rl a
    rl a
    mov r1, acc
    ret

printAsVoltage:
    ; Calculate voltage: (ADC * 10) / 1023
    ; Adjusting for the voltage divider
    mov r2, #88h        ; Adjust multiplier to match (ADC * 10)
    mov r3, #43h        ; The multiplication factor, close to 10.00 (128 * 0.078125) = 9.984375
    lcall mul16         ; Perform 16-bit multiplication

    mov r4, #00h
    mov r5, #03h        ; High byte of 1023
    mov r6, #00h        ; Low byte of 1023
    mov r7, #00h

    lcall div32         ; Perform 32-bit division

    ; Convert the result to BCD for display
    mov a, r4
    mov r0, acc
    mov a, r5
    mov r1, acc
    mov a, r6
    mov r2, acc
    mov a, r7
    mov r3, acc

    lcall hexbcd32      ; Convert the 32-bit result to BCD

    ; Display the result on the LCD
    mov a, r1
    swap a
    lcall lcdoutnib
    mov a, #'.'
    lcall DATAWRT
    mov a, r1
    lcall lcdoutnib
    mov a, r0
    lcall lcdoutbyte
    mov a, #'V'
    lcall DATAWRT
    ret

print:    
    mov a, #80h    
    lcall COMNWRT
    lcall printAsVoltage
    lcall printBatteryPercentage

    mov a,#Dbh
    lcall COMNWRT

    mov dptr,#back
    lcall lcdoutmsga
    ret

printBatteryPercentage:
    ; Calculate percentage: ((V_actual * 100) / 7.4)
    ; V_actual is in r0:r1 (high byte in r1, low byte in r0)

    ; Multiply by 100 to get percentage
    mov r2, #64h    ; Multiplier 100
    mov r3, #00h

    lcall mul16     ; Perform 16-bit multiplication

    ; Divide by 7.4 (scaled by 10 to 74)
    mov r4, #00h
    mov r5, #4Ah    ; High byte of 74
    mov r6, #00h    ; Low byte of 74
    mov r7, #00h

    lcall div32     ; Perform 32-bit division

    ; Convert the result to BCD for display
    mov a, r4
    mov r0, acc
    mov a, r5
    mov r1, acc
    mov a, r6
    mov r2, acc
    mov a, r7
    mov r3, acc

    lcall hexbcd32  ; Convert the 32-bit result to BCD

    ; Display the result on the LCD
    mov a,#c0h
    lcall COMNWRT
   
    mov a, r0
    lcall lcdoutbyte
    mov a, #'%'
    lcall DATAWRT
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialization of ADC for Battery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

initadc_battery: 
    push syscon0
    mov syscon0, #004h
    mov adc_page, #000h
    mov adc_globctr, #10110000b
    mov adc_inpcr0, #00ah
    mov adc_prar, #080h
    mov adc_page, #006h
    mov adc_crmr1, #001h
    pop syscon0
    ret

adcbattery:
    push syscon0
    mov syscon0, #004h
    mov adc_page, #006h
    mov adc_crpr1, #10000000b
    mov adc_page, #000h
    nop
    nop
adcbattery1:
    mov a, adc_globstr
    jb acc.0, adcbattery1
    mov adc_page, #002h
    mov a, adc_resr0h
    mov b, adc_resr0l
    pop syscon0
    ret
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     
start_read_3:
    lcall READ_KEY_3
    ljmp start_read_3

READ_KEY_3:
    MOV TEMPVAR,#03H
READ_KEY_AGAIN_3:
    MOV A,#00H
    CLR C
    JB KBD_CLK, $                                 
    MOV R2, #08h    ; 8 bits per byte
    JB KBD_CLK,$
    JNB KBD_CLK,$
    JB KBD_CLK,$
    JNB KBD_CLK,$
ReadByteLoop_3:
    MOV C, KBD_DATA; Read data bit
    RLC A               ; Rotate data bit into accumulator
    JB KBD_CLK,$
    JNB KBD_CLK,$
    DJNZ R2, ReadByteLoop_3   ; Loop until all bits are read
    JNB KBD_CLK,$
    JB KBD_CLK,$
    JNB KBD_CLK,$
    DJNZ TEMPVAR,READ_KEY_AGAIN_3

asciiconv_3:
PRESS_A:
    CJNE A,#10101000b,PRESS_B
    mov a,#'a'  
    lcall LCD_WRITE_CHAR
    RET

PRESS_B:
    CJNE A,#01001100b,PRESS_C
    mov a,#'b'
    lcall LCD_WRITE_CHAR
    RET

PRESS_C:
    CJNE A,#10000100b,PRESS_D
    mov a,#'c'
    lcall LCD_WRITE_CHAR
    RET

PRESS_D:
    CJNE A,#11000100b,PRESS_E
    mov a,#'d'
    lcall LCD_WRITE_CHAR
    RET

PRESS_E:
    CJNE A,#24H,PRESS_f
    mov a,#'e'
    lcall LCD_WRITE_CHAR
    RET
  
PRESS_f:
    CJNE A,#11010100b ,PRESS_G
    mov a,#'f'
    lcall LCD_WRITE_CHAR
    RET

PRESS_G:
    CJNE A,#00101100b,PRESS_H
    mov a,#'g'
    lcall LCD_WRITE_CHAR
    RET

PRESS_H:
    CJNE A,#11001100b,PRESS_I
    mov a,#'h'
    lcall LCD_WRITE_CHAR
    RET

PRESS_I:
    CJNE A,#11000010b,PRESS_J
    mov a,#'i'
    lcall LCD_WRITE_CHAR
    RET

PRESS_J:
    CJNE A,#11011100b,PRESS_K
    mov a,#'j'
    lcall LCD_WRITE_CHAR
    RET

PRESS_K:
    CJNE A,#42H,PRESS_L
    mov a,#'k'
    lcall LCD_WRITE_CHAR
    RET

PRESS_L:
    CJNE A,#11010010b,PRESS_M
    mov a,#'l'
    lcall LCD_WRITE_CHAR
    RET

PRESS_M:
    CJNE A,#00110010b,PRESS_N
    mov a,#'m'
    lcall LCD_WRITE_CHAR
    RET

PRESS_N:
    CJNE A,#10001100b,PRESS_O
    mov a,#'n'
    lcall LCD_WRITE_CHAR
    RET

PRESS_O:
    CJNE A,#00100010b,PRESS_P
    mov a,#'o'
    lcall LCD_WRITE_CHAR
    RET

PRESS_P:
    CJNE A,#10110010b,PRESS_Q
    mov a,#'p'
    lcall LCD_WRITE_CHAR
    RET

PRESS_Q:
    CJNE A,#00111000b,PRESS_R
    mov a,#'q'
    lcall LCD_WRITE_CHAR
    RET

PRESS_R:
    CJNE A,#10110100b,PRESS_S
    mov a,#'r'
    lcall LCD_WRITE_CHAR
    RET

PRESS_S:
    CJNE A,#11011000b,PRESS_T
    mov a,#'s'
    lcall LCD_WRITE_CHAR
    RET

PRESS_T:
    CJNE A,#00110100b,PRESS_U
    mov a,#'t'
    lcall LCD_WRITE_CHAR
    RET

PRESS_U:
    CJNE A,#3CH,PRESS_V
    mov a,#'u'
    lcall LCD_WRITE_CHAR
    RET

PRESS_V:
    CJNE A,#01010100b,PRESS_W
    mov a,#'v'
    lcall LCD_WRITE_CHAR
    RET

PRESS_W:
    CJNE A,#01011000b,PRESS_X
    mov a,#'w'
    lcall LCD_WRITE_CHAR
    RET

PRESS_X:
    CJNE A,#01000100b,PRESS_Y
    mov a,#'x'
    lcall LCD_WRITE_CHAR
    RET

PRESS_Y:
    CJNE A,#10101100b,PRESS_Z
    mov a,#'y'
    lcall LCD_WRITE_CHAR
    RET

PRESS_Z:
    CJNE A,#10111000b,PRESS_ques
    mov a,#'z'
    lcall LCD_WRITE_CHAR
    RET

PRESS_ques:
    CJNE A,#01011100b,PRESS_back
    mov a,#'?'
    lcall LCD_WRITE_CHAR
    RET 

PRESS_back:
    CJNE A, #'f', PRESS_enter
    MOV A, #10h    ; Command to move cursor left
    ACALL COMNWRT  ; Call the command write subroutine
    LCALL delay2ms
    MOV A, #20h    ; ASCII for space (blanking the character)
    ACALL DATAWRT  ; Call the data write subroutine
    LCALL delay2ms
    MOV A, #10h    ; Command to move cursor left again
    ACALL COMNWRT  ; Call the command write subroutine
    LCALL delay2ms
    RET

PRESS_enter:
    CJNE A, #'Z', PRESS_space
    MOV A, #95h    ; Command to move to the next line (arbitrary command)
    LCALL COMNWRT
    RET

PRESS_space:
    CJNE A, #10010100b, PRESS_1
    MOV A, #' '
    LCALL LCD_WRITE_CHAR
    RET

PRESS_1:
    CJNE A,#01101000b,PRESS_2
    mov a,#'1'
    lcall LCD_WRITE_CHAR
    RET

PRESS_2:
    CJNE A,#01111000b,PRESS_3
    mov a,#'2'
    lcall LCD_WRITE_CHAR
    RET

PRESS_3:
    CJNE A,#01100100b,PRESS_4
    mov a,#'3'
    lcall LCD_WRITE_CHAR
    RET

PRESS_4:
    CJNE A,#10100100b,PRESS_5
    mov a,#'4'
    lcall LCD_WRITE_CHAR
    RET

PRESS_5:
    CJNE A,#01110100b,PRESS_6
    mov a,#'5'
    lcall LCD_WRITE_CHAR
    RET

PRESS_6:
    CJNE A,#01101100b,PRESS_7
    mov a,#'6'
    lcall LCD_WRITE_CHAR
    RET

PRESS_7:
    CJNE A,#10111100b,PRESS_8
    mov a,#'7'
    lcall LCD_WRITE_CHAR
    RET

PRESS_8:
    CJNE A,#01111100b,PRESS_9
    mov a,#'8'
    lcall LCD_WRITE_CHAR
    RET

PRESS_9:
    CJNE A,#01100010b,PRESS_0
    mov a,#'9'
    lcall LCD_WRITE_CHAR
    RET

PRESS_0:
    CJNE A,#10100010b,PRESS_ESC1
    mov a,#'0'
    lcall LCD_WRITE_CHAR
    RET

PRESS_ESC1:
    CJNE A,#01101110b,EXIT1
    mov a,#cleardisplay
    lcall COMNWRT
    LCALL DELAY
    ljmp start

EXIT1:
    RET

LCD_WRITE_CHAR:
    ; Write character code in register A to the LCD
    LCALL DATAWRT

    ; Update cursor position (in register R0)
    INC R0

    ; Check if the cursor is at the end of the current line
    CJNE R0, #20, CONTINUE_WRITE  ; 20 characters per line

    ; Handle the line jump based on the current line (R1)
    CJNE R1, #0, CHECK_LINE_2

    ; Move from line 1 to line 2
    MOV A, #0xC0  ; DDRAM address for line 2
    MOV R1, #1
    SJMP SET_CURSOR

CHECK_LINE_2:
    CJNE R1, #1, CHECK_LINE_3

    ; Move from line 2 to line 3
    MOV A, #0x94  ; DDRAM address for line 3
    MOV R1, #2
    SJMP SET_CURSOR

CHECK_LINE_3:
    CJNE R1, #2, CHECK_LINE_4

    ; Move from line 3 to line 4
    MOV A, #0xD4  ; DDRAM address for line 4
    MOV R1, #3
    SJMP SET_CURSOR

CHECK_LINE_4:
    ; Move from line 4 to line 1
    MOV A, #0x80  ; DDRAM address for line 1
    MOV R1, #0

SET_CURSOR:
    ; Set the cursor position using COMNWRT
    LCALL COMNWRT
    MOV R0, #0  ; Reset cursor position to the beginning of the new line

CONTINUE_WRITE:
    RET

BUFFER_START EQU 30H                               

ADD_TO_BUFFER:
    MOV @R0, a
    inc r0             
    inc count
    ret
                     
pbuffer:
    MOV R0, #BUFFER_START  ; Reset R0 to the start of the buffer
    MOV R1, count          ; Move count to R1 for decrementing in loop

read1_loop:
    MOV A, @R0             ; Load the key from buffer into accumulator
    lcall DATAWRT
    INC R0                 ; Increment buffer pointer to the next key
    DJNZ R1, read1_loop    ; Decrement count and loop if not zero
    RET

; Messages to be displayed
welcome:        DB "Hello!",00h
chat:           DB "1.Chat",00h
batt:           DB "2.Battery",00h
clock:          DB "3.clock",00h
choose:         DB "Press 1,2 or 3:",00h
back:           DB "ESC to return",00h

#include "c:\xcez1.inc"



