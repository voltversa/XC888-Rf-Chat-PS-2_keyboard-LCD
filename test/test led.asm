
        org 0000h
        mov sp,#7fh

        lcall initp0

        loop:
        mov p0_data,#00h

        mov a,#1
        lcall delaya0k05s


        mov p0_data,#ffh


            mov a,#1
        lcall delaya0k05s


        ljmp loop












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

              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Delay1ms           is een tijdsvertraging van 1 ms.
;
; Gebruikt geen registers
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

delay1ms:     push   acc                  ;8*42ns=330ns
              push   psw                  ;8*42ns=330ns
              mov    acc,#100             ;420ns
delay1ms1:    lcall  delay10us            ;10us
              djnz   acc,delay1ms1        ;420ns*aantal keer doorlopen
              pop    psw                  ;8*42ns=330ns
              pop    acc                  ;8*42ns=330ns
              ret                         ;165ns
;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


delay10us:    push   acc                  ;8*42ns=330ns
              push   psw                  ;8*42ns=330ns
              mov    acc,#01ah            ;420ns
              djnz   acc,$                ;420ns*aantal keer doorlopen
              pop    psw                  ;8*42ns=330ns
              pop    acc                  ;8*42ns=330ns
              ret                         ;165ns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










initp0:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p0_dir,#0ffh         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              
            ret

#include	"c:\xcez1.inc"

