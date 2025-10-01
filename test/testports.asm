        



                   org 000h
                   lcall initp1
                   lcall initp3


                   loop:

                   mov p1_data,#ffh
                   mov p3_data,#ffh

                           
                   ljmp loop



















initp3:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p3_dir,#0ffh         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              
            ret


initp1:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p1_dir,#0ffh         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              
            ret

#include	"c:\xcez1.inc"
