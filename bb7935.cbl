      $SET DEFAULTBYTE(48)
      $SET SQL(dbman=ODBC)
      $SET NOOSVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bb7935.
      ******************************************************************
      * Program-ID.....: bb7935.cbl
      * Author.........: Joaquin H.
      * Date Written...: 21/10/2020 at 7:59:30
      * Last Updated...: 21/10/2020 at 11:53:36 by jluque01
      * Function.......: extract JITS price
      ******************************************************************
     /*/Description ****************************************************
     /*
     /* Extract JITS price
     /* 
     /*
     /*/End-description ************************************************
050720******************************************************************

      ********************************************************************************************************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  UNIX-SVR4.
       OBJECT-COMPUTER.  UNIX-SVR4.
       SPECIAL-NAMES.

       CLASS A-Z-OR-0-9 IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
       CLASS A-Z        IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       CLASS Y-OR-N     IS "YN"
b60811*CLASS REFER-A-Z  IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789- "
b60811*CLASS REFER-Z-A  IS "zyxwvutsrqponmlkjihgfedcba"
b60811 CLASS ALPHANUM   IS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQR
b60812-                    "STUVWXYZ0123456789 -.*@"
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.




           SELECT BOOKING-CHECK    ASSIGN WS-BOOKING-CHECK
                                   ORGANIZATION LINE SEQUENTIAL
                                   LOCK MODE IS MANUAL
                                   FILE STATUS IS WS-REPLY.

           SELECT PRICE-JITS-FILE  ASSIGN WS-PRICE-JITS-FILE
                                   ORGANIZATION LINE SEQUENTIAL
                                   LOCK MODE IS MANUAL
                                   FILE STATUS IS WS-REPLY.                        

           COPY "jtvbookingsold.sel".
           COPY "jttuball.sel".
           COPY "jtinvoice.sel".

           COPY "rechisto.sel"
                     REPLACING =="$DD_SYSFILES/RECHISTO"==
                                   BY ==WS-RECHISTO==.

           COPY "recairpo.sel"  
                      REPLACING =="$DD_SYSFILES/RECAIRPO"==
                                   BY ==WS-RECAIRPO==.


           COPY "njairporttax.sel"
                      REPLACING =="$DD_SYSFILES/NJAIRPORTTAX"==
                                   BY ==WS-NJAIRPORTTAX==.

           COPY "jtmemo.sel"
                      REPLACING =="$DD_SYSFILES/JTMEMO"== 
                                   BY ==WS-JTMEMO==.

           COPY "recroomd.sel"
                      REPLACING =="$DD_SYSFILES/RECROOMD"==
                                  BY ==WS-RECROOMD==.

           COPY "recfligh.sel"
                 REPLACING =="$DD_SYSFILES/RECFLIGH"==
                              BY ==WS-RECFLIGH==.

           COPY "njflightcost.sel"
                      REPLACING =="$DD_SYSFILES/NJFLIGHTCOST"==
                                   BY ==WS-NJFLIGHTCOST==.

           COPY "jtvbdatebooknr.sel"
                      REPLACING =="$DD_SYSFILES/JTVBDATEBOOKNR"==
                                   BY ==WS-JTVBDATEBOOKNR==.

           COPY "jtvbookings.sel"
                      REPLACING =="$DD_SYSFILES/JTVBOOKINGS"==
                                   BY ==WS-JTVBOOKINGS==.

           COPY "jtvagents.sel"
                      REPLACING =="$DD_SYSFILES/JTVAGENTS"==
                                   BY ==WS-JTVAGENTS==.

           COPY "jtgroupnames.sel"
                      REPLACING =="$DD_SYSFILES/JTGROUPNAMES"==
                                   BY ==WS-JTGROUPNAMES==.

           COPY "jtclients.sel"
                      REPLACING =="$DD_SYSFILES/JTCLIENTS"==
                                   BY ==WS-JTCLIENTS==.

           COPY "jtmchotels.sel"
                      REPLACING =="$DD_SYSFILES/JTMCHOTELS"==
                                   BY ==WS-JTMCHOTELS==.

           COPY "jtvhotels.sel"
                      REPLACING =="$DD_SYSFILES/JTVHOTELS"==
                                   BY ==WS-JTVHOTELS==.

           COPY "jtvflights.sel"
                      REPLACING =="$DD_SYSFILES/JTVflights"==
                                  BY ==WS-JTVFLIGHTS==.
           COPY "jtaccounts.sel"
                 REPLACING =="$DD_SYSFILES/JTACCOUNTS"==
                                   BY ==WS-JTACCOUNTS==.


           COPY "jtmcprice.sel"
                      REPLACING =="$DD_SYSFILES/JTMCPRICE"==
                                   BY ==WS-JTMCPRICE==.


           COPY "jtfaxmemo.sel"
                      REPLACING =="$DD_SYSFILES/JTFAXMEMO"==
                                   BY ==WS-JTFAXMEMO==.



           COPY "njprodautoprc.sel"
                      REPLACING ==:TAG:== BY ==NJPP==
                                =="$DD_SYSFILES/NJPRODAUTOPRC"==
                                          BY ==WS-NJPRODAUTOPRC==.


          COPY "njprodautohtl.sel"
                              REPLACING
                              ==:TAG:== BY ==NJPH==
                              =="$DD_SYSFILES/NJPRODAUTOHTL"==
                              BY ==WS-NJPRODAUTOHTL==.     

           COPY "jtprice.sel"
                      REPLACING =="$DD_SYSFILES/JTPRICE"==
                                   BY ==WS-JTPRICE==.


           COPY "jpromo1.sel"
                      REPLACING =="$DD_SYSFILES/JPROMO1"==
                                   BY ==WS-JPROMO1==.

           COPY "njpromo1.sel"
                      REPLACING =="$DD_SYSFILES/NJPROMO1"==
                                   BY ==WS-NJPROMO1==
                                ==:TAG01:== BY ==01 NJPR==
                                ==:TAG02:== BY ==02 NJPR==
                                ==:TAG03:== BY ==03 NJPR==
                                ==:TAG04:== BY ==04 NJPR==
                                ==:TAG05:== BY ==05 NJPR==
                                ==:TAG07:== BY ==07 NJPR==
                                ==:TAG08:== BY ==08 NJPR==
                                ==:TAG09:== BY ==09 NJPR==
                                ==:TAG11:== BY ==11 NJPR==
                                ==:TAG13:== BY ==13 NJPR==
                                ==:TAG:== BY ==NJPR==
                                ==:RED:== BY ==REDEFINES==
                                ==:RED05:== BY ==05 REDEFINES==
                                ==:RED07:== BY ==07 REDEFINES==
                                ==:RED09:== BY ==09 REDEFINES==
                                ==:RED11:== BY ==11 REDEFINES==.


           COPY "recprom3.sel"
                      REPLACING =="$DD_SYSFILES/RECPROM3"==
                                   BY ==WS-RECPROM3==.

    
           COPY "jtvproductip.sel"
                      REPLACING =="$DD_SYSFILES/JTVPRODUCTIP"==
                                   BY ==WS-JTVPRODUCTIP==.


           COPY "jtlongstay.sel"
                      REPLACING =="$DD_SYSFILES/JTLONGSTAY"==
                                   BY ==WS-JTLONGSTAY==.

           COPY "jtcommiss.sel"
                      REPLACING =="$DD_SYSFILES/JTCOMMISS"==
                                   BY ==WS-JTCOMMISS==.

           COPY "jtdienstregeling.sel"
                      REPLACING =="$DD_SYSFILES/JTDIENSTREGELING"==
                                   BY ==WS-JTDIENSTREGELING==.



           SELECT RATES          ASSIGN WS-EUROCURRENCYRATES
                                 ORGANIZATION LINE SEQUENTIAL
                                 LOCK MODE IS MANUAL
                                 FILE STATUS IS WS-REPLY.

           SELECT JTDWH          ASSIGN WS-JTDWH
                                 ORGANISATION INDEXED
                                 ACCESS DYNAMIC
                                 RECORD KEY JTDWH-KEY
                                 LOCK MODE IS MANUAL
                                 FILE STATUS WS-REPLY.         


           COPY "jttuitickets.sel".
           COPY "jttuitickall.sel".

           COPY "jtlhtax.sel".

           SELECT JTPOOFFER      ASSIGN WS-JTPOOFFER
                                 ORGANISATION INDEXED
                                 ACCESS DYNAMIC
                                 RECORD KEY JTPOO-KEY
                                 LOCK MODE IS MANUAL
                                 FILE STATUS WS-REPLY.


           SELECT ERRFILET      ASSIGN WS-ERRFILENAMET
                                ORGANIZATION IS LINE SEQUENTIAL
                                LOCK MODE IS MANUAL
                                FILE STATUS IS WS-REPLY.

           COPY "jtpaxbvr.sel"
                      REPLACING =="$DD_SYSFILES/JTPAXBVR"==
                                   BY ==WS-JTPAXBVR==.

  
           COPY "jtvmuntkoersen.sel"
                      REPLACING =="$DD_SYSFILES/JTVMUNTKOERSEN"==
                                   BY ==WS-JTVMUNTKOERSEN==.

           COPY "jtvbflexprice.sel"
                      REPLACING =="$DD_SYSFILES/JTVBFLEXPRICE"==
                                   BY ==WS-JTVBFLEXPRICE==.

      
      $SET NODATACOMPRESS

           SELECT JTTRANS1    ASSIGN "$DD_SYSFILES/JTTRANS1"             *> Only new bookings of 1 day (today), daily reset
                              ORGANIZATION SEQUENTIAL
                              LOCK MODE IS MANUAL
                              FILE STATUS WS-REPLY.


           SELECT LISTMODCALC ASSIGN TO LISTMODCALC-FILENAME
                              ORGANIZATION IS LINE SEQUENTIAL
                              FILE STATUS WS-REPLY.


       DATA DIVISION.
       FILE SECTION.

       FD BOOKING-CHECK.
       01 BOOKING-CHECK-RECORD  PIC X(50).

       FD PRICE-JITS-FILE.
       01 PRICE-JITS-FILE-RECORD.
          03 WS-OUTPUT-BOOKNR           PIC X(9).
          03 FILLER                     PIC X.
          03 WS-OUTPUT-PAX              PIC X.
          03 FILLER                     PIC X.
          03 WS-OUTPUT-DWH-PRICE        PIC X(8).
          03 FILLER                     PIC X.
          03 WS-OUTPUT-FILE-PRICE       PIC ZZZ.ZZ9,99.
          03 FILLER                     PIC X.
          03 WS-OUTPUT-MODCALC-PRICE    PIC ZZZ.ZZ9,99.
          03 FILLER                     PIC X.
          03 WS-OUTPUT-WB-PRICE         PIC -ZZZ.ZZ9,99.

       01 PRICE-JITS-FILE-TITLE
          REDEFINES PRICE-JITS-FILE-RECORD  PIC X(100).   



       FD  JTTUBALL               EXTERNAL
                                  RECORDING MODE IS VARIABLE.
       COPY "jttuball.cpy".

       FD ERRFILET                RECORDING MODE IS VARIABLE.
       01  ERRMESSAGET            PIC X(80).



       FD  RECAIRPO               EXTERNAL
                                  RECORDING MODE IS VARIABLE.
       COPY "recairpo.cpy".

       FD  NJAIRPORTTAX           EXTERNAL
                                  RECORDING MODE IS VARIABLE.
       COPY "njairporttax.cpy".

       FD  RECFLIGH               RECORDING MODE IS VARIABLE.
       COPY "recfligh.cpy".

       FD  NJFLIGHTCOST           RECORDING MODE IS VARIABLE.
       COPY "njflightcost.cpy".

       FD  RECROOMD               EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "recroomd.cpy".

       FD  JTVPRODUCTIP           EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtvproductip.cpy".

       FD  JTVBDATEBOOKNR         EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtvbdatebooknr.cpy".

       FD  JTVBOOKINGS            EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtvbookingstag.cpy"
                      REPLACING ==:TAG01:== BY ==01 JTVB==
                                ==:TAG02:== BY ==02 JTVB==
                                ==:TAG03:== BY ==03 JTVB==
                                ==:TAG04:== BY ==04 JTVB==
                                ==:TAG05:== BY ==05 JTVB==
                                ==:TAG07:== BY ==07 JTVB==
                                ==:TAG09:== BY ==09 JTVB==
                                ==:TAG11:== BY ==11 JTVB==
                                ==:TAG88:== BY ==88 JTVB==
                                ==:TAG:== BY ==JTVB==
                                ==:TAGX:== BY ==JTVB==
                                ==:RED:== BY ==REDEFINES==
                                ==:RED03:== BY ==03 REDEFINES==
                                ==:RED05:== BY ==05 REDEFINES==
                                ==:RED07:== BY ==07 REDEFINES==
                                ==:RED09:== BY ==09 REDEFINES==.

       FD  JTVBOOKINGSOLD         EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtvbookingstag.cpy"
                      REPLACING ==:TAG01:== BY ==01 JTVO==
                                ==:TAG02:== BY ==02 JTVO==
                                ==:TAG03:== BY ==03 JTVO==
                                ==:TAG04:== BY ==04 JTVO==
                                ==:TAG05:== BY ==05 JTVO==
                                ==:TAG07:== BY ==07 JTVO==
                                ==:TAG09:== BY ==09 JTVO==
                                ==:TAG11:== BY ==11 JTVO==
                                ==:TAG88:== BY ==88 JTVO==
                                ==:TAG:== BY ==JTVO==
                                ==:TAGX:== BY ==JTVO==
                                ==:RED:== BY ==REDEFINES==
                                ==:RED03:== BY ==03 REDEFINES==
                                ==:RED05:== BY ==05 REDEFINES==
                                ==:RED07:== BY ==07 REDEFINES==
                                ==:RED09:== BY ==09 REDEFINES==.

       FD  JTGROUPNAMES           EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtgroupnames.cpy".

       FD  JTCLIENTS              EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtclients.cpy".

       FD  JTVAGENTS              EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtvagents.cpy".

       FD JTMCHOTELS              EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtmchotels.cpy"
                      REPLACING ==:TAG01:== BY ==01 JTMH==
                                ==:TAG03:== BY ==03 JTMH==
                                ==:TAG05:== BY ==05 JTMH==
                                ==:TAG07:== BY ==07 JTMH==
                                ==:TAG09:== BY ==09 JTMH==
                                ==:TAG11:== BY ==11 JTMH==
                                ==:TAG13:== BY ==13 JTMH==
                                ==:TAG:== BY ==JTMH==
                                ==:RED:== BY ==REDEFINES==
                                ==:RED03:== BY ==03 REDEFINES==
                                ==:RED05:== BY ==05 REDEFINES==
                                ==:RED07:== BY ==07 REDEFINES==.

       FD  JTVHOTELS              EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtvhotels.cpy".

       FD JTINVOICE            EXTERNAL
                               RECORDING MODE IS VARIABLE.
        COPY "jtinvoice.cpy".

       FD  JTVFLIGHTS             EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtvflights.cpy".

       FD  JTACCOUNTS             EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtaccountstag.cpy"
                      REPLACING ==:TAG01:== BY ==01 JTAC==
                                ==:TAG03:== BY ==03 JTAC==
                                ==:TAG05:== BY ==05 JTAC==
                                ==:TAG07:== BY ==07 JTAC==
                                ==:TAG09:== BY ==09 JTAC==
                                ==:TAG11:== BY ==11 JTAC==
                                ==:TAG13:== BY ==13 JTAC==
                                ==:RED05:== BY ==05 REDEFINES==
                                ==:TAG:== BY ==JTAC==
                                ==:RED03:== BY ==03 REDEFINES==.


       FD  JTMCPRICE              EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtmcprice.cpy".

       FD  RECPROM3               EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "recprom3.cpy".

       FD  JTPRICE                EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtprice.cpy".

071123 FD  NJPRODAUTOHTL        EXTERNAL
071123                          RECORDING MODE IS VARIABLE.
071123 COPY "njprodautohtltag.cpy"
071123                REPLACING ==:TAG01:== BY ==01 NJPH==
071123                          ==:TAG02:== BY ==02 NJPH==
071123                          ==:TAG03:== BY ==03 NJPH==
071123                          ==:TAG04:== BY ==04 NJPH==
071123                          ==:TAG05:== BY ==05 NJPH==
071123                          ==:TAG07:== BY ==07 NJPH==
071123                          ==:TAG08:== BY ==08 NJPH==
071123                          ==:TAG09:== BY ==09 NJPH==
071123                          ==:TAG11:== BY ==11 NJPH==
071123                          ==:TAG13:== BY ==13 NJPH==
071123                          ==:TAG15:== BY ==15 NJPH==
071123                          ==:TAG88:== BY ==88 NJPH==
071123                          ==:TAG:== BY ==NJPH==
071123                          ==:RED:== BY ==REDEFINES==
071123                          ==:RED03:== BY ==03 REDEFINES==
071123                          ==:RED05:== BY ==05 REDEFINES==
071123                          ==:RED07:== BY ==07 REDEFINES==
071123                          ==:RED09:== BY ==09 REDEFINES==
071123                          ==:RED11:== BY ==11 REDEFINES==
071123                          ==:RED13:== BY ==13 REDEFINES==.


071123 FD  NJPRODAUTOPRC        EXTERNAL
071123                          RECORDING MODE IS VARIABLE.
071123 COPY "njprodautoprctag.cpy"
071123                REPLACING ==:TAG01:== BY ==01 NJPP==
071123                          ==:TAG02:== BY ==02 NJPP==
071123                          ==:TAG03:== BY ==03 NJPP==
071123                          ==:TAG04:== BY ==04 NJPP==
071123                          ==:TAG05:== BY ==05 NJPP==
071123                          ==:TAG07:== BY ==07 NJPP==
071123                          ==:TAG08:== BY ==08 NJPP==
071123                          ==:TAG09:== BY ==09 NJPP==
071123                          ==:TAG11:== BY ==11 NJPP==
071123                          ==:TAG13:== BY ==13 NJPP==
071123                          ==:TAG15:== BY ==15 NJPP==
071123                          ==:TAG88:== BY ==88 NJPP==
071123                          ==:TAG:== BY ==NJPP==
071123                          ==:RED:== BY ==REDEFINES==
071123                          ==:RED03:== BY ==03 REDEFINES==
071123                          ==:RED05:== BY ==05 REDEFINES==
071123                          ==:RED07:== BY ==07 REDEFINES==
071123                          ==:RED09:== BY ==09 REDEFINES==
071123                          ==:RED11:== BY ==11 REDEFINES==
071123                          ==:RED13:== BY ==13 REDEFINES==.



       FD  JPROMO1                EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jpromo1.cpy".

h90903 FD  NJPROMO1               EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "njpromo1tag.cpy"
                      REPLACING ==:TAG01:== BY ==01 NJPR==
                                ==:TAG02:== BY ==02 NJPR==
                                ==:TAG03:== BY ==03 NJPR==
                                ==:TAG04:== BY ==04 NJPR==
                                ==:TAG05:== BY ==05 NJPR==
                                ==:TAG07:== BY ==07 NJPR==
                                ==:TAG08:== BY ==08 NJPR==
                                ==:TAG09:== BY ==09 NJPR==
                                ==:TAG11:== BY ==11 NJPR==
                                ==:TAG13:== BY ==13 NJPR==
                                ==:TAG:== BY ==NJPR==
                                ==:RED:== BY ==REDEFINES==
                                ==:RED05:== BY ==05 REDEFINES==
                                ==:RED07:== BY ==07 REDEFINES==
                                ==:RED09:== BY ==09 REDEFINES==
                                ==:RED11:== BY ==11 REDEFINES==.

       FD  JTMEMO                 EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtmemo.cpy".

      *FD  JTINVOICE              EXTERNAL RECORDING MODE IS VARIABLE.
      *COPY "jtinvoice.cpy".

       FD  JTFAXMEMO              EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtfaxmemo.cpy".

       FD  JTLONGSTAY             EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtlongstay.cpy".

       FD  JTCOMMISS              EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtcommiss.cpy".


       FD  JTDIENSTREGELING       EXTERNAL RECORDING MODE IS VARIABLE.
       COPY "jtdienstregeling.cpy".

       FD  RATES.
       01  RATES-RECORD  PIC X(100).

       FD  JTDWH                        RECORDING MODE IS VARIABLE.
       COPY "jtdwh.cpy".

       FD  RECHISTO                     EXTERNAL
                                        RECORDING MODE IS VARIABLE.
       COPY "rechistotag.cpy"
                      REPLACING ==:TAG01:== BY ==01 HIS==
                                ==:TAG02:== BY ==02 HIS==
                                ==:TAG03:== BY ==03 HIS==
                                ==:TAG04:== BY ==04 HIS==
                                ==:TAG05:== BY ==05 HIS==
                                ==:TAG07:== BY ==07 HIS==
                                ==:TAG09:== BY ==09 HIS==
                                ==:TAG11:== BY ==11 HIS==
                                ==:TAG88:== BY ==88 HIS==
                                ==:TAG:== BY ==HIS==
                                ==:TAGX:== BY ==HIS==
                                ==:RED:== BY ==REDEFINES==
                                ==:RED03:== BY ==03 REDEFINES==
                                ==:RED05:== BY ==05 REDEFINES==
                                ==:RED07:== BY ==07 REDEFINES==
                                ==:RED09:== BY ==09 REDEFINES==.


030902 FD  JTTUITICKETS                 EXTERNAL
                                        RECORDING MODE IS VARIABLE.
           COPY "jttuitickets.cpy".

030902 FD  JTTUITICKALL                 EXTERNAL
                                        RECORDING MODE IS VARIABLE.
030902     COPY "jttuitickall.cpy".

041001 FD  JTLHTAX                      EXTERNAL
041001                                  RECORDING MODE IS VARIABLE.
041001 COPY "jtlhtaxtag.cpy"
041001                REPLACING ==:TAG01:== BY ==01 JTLH==
041001                          ==:TAG02:== BY ==02 JTLH==
041001                          ==:TAG03:== BY ==03 JTLH==
041001                          ==:TAG04:== BY ==04 JTLH==
041001                          ==:TAG05:== BY ==05 JTLH==
041001                          ==:TAG07:== BY ==07 JTLH==
041001                          ==:TAG09:== BY ==09 JTLH==
                                ==:TAGX:== BY ==JTLH==
                                ==:RED:== BY ==REDEFINES==.

051201 FD JTPOOFFER                      RECORDING MODE IS VARIABLE.
051201 COPY "jtpooffer.cpy".

071214 FD  JTPAXBVR.
071214 COPY "jtpaxbvrtag.cpy"
071214                REPLACING ==:TAG01:== BY ==01 JTPB==
071214                          ==:TAG03:== BY ==03 JTPB==
071214                          ==:TAG05:== BY ==05 JTPB==
071214                          ==:TAG07:== BY ==07 JTPB==
071214                          ==:TAG09:== BY ==09 JTPB==.

d80215 FD  JTVMUNTKOERSEN EXTERNAL RECORDING MODE IS VARIABLE.
d80215 COPY "jtvmuntkoersentag.cpy"
d80215      REPLACING ==:TAG:==   BY ==JTVMUNT==
d80215                ==:TAG01:== BY ==01 JTVMUNT==
d80215                ==:TAG03:== BY ==03 JTVMUNT==
d80215                ==:TAG05:== BY ==05 JTVMUNT==
d80215                ==:TAG06:== BY ==06 JTVMUNT==
d80215                ==:TAG07:== BY ==07 JTVMUNT==.

d80215 FD  JTVBFLEXPRICE EXTERNAL RECORDING MODE IS VARIABLE.
d80215 COPY "jtvbflexprice.cpy".


       FD LISTMODCALC.
       01 LISTMODCALC-RECORD                        PIC X(50).

       FD JTTRANS1              RECORDING MODE IS VARIABLE.
       COPY "jttrans1.cpy"
                    REPLACING ==:TAGJT:== BY ==JTTR1==
                              ==:TAG01:== BY ==03 JTTR1-BOOK==
                              ==:TAG02:== BY ==05 JTTR1==
                              ==:TAG03:== BY ==07 JTTR1==
                              ==:TAG04:== BY ==09 JTTR1==
                              ==:TAG05:== BY ==10 JTTR1==
                              ==:TAG07:== BY ==12 JTTR1==
                              ==:TAG09:== BY ==14 JTTR1==
                              ==:TAG11:== BY ==16 JTTR1==
                              ==:TAG13:== BY ==18 JTTR1==
                              ==:TAG88:== BY ==88 JTTR1==
                              ==:TAG:== BY ==JTTR1==
                              ==:TAGX:== BY ==JTTR1-BOOK==
                              ==:RED:== BY ==REDEFINES==
                              ==:RED03:== BY ==07 REDEFINES==
                              ==:RED05:== BY ==10 REDEFINES==
                              ==:RED07:== BY ==12 REDEFINES==
                              ==:RED09:== BY ==14 REDEFINES==
                              ==:RED11:== BY ==16 REDEFINES==.


       WORKING-STORAGE SECTION.
       COPY "wsmunten.cpy".
o20705 COPY "ws_flightroutes.cpy".
o20705 01 WS-HULPVELDEN-FLIGHTS.
o20705    03 TOT-FETCH-AIRPORT        PIC 9(9).
o20705    03 WS-PREV-FLIGHT-CODE      PIC X(3).
o20706    03 WS-DEP-AIRPORT           PIC X(3).
o20706    03 WS-DESTINATION           PIC X(3).
o21015    03 PREV-DESTINATION         PIC X(3).
o21015    03 PREV-SEASON              PIC 9.
o21015    03 PREV-SUBCODE             PIC X(5).
o21015    03 PREV-USER                PIC 9.
o21015    03 PREV-ACCOM-TYPE          PIC X.


       COPY "unixvelden.cpy".
cjtvh  COPY "ws-jtvh-fields.cpy".
071126 COPY "wsl132.cpy".
       COPY "y2kdata.cpy".
060511 COPY "arradata.cpy"       .
071130 COPY "wsintranet.cpy".
o21015 COPY "jade.cpy".
o21015 COPY "jadedata.cpy".
071130 COPY "wsnjprodauto.cpy".
a50831 COPY "c2dsqlsea.cpy".
a50831 COPY "season.cpy".
x90112 COPY "wssql.cbl.cpy".
       COPY "commoncpy.cpy".


       01 WS-REPLY                     PIC X(2).
       01 WS-FILE-NAMES.
          03 WS-BOOKENTRY              PIC X(60).
          03 WS-BUSTRANS               PIC X(60).
          03 WS-TRAVAGENT              PIC X(60).
          03 WS-BOOKPRODVAR            PIC X(60).
          03 WS-BOOKACCOUNIT           PIC X(60).
          03 WS-BOOKFLIGHTSEG          PIC X(60).
          03 WS-BOOKACCOBOARD          PIC X(60).
          03 WS-BOOKTRANSSERV          PIC X(60).
          03 WS-REDSUPPERBPV           PIC X(60).
          03 WS-PROMPERBT              PIC X(60).
          03 WS-REDSUPPERBT            PIC X(60).
          03 WS-BOOKOTHERSERV          PIC X(60).
          03 WS-RSACCOUNIT             PIC X(60).
          03 WS-RSACCOBOARD            PIC X(60).
          03 WS-DWHISBOOKACCOBOARD     PIC X(60).
          03 WS-DWHISBOOKACCOUNIT      PIC X(60).
          03 WS-DWHISBOOKENTRY         PIC X(60).
          03 WS-DWHISBOOKFLIGHTSEG     PIC X(60).
          03 WS-DWHISBOOKOTHERSERV     PIC X(60).
          03 WS-DWHISBOOKPRODVAR       PIC X(60).
          03 WS-DWHISBOOKTRANSSERV     PIC X(60).
          03 WS-DWHISBUSTRANS          PIC X(60).
          03 WS-DWHISPROMPERBT         PIC X(60).
          03 WS-DWHISREDSUPPERBPV      PIC X(60).
          03 WS-DWHISREDSUPPERBT       PIC X(60).
          03 WS-DWHISRSACCOBOARD       PIC X(60).
          03 WS-DWHISRSACCOUNIT        PIC X(60).
          03 WS-DWHISTRAVAGENT         PIC X(60).
          03 WS-JTDWHINVLINES          PIC X(60).
020604    03 WS-JTEXPHOTELS            PIC X(55).
          03 WS-JTVHOTELS              PIC X(55).
          03 WS-RECROOMD               PIC X(55).
          03 WS-JTVPRODUCTIP           PIC X(55).
          03 WS-JTMEMO                 PIC X(55).
          03 WS-RECFLIGH               PIC X(55).
060519    03 WS-NJFLIGHTCOST           PIC X(55).
          03 WS-JTVBDATEBOOKNR         PIC X(55).
          03 WS-JTVBOOKINGS            PIC X(55).
          03 WS-JTVAGENTS              PIC X(55).
          03 WS-JTGROUPNAMES           PIC X(55).
          03 WS-JTCLIENTS              PIC X(55).
          03 WS-JTMCHOTELS             PIC X(55).
          03 WS-JTVFLIGHTS             PIC X(55).
          03 WS-JTACCOUNTS             PIC X(55).
          03 WS-JTRHOTELDATA           PIC X(55).
          03 WS-JTMCPRICE              PIC X(55).
          03 WS-JTFAXMEMO              PIC X(55).
          03 WS-JTPRICE                PIC X(55).
          03 WS-JPROMO1                PIC X(55).
h90903    03 WS-NJPROMO1               PIC X(55).
          03 WS-RECPROM3               PIC X(55).
          03 WS-JTLONGSTAY             PIC X(55).
          03 WS-JTCOMMISS              PIC X(55).
          03 WS-EUROCURRENCYRATES      PIC X(55).
          03 WS-JTDWH                  PIC X(55).
          03 WS-RECHISTO               PIC X(55).
          03 WS-RECAIRPO               PIC X(55).
060519    03 WS-NJAIRPORTTAX           PIC X(55).
051201    03 WS-JTPOOFFER              PIC X(55).
071123    03 WS-NJPRODAUTOHTL          PIC X(55).
071123    03 WS-NJPRODAUTOPRC          PIC X(55).
071214    03 WS-JTPAXBVR               PIC X(55).
d80215    03 WS-JTVMUNTKOERSEN         PIC X(55).
d80215    03 WS-JTVBFLEXPRICE          PIC X(55).


d      01 L01-IN.
d         03 L01-DOCNR                 PIC 9(8).
d     *   03 L01-BOOKNR                PIC 9(7).
nbookd    03 L01-BOOKNR9                PIC 9(9).
d      01 L02-OUT.
d         03 L02-BRUTO                 PIC 9(8)V99.
d         03 L02-NETTO                 PIC 9(8)V99.
d         03 L02-MUNT                  PIC X.
d         03 L02-COMM-BASIS            PIC 9(8)V99.
d         03 L02-COMM-PCT              PIC 99999.
d         03 L02-COMM-MARKER           PIC X.
d         03 L02-COMM                  PIC 9(8)V99.
d         03 L02-SUPER-BASIS           PIC 9(8)V99.
d         03 L02-SUPER-PCT             PIC 99999.
d         03 L02-SUPER-MARKER          PIC X.
d         03 L02-SUPER-COMM            PIC 9(8)V99.
d         03 L02-BELASTB-BEDRAG        PIC 9(8)V99.
d         03 L02-TVA                   PIC 9(8)V99.
d         03 L02-BELASTB0-BEDRAG       PIC 9(8)V99.
d         03 L02-TVA0                  PIC 9(8)V99.
d         03 L02-BELASTBT-BEDRAG       PIC 9(8)V99.
d         03 L02-TVAT                  PIC 9(8)V99.
d         03 L02-LHTAX                 PIC 9(8)V99.
d         03 L02-DEPOSIT               PIC 9(8)V99.
d         03 L02-DEPOSIT2              PIC 9(8)V99.
d         03 L02-COMM-INS              PIC 9(8)V99.
d      01 L03-ERROR.
d         03 L03-REPLY                 PIC S9(2).
d         03 L03-MESSAGE               PIC X(40).



o10510 COPY "jtmchotels.cpy"
o10510                REPLACING ==:TAG01:== BY ==01 JTSW==
o10510                          ==:TAG03:== BY ==03 JTSW==
o10510                          ==:TAG05:== BY ==05 JTSW==
o10510                          ==:TAG07:== BY ==07 JTSW==
o10510                          ==:TAG09:== BY ==09 JTSW==
o10510                          ==:TAG11:== BY ==11 JTSW==
o10510                          ==:TAG13:== BY ==13 JTSW==
o10510                          ==:TAG:== BY ==JTSW==
o10510                          ==:RED:== BY ==REDEFINES==
o10510                          ==:RED03:== BY ==03 REDEFINES==
o10510                          ==:RED05:== BY ==05 REDEFINES==
o10510                          ==:RED07:== BY ==07 REDEFINES==.

C60927 COPY "modgethotel.cpy".
C60927 COPY "i2dhtlis.cpy".
C60927 COPY "i2derror.cpy".



       COPY "wsbatch1.cpy".
       01 REDEFINES BA-USER-PARAMS.        
          03 BA-INPUT-FILE                 PIC X(30).
          03 BA-DATE-FROM                  PIC 9(8).
          03 BA-DATE-TO                    PIC 9(8).
          03 BA-OK                         PIC X.
          03 BA-FILE-SOURCE                PIC X.


       01 WS-CALL-MODVIPUPGROOM         PIC X(13) VALUE "modvipupgroom".        

       01 SW-END-OF-FILE                   PIC X.
          88 SW-END-OF-FILE-YES            VALUE "Y".
          88 SW-END-OF-FILE-NO             VALUE "N".

       01 WS-INPUT-BOOKNR                  PIC 9(9).
       01 WS-INPUT-PAX                     PIC X.
       01 WS-INPUT-PRICE                   PIC X(8).           
       01 TOT-INPUT-BOOKINGS               PIC 9(9).
       01 TOT-OUTPUT-BOOKING               PIC 9(9).

       01 WS-SALDO                         PIC 9(6)V9(2).
       01 WS-MODCALC-PRICE                 PIC S9(6)V9(2).  
       01 WS-WB-PRICE                      PIC S9(6)V9(2).        

       01 WS-IND                           PIC 9(5).
       01 WS-Q                             PIC 9(5).
       01 WS-FROM-BIN                      PIC S9(08) COMP SYNC.
       01 WS-TO-BIN                        PIC S9(08) COMP SYNC.

       01 TBCW-CURRENT-WAARDEBONNEN.
          03 TBCW-CURRENT-WAARDEBONNEN-INFO OCCURS 60
                                            DEPENDING ON WS-IND-WB.
             05 TBCW-WAARDE                 PIC S9(6)V9(2) VALUES ZEROS.
          03 WS-IND-WB                      PIC 9(2) VALUE ZEROS.     
       01 WS-BOOK-NUM                       PIC 9(9).
       01 WS-BOOK-ALF REDEFINES WS-BOOK-NUM PIC X(9). 
       01 WS-PO-GET-POUSER                  PIC X(9). 
       

       01 SQL-CURRENT-WAARDEBONNEN.
          03 SQL-WB-DEPTUITGEVER            PIC X(04)   VALUES SPACES.
          03 SQL-WB-YEAR                    PIC X(02)   VALUES SPACES.
          03 SQL-WB-NUMBER                  PIC X(06)   VALUES SPACES.
          03 SQL-WB-STATUS                  PIC X(04)   VALUES SPACES.
          03 SQL-WB-DATE                    PIC X(08)   VALUES SPACES.
          03 SQL-WB-BROCHURE                PIC X(200)  VALUES SPACES.
          03 SQL-WB-ACTIVATIONCODE          PIC X(10)   VALUES SPACES.
          03 SQL-WB-WAARDE                  PIC X(09)   VALUES SPACES.
          03 SQL-WB-DEELWAARDE              PIC X(09)   VALUES SPACES.
          03 SQL-WB-CURRENCY                PIC X(03)   VALUES SPACES.
          03 SQL-WB-CURRENCY-AMOUNT         PIC X(13)   VALUES SPACES.
          03 SQL-WB-CURRENCY-DEELWAARDE     PIC X(13)   VALUES SPACES.
          03 SQL-WB-CRISIS-REFERENCE        PIC X(50)   VALUES SPACES.
          03 SQL-WB-LK-DEPTUITGEVER         PIC X(04)   VALUES SPACES.
          03 SQL-WB-LK-YEAR                 PIC X(02)   VALUES SPACES.
          03 SQL-WB-LK-NUMBER               PIC X(06)   VALUES SPACES.

       01 WS-DATE-NUM                        PIC 9(08).
       01 WS-DATE-FILE REDEFINES WS-DATE-NUM PIC X(8).
       01 X                                  PIC 9      VALUE ZEROS.
       01 WS-CHECK-VOUCHER                   PIC X(80)  VALUE SPACES.

071126 COPY "link460.cpy".                                            *> do not add working storage after this copybook
       COPY "modredpo.cpy".
       COPY "modvipupgroom.cpy".

       PROCEDURE DIVISION.

      **** unix time stamps (auto inserted) ***************************
           MOVE ZEROES TO UNIX-DATE-ACCEPTED-NUM.
           ACCEPT UNIX-DATE-ACCEPTED-NUM6 FROM DATE.
           ACCEPT UNIX-TIME-ACCEPTED FROM TIME.
           ADD 20000000 TO UNIX-DATE-ACCEPTED-NUM.
           MOVE FUNCTION WHEN-COMPILED TO UNIX-DATE-COMPILED.

           DISPLAY "++++".
           DISPLAY "++++ Last Amended: 20/10/2020 11:53:36 jluque01".
           DISPLAY "++++ Last Compiled :"
                   " " UNIX-DAY "/" UNIX-MONTH "/" UNIX-YEAR
                   " " UNIX-HOUR ":" UNIX-MINUTES ":" UNIX-SEC.
           DISPLAY "++++ Run started at:"
                   " " UNIX-DATE-ACCEPTED-DD
                   "/" UNIX-DATE-ACCEPTED-MM
                   "/" UNIX-DATE-ACCEPTED-YEAR
                   " " UNIX-TIME-ACCEPTED-UUR
                   ":" UNIX-TIME-ACCEPTED-MIN
                   ":" UNIX-TIME-ACCEPTED-SEC
           DISPLAY "++++".
           DISPLAY " ".
      **** end of unix time stamps ************************************


      /---
       A-BEGIN.      

010314     DISPLAY "**** bb7935.cbl ****".

           MOVE "bb7935"     TO BE-PROG-NAME.
           MOVE "Starts ..." TO BE-COMMENT.
           MOVE SPACES       TO BE-REPLY.
           DISPLAY BE-DISPLAY.
           
           MOVE UNIX-DATE-ACCEPTED-NUM TO WS-DATE-NUM

           PERFORM B-OPENEN-BESTANDEN.    
           PERFORM SQL-CONNECT.     

           PERFORM D-CHECK-BOOKINGS.                   

           PERFORM X-EINDE.

      /---
       B-OPENEN-BESTANDEN.      


           IF BA-INPUT-FILE NOT = SPACES AND LOW-VALUES       
              MOVE BA-INPUT-FILE TO WS-BOOKING-CHECK
              OPEN INPUT BOOKING-CHECK
              IF WS-REPLY <> ZEROES
                 DISPLAY "** Error opening BOOKING-CHECK " WS-REPLY                         
                 PERFORM X-EINDE
              END-IF
           ELSE 
              CALL "datec2b"  USING BA-DATE-FROM WS-FROM-BIN
              CALL "datec2b"  USING BA-DATE-TO   WS-TO-BIN

              DISPLAY "DATE FROM to check: "   BA-DATE-FROM " - "
                      WS-FROM-BIN
              DISPLAY "DATE TO   to check: "   BA-DATE-TO " - "
                      WS-TO-BIN
           END-IF   

           
           MOVE "$DD_WORKDIR/bookings_price_checked.csv" TO 
                                                      WS-PRICE-JITS-FILE
           OPEN OUTPUT PRICE-JITS-FILE
           IF WS-REPLY <> ZEROES
              DISPLAY "** Error opening PRICE-JITS-FILE " WS-REPLY                         
              PERFORM X-EINDE
           ELSE
              INITIALIZE PRICE-JITS-FILE-TITLE
           
              STRING  "Bookingnumber;"
                      "Pax;"
                      "Revenue;"
                      "File Price;"
                      "Calculated Price;"
                      "Voucher price;"
                 DELIMITED BY SIZE 
                     INTO PRICE-JITS-FILE-TITLE
              WRITE PRICE-JITS-FILE-TITLE
           END-IF           
           MOVE SPACES TO PRICE-JITS-FILE-TITLE
      
o10510     OPEN INPUT JTINVOICE
o10510     IF WS-REPLY <> ZEROES
o10510        DISPLAY "** Error opening JTINVOICE " WS-REPLY
o10510        PERFORM X-EINDE
o10510     END-IF


020604     IF BA-FILE-SOURCE = "L"              
              MOVE "$DD_SYSFILES/JTEXPHOTELS"   TO WS-JTEXPHOTELS
              MOVE "$DD_SYSFILES/JTLONGSTAY"    TO WS-JTLONGSTAY
              MOVE "$DD_SYSFILES/JTVHOTELS"     TO WS-JTVHOTELS
              MOVE "$DD_SYSFILES/RECROOMD"      TO WS-RECROOMD
              MOVE "$DD_SYSFILES/JTVPRODUCTIP"  TO WS-JTVPRODUCTIP
              MOVE "$DD_SYSFILES/JTFAXMEMO"     TO WS-JTFAXMEMO
              MOVE "$DD_SYSFILES/RECHISTO"      TO WS-RECHISTO
              MOVE "$DD_SYSFILES/RECAIRPO"      TO WS-RECAIRPO
              MOVE "$DD_SYSFILES/NJAIRPORTTAX"  TO WS-NJAIRPORTTAX
              MOVE "$DD_SYSFILES/JTMEMO"        TO WS-JTMEMO
              MOVE "$DD_SYSFILES/RECFLIGH"      TO WS-RECFLIGH
              MOVE "$DD_SYSFILES/NJFLIGHTCOST"  TO WS-NJFLIGHTCOST
              MOVE "$DD_SYSFILES/JTVBDATEBOOKNR"
                                               TO WS-JTVBDATEBOOKNR
              MOVE "$DD_SYSFILES/JTVBOOKINGS"   TO WS-JTVBOOKINGS
              MOVE "$DD_SYSFILES/JTVAGENTS"     TO WS-JTVAGENTS
              MOVE "$DD_SYSFILES/JTGROUPNAMES"  TO WS-JTGROUPNAMES
              MOVE "$DD_SYSFILES/JTCLIENTS"     TO WS-JTCLIENTS
              MOVE "$DD_SYSFILES/JTMCHOTELS"    TO WS-JTMCHOTELS
              MOVE "$DD_SYSFILES/JTVFLIGHTS"    TO WS-JTVFLIGHTS
              MOVE "$DD_SYSFILES/JTACCOUNTS"    TO WS-JTACCOUNTS
              MOVE "$DD_SYSFILES/JTMCPRICE"     TO WS-JTMCPRICE
              MOVE "$DD_SYSFILES/JTFAXMEMO"     TO WS-JTFAXMEMO
              MOVE "$DD_SYSFILES/JTPRICE"       TO WS-JTPRICE
              MOVE "$DD_SYSFILES/JPROMO1"       TO WS-JPROMO1
              MOVE "$DD_SYSFILES/NJPROMO1"      TO WS-NJPROMO1
              MOVE "$DD_SYSFILES/RECPROM3"      TO WS-RECPROM3
              MOVE "$DD_SYSFILES/JTCOMMISS"     TO WS-JTCOMMISS
              MOVE "$DD_SYSFILES/eurocurrencyrates.csv"
                                                TO WS-EUROCURRENCYRATES
              MOVE "$DD_SYSFILES/JTDWH"         TO WS-JTDWH
              MOVE "$DD_SYSFILES/JTDIENSTREGELING"
                                                TO WS-JTDIENSTREGELING
              MOVE "$DD_SYSFILES/JTPOOFFER"     TO WS-JTPOOFFER

              MOVE "$DD_SYSFILES/NJPRODAUTOHTL" TO WS-NJPRODAUTOHTL
              MOVE "$DD_SYSFILES/NJPRODAUTOPRC" TO WS-NJPRODAUTOPRC
              MOVE "$DD_SYSFILES/JTPAXBVR"      TO WS-JTPAXBVR
              MOVE "$DD_SYSFILES/JTVMUNTKOERSEN" TO WS-JTVMUNTKOERSEN
              MOVE "$DD_SYSFILES/JTVBFLEXPRICE" TO WS-JTVBFLEXPRICE

           ELSE
              MOVE "$DD_BACKUP/JTEXPHOTELS"   TO WS-JTEXPHOTELS
              MOVE "$DD_BACKUP/JTLONGSTAY"    TO WS-JTLONGSTAY
              MOVE "$DD_BACKUP/JTVHOTELS"     TO WS-JTVHOTELS
              MOVE "$DD_BACKUP/RECROOMD"      TO WS-RECROOMD
              MOVE "$DD_BACKUP/JTVPRODUCTIP"  TO WS-JTVPRODUCTIP
              MOVE "$DD_BACKUP/JTFAXMEMO"     TO WS-JTFAXMEMO
              MOVE "$DD_BACKUP/JTMEMO"        TO WS-JTMEMO
              MOVE "$DD_BACKUP/RECFLIGH"      TO WS-RECFLIGH
              MOVE "$DD_BACKUP/NJFLIGHTCOST"  TO WS-NJFLIGHTCOST
              MOVE "$DD_BACKUP/RECHISTO"      TO WS-RECHISTO
              MOVE "$DD_BACKUP/RECAIRPO"      TO WS-RECAIRPO
              MOVE "$DD_BACKUP/NJAIRPORTTAX"  TO WS-NJAIRPORTTAX
              MOVE "$DD_BACKUP/JTVBDATEBOOKNR"
                                             TO WS-JTVBDATEBOOKNR
              MOVE "$DD_BACKUP/JTVBOOKINGS"   TO WS-JTVBOOKINGS
              MOVE "$DD_BACKUP/JTVAGENTS"     TO WS-JTVAGENTS
              MOVE "$DD_BACKUP/JTGROUPNAMES"  TO WS-JTGROUPNAMES
              MOVE "$DD_BACKUP/JTCLIENTS"     TO WS-JTCLIENTS
              MOVE "$DD_BACKUP/JTMCHOTELS"    TO WS-JTMCHOTELS
              MOVE "$DD_BACKUP/JTVFLIGHTS"    TO WS-JTVFLIGHTS
              MOVE "$DD_BACKUP/JTACCOUNTS"    TO WS-JTACCOUNTS
              MOVE "$DD_BACKUP/JTMCPRICE"     TO WS-JTMCPRICE
              MOVE "$DD_BACKUP/JTFAXMEMO"     TO WS-JTFAXMEMO
              MOVE "$DD_BACKUP/JTPRICE"       TO WS-JTPRICE
              MOVE "$DD_BACKUP/JPROMO1"       TO WS-JPROMO1
              MOVE "$DD_BACKUP/NJPROMO1"      TO WS-NJPROMO1
              MOVE "$DD_BACKUP/RECPROM3"      TO WS-RECPROM3
              MOVE "$DD_BACKUP/JTCOMMISS"     TO WS-JTCOMMISS
              MOVE "$DD_BACKUP/eurocurrencyrates.csv"
                                             TO WS-EUROCURRENCYRATES
              MOVE "$DD_BACKUP/JTDWH"         TO WS-JTDWH
              MOVE "$DD_BACKUP/JTDIENSTREGELING"
                                             TO WS-JTDIENSTREGELING
              MOVE "$DD_BACKUP/JTPOOFFER"     TO WS-JTPOOFFER
              MOVE "$DD_BACKUP/NJPRODAUTOHTL" TO WS-NJPRODAUTOHTL
              MOVE "$DD_BACKUP/NJPRODAUTOPRC" TO WS-NJPRODAUTOPRC
              MOVE "$DD_BACKUP/JTPAXBVR"      TO WS-JTPAXBVR
              MOVE "$DD_BACKUP/JTVMUNTKOERSEN" TO WS-JTVMUNTKOERSEN
              MOVE "$DD_BACKUP/JTVBFLEXPRICE" TO WS-JTVBFLEXPRICE
           END-IF


071019     OPEN INPUT JTTUBALL.
071019     IF WS-REPLY NOT = "00"
d01104        DISPLAY "**Error OPEN JTTUBALL " WS-REPLY
071019        PERFORM X-EINDE.

           OPEN INPUT RECROOMD.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open RECROOMD " WS-REPLY                      
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTVPRODUCTIP.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTVPRODUCTIP " WS-REPLY                      
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTVBDATEBOOKNR.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTVBDATEBOOKNR " WS-REPLY                      
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTMCPRICE.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTMCPRICE " WS-REPLY                      
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTFAXMEMO.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTFAXMEMO " WS-REPLY                      
              PERFORM X-EINDE
           END-IF

           OPEN INPUT NJPRODAUTOHTL.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open NJPRODAUTOHTL " WS-REPLY                       
              PERFORM X-EINDE
           END-IF.

           OPEN INPUT NJPRODAUTOPRC.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open NJPRODAUTOPRC " WS-REPLY                       
              PERFORM X-EINDE
           END-IF.

d80215     OPEN INPUT JTVMUNTKOERSEN
d80215     IF WS-REPLY NOT = "00"
d80215        DISPLAY "** Error open JTVMUNTKOERSEN " WS-REPLY
d80215        PERFORM X-EINDE
d80215     END-IF.

d80215     OPEN INPUT JTVBFLEXPRICE
d80215     IF WS-REPLY NOT = "00"
d80215        DISPLAY "** Error open JTVBFLEXPRICE " WS-REPLY
d80215        PERFORM X-EINDE
d80215     END-IF.

071214     OPEN INPUT JTPAXBVR.
071214     IF WS-REPLY NOT = "00"
071214        DISPLAY "** Error open JTPAXBVR " WS-REPLY
071214        PERFORM X-EINDE
071214     END-IF.

           OPEN INPUT JTPRICE.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTPRICE " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JPROMO1.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JPROMO1 " WS-REPLY
              PERFORM X-EINDE
           END-IF

h90903     OPEN INPUT NJPROMO1.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open NJPROMO1 " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT RECHISTO.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open RECHISTO " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT RECPROM3.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open RECPROM3 " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTMEMO.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTMEMO " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTLONGSTAY.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTLONGSTAY " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTCOMMISS.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTCOMMISS " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTDIENSTREGELING.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTDIENTSREGELING " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTVBOOKINGS.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTVBOOKINGS " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTVBOOKINGSOLD.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTVBOOKINGSOLD " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTVAGENTS
           IF WS-REPLY NOT = "00"
              DISPLAY "OPENEN VAN FILE JTVAGENTS FOUTIEF " WS-REPLY
              PERFORM X-EINDE
           END-IF.

           OPEN INPUT JTGROUPNAMES.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTGROUPNAMES " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTCLIENTS.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTCLIENTS " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTMCHOTELS.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTMCHOTELS " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTVFLIGHTS.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTVFLIGHTS " WS-REPLY
              PERFORM X-EINDE
           END-IF

051201     OPEN INPUT JTPOOFFER.
051201     IF WS-REPLY NOT = "00"
051201        DISPLAY "** Error open JTPOOFFER " WS-REPLY
051201        PERFORM X-EINDE
051201     END-IF

C60916     COPY "jtvhotels.open.cbl.cpy"
C60916     REPLACING ==:MODE:== BY ==INPUT== .
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTVHOTELS " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTACCOUNTS.
           IF WS-REPLY NOT = "00"
              DISPLAY "** Error open JTACCOUNTS " WS-REPLY
              PERFORM X-EINDE
           END-IF

           OPEN INPUT JTDWH.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open JTDWH " WS-REPLY
              PERFORM X-EINDE
           END-IF.

           OPEN INPUT RECFLIGH.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open RECFLIGH " WS-REPLY
              PERFORM X-EINDE
           END-IF.

060519     OPEN INPUT NJFLIGHTCOST.
060519     IF WS-REPLY NOT = ZEROES
060519        DISPLAY "** Error open NJFLIGHTCOST " WS-REPLY
060519        PERFORM X-EINDE
060519     END-IF.

           OPEN INPUT RECAIRPO.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error open RECAIRPO " WS-REPLY
              PERFORM X-EINDE
           END-IF.

060519     OPEN INPUT NJAIRPORTTAX.
060519     IF WS-REPLY NOT = ZEROES
060519        DISPLAY "** Error open NJAIRPORTTAX " WS-REPLY
060519        PERFORM X-EINDE
060519     END-IF.

030902     OPEN INPUT JTTUITICKETS.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error OPEN JTTUITICKETS " WS-REPLY
              PERFORM X-EINDE.

030902     OPEN INPUT JTTUITICKALL.
           IF WS-REPLY NOT = ZEROES
              DISPLAY "** Error OPEN JTTUITICKALL " WS-REPLY
              PERFORM X-EINDE.

041001     OPEN INPUT JTLHTAX.
041001     IF WS-REPLY NOT = ZEROES
041001        DISPLAY "OPEN JTLHTAX FAILES WITH " WS-REPLY
041001        PERFORM X-EINDE
041001     END-IF
           .     


      /---
       D-CHECK-BOOKINGS.

           IF BA-INPUT-FILE NOT = SPACES AND LOW-VALUES
              SET SW-END-OF-FILE-NO TO TRUE
              PERFORM R-READ-INPUT-BOOKINGS
              IF WS-INPUT-BOOKNR IS NOT NUMERIC
                 PERFORM R-READ-INPUT-BOOKINGS
              END-IF

              PERFORM UNTIL SW-END-OF-FILE-YES
                 MOVE ZEROS      TO WS-SALDO
                 MOVE ZEROS      TO WS-MODCALC-PRICE                 
                 PERFORM R-READ-JTVB               
                 IF WS-REPLY = ZEROES         
                    PERFORM GET-WB-BOOKING
                    PERFORM GET-WB-PRICE            
                    MOVE JTVB-SALDO TO WS-SALDO 
                    IF JTVB-BOOKING-TYPE = 4                     
                       PERFORM R-READ-JTMCPRICE                       
                    ELSE
                       PERFORM R-CALL-MODCALC                
                    END-IF   
                 END-IF               
                 PERFORM R-WRITE-OUTPUT-FILE
                 PERFORM R-READ-INPUT-BOOKINGS               
              END-PERFORM
           ELSE
              INITIALIZE JTVB-RECORD
              MOVE ZEROS TO JTVB-BOOKNR9
              MOVE 99    TO JTVB-SUFFIX              
              PERFORM UNTIL WS-REPLY = 10 OR 46    
                 MOVE ZEROS      TO WS-SALDO
                 MOVE ZEROS      TO WS-MODCALC-PRICE                
                 READ JTVBOOKINGS NEXT
                 IF WS-REPLY = ZEROES AND JTVB-SUFFIX = 99                            
                    IF JTVB-H1-FROM >= WS-FROM-BIN AND 
                       JTVB-H1-FROM <= WS-TO-BIN
                       PERFORM GET-WB-BOOKING
                       PERFORM GET-WB-PRICE
                       MOVE JTVB-SALDO TO WS-SALDO
                       IF JTVB-BOOKING-TYPE = 4
                          PERFORM R-READ-JTMCPRICE
                       ELSE
                          PERFORM R-CALL-MODCALC
                       END-IF
                       MOVE JTVB-BOOKNR9 TO WS-INPUT-BOOKNR
                       MOVE ZEROS        TO WS-INPUT-PAX
                                            WS-INPUT-PRICE
                       PERFORM R-WRITE-OUTPUT-FILE                                                    

                       ADD 1 TO TOT-INPUT-BOOKINGS
                       ON 1 AND EVERY 500
                       DISPLAY "Bookings read so far: " 
                               TOT-INPUT-BOOKINGS " - " WS-INPUT-BOOKNR
                    END-IF                               
                 END-IF                 
              END-PERFORM   
           END-IF   
           .      
       

      /---
       R-CALL-MODCALC.
           
           INITIALIZE     L02-PRINT-LINES
                          L04-RESPONSES
                          L05-PROMOTIONS
                          L03-PARAMETERS
           MOVE "N"    TO L03-TRACER
                          L03-COST-AMEND
           MOVE 1      TO L03-CALL-TYPE
           MOVE ZEROS  TO L03-FORCE-SEASON

  
           INITIALIZE JTFP-FRECORD.
           MOVE JTVB-BOOKNR9   TO JTFP-FBOOKNR9
           MOVE JTVB-SUFFIX    TO JTFP-FSUFFIX
           READ JTVBFLEXPRICE
           IF WS-REPLY = ZEROES
              MOVE JTFP-FYIELD-RED   TO L03-YIELD-RED
              MOVE JTFP-FFLIGHT-RED  TO L03-FLIGHT-RED
              MOVE JTFP-FHOTEL-RED   TO L03-HOTEL-RED
           END-IF.

           INITIALIZE REDPO-RECORD

           EVALUATE JTVB-LANGUAGE
              WHEN 1
                 MOVE "NL" TO REDPO-LANGUAGE
              WHEN 2
                 MOVE "FR" TO REDPO-LANGUAGE
              WHEN OTHER
                 MOVE "EN" TO REDPO-LANGUAGE
           END-EVALUATE

           MOVE JTVB-BOOKNR9         TO REDPO-BOOKNR

           MOVE ZEROS                TO REDPO-VERSION

           CALL "modredpo"       USING REDPO-INPUT-DATA
                                       REDPO-OUTPUT-DATA
           IF REDPO-MESSAGE-CODE = ZEROES
              IF JTVB-BOOKING-TYPE <> 4
                 PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL
                    (WS-IND > 5)  OR
                    (REDPO-REDUCTION-CODE(WS-IND) = SPACES OR 
                    LOW-VALUES) 
                    MOVE REDPO-REDUCTION-CODE(WS-IND)  TO 
                                                    L03-RED-CODE(WS-IND)
                    MOVE REDPO-REDUCTION-DESCRIPTION(WS-IND) TO 
                                                    L03-RED-DESC(WS-IND)
                    MOVE REDPO-REDUCTION-AMOUNT(WS-IND) TO
                                                  L03-RED-AMOUNT(WS-IND)
                    MOVE REDPO-REDUCTION-TYPE(WS-IND) TO 
                                                    L03-RED-TYPE(WS-IND)
                 END-PERFORM                                     
              END-IF
           END-IF

           IF JTVB-USER-NO = 3              
              CALL WS-CALL-MODVIPUPGROOM USING JTVB-RECORD
                                               L02-PRINT-LINES
                                               L03-PARAMETERS
                                               L04-RESPONSES
                                               L05-PROMOTIONS
                                               LK-MVUR-OUT              
              
              IF LK-MVUR-OUT-CONTINUE-YES                 
                 MOVE LK-MVUR-OUT-ROOM     TO L03-VIP-UPGR-ROOM
                 MOVE LK-MVUR-OUT-DISCOUNT TO L03-VIP-UPGR-DIFF-PRICE
                 MOVE LK-MVUR-OUT-DEDUCT-FROM-ACCO
                                             TO L03-VIP-UPGR-DEDUCT-SUPP
              END-IF
           END-IF
           
           CANCEL "modcalc"
           CALL "modcalc" USING  JTVB-RECORD 
                                 L02-PRINT-LINES                         
                                 L03-PARAMETERS
                                 L04-RESPONSES
                                 L05-PROMOTIONS
           
          
           
           PERFORM VARYING WS-Q FROM 1 BY 1 
                   UNTIL WS-Q > 72 OR L02-LINE-NO(WS-Q) = 40                    
                   ADD L02-PRICE(WS-Q) TO WS-MODCALC-PRICE                    
           END-PERFORM                              
           
           .

       R-READ-JTMCPRICE.
                      
           MOVE SPACES        TO JTMC-KEY
           MOVE JTVB-BOOKNR9  TO JTMC-BOOKNR9
           MOVE JTVB-SUFFIX   TO JTMC-SUFFIX          

           READ JTMCPRICE  
           IF WS-REPLY = ZEROES              
              PERFORM VARYING WS-Q FROM 1 BY 1
                 UNTIL WS-Q > 200 OR WS-Q > JTMC-NUMBER-OF-LINES
                 MOVE ZEROS TO X
                 MOVE FUNCTION UPPER-CASE(JTMC-LINE(WS-Q)) TO 
                                                       WS-CHECK-VOUCHER
                 INSPECT WS-CHECK-VOUCHER TALLYING X 
                         FOR ALL "GIFT VOUCHER"
                 IF X > 0 
                    CONTINUE
                 ELSE
                    ADD      JTMC-S-VALUE(WS-Q)   TO WS-MODCALC-PRICE   
                 END-IF                   
              END-PERFORM               
           ELSE              
              DISPLAY "** Warning, booking is not read in JTMCPRICE: "
                      WS-REPLY " - " JTMC-BOOKNR9              
           END-IF


           .    

      
       R-READ-INPUT-BOOKINGS.
           
           READ BOOKING-CHECK NEXT            
           IF WS-REPLY = ZEROES             
              
              MOVE ZEROS   TO WS-INPUT-BOOKNR
              MOVE SPACES  TO WS-INPUT-PAX
                              WS-INPUT-PRICE
              
              UNSTRING BOOKING-CHECK-RECORD DELIMITED BY ";"
                  INTO WS-INPUT-BOOKNR
                       WS-INPUT-PAX
                       WS-INPUT-PRICE                                     
              
             
              IF WS-INPUT-BOOKNR IS NUMERIC
                 ADD 1 TO TOT-INPUT-BOOKINGS
                 ON 1 AND EVERY 500
                    DISPLAY "Bookings read so far: "
                            TOT-INPUT-BOOKINGS
                            " - " WS-INPUT-BOOKNR
              END-IF                                   

           ELSE
              IF WS-REPLY = 10 OR 46 
                 SET SW-END-OF-FILE-YES TO TRUE
              ELSE
                 DISPLAY "** Error read input file: " WS-REPLY
                 PERFORM X-EINDE
              END-IF   
           END-IF
           .

      /---
       R-READ-JTVB.
           INITIALIZE JTVB-RECORD
           MOVE WS-INPUT-BOOKNR TO JTVB-BOOKNR9
           MOVE 99              TO JTVB-SUFFIX
           READ JTVBOOKINGS           
           IF WS-REPLY <> ZEROES
              DISPLAY "** Warning: could not read JTVB: " 
                      JTVB-BOOKNR9 "  WS-REPLY: "    WS-REPLY              
           
           END-IF

           .

      /---
       R-WRITE-OUTPUT-FILE.         
                      
           INITIALIZE PRICE-JITS-FILE-RECORD
           MOVE ALL ";"               TO PRICE-JITS-FILE-RECORD
           MOVE WS-INPUT-BOOKNR       TO WS-OUTPUT-BOOKNR
           MOVE WS-INPUT-PAX          TO WS-OUTPUT-PAX           
           MOVE WS-INPUT-PRICE        TO WS-OUTPUT-DWH-PRICE           
           MOVE WS-SALDO              TO WS-OUTPUT-FILE-PRICE
           MOVE WS-MODCALC-PRICE      TO WS-OUTPUT-MODCALC-PRICE
           MOVE WS-WB-PRICE           TO WS-OUTPUT-WB-PRICE


           WRITE PRICE-JITS-FILE-RECORD
           IF WS-REPLY NOT = ZEROES              
              DISPLAY "** Error writing PRICE-JITS-FILE "
                             WS-REPLY " - " WS-OUTPUT-BOOKNR              
           ELSE
              ADD 1 TO TOT-OUTPUT-BOOKING
           END-IF
           .          

      /---
       GET-WB-BOOKING.

           INITIALIZE TBCW-CURRENT-WAARDEBONNEN
           MOVE ZEROS TO WS-IND-WB
           MOVE JTVB-BOOKNR9          TO WS-BOOK-NUM
           MOVE WS-BOOK-ALF           TO WS-PO-GET-POUSER
           
                      
           IF WS-PO-GET-POUSER NOT = SPACES
               
              exec sql
                 DECLARE C-GET-SQL-WAARDEBONNEN CURSOR FOR
                 CALL JAROS.sp_get_waardebonnen_for_booknr
                      ( :WS-PO-GET-POUSER )
              end-exec

              *> open cursor
              exec sql
                 OPEN C-GET-SQL-WAARDEBONNEN
              end-exec

              PERFORM UNTIL SQLCODE = 100 OR WS-IND-WB = 10
                 *> get data from cursor
                 exec sql
                    FETCH C-GET-SQL-WAARDEBONNEN INTO
                       :SQL-WB-DEPTUITGEVER,       
                       :SQL-WB-YEAR,               
                       :SQL-WB-NUMBER,             
                       :SQL-WB-STATUS,             
                       :SQL-WB-DATE,               
                       :SQL-WB-BROCHURE,           
                       :SQL-WB-ACTIVATIONCODE,     
                       :SQL-WB-WAARDE,             
                       :SQL-WB-DEELWAARDE,         
                       :SQL-WB-CURRENCY,           
                       :SQL-WB-CURRENCY-AMOUNT,    
                       :SQL-WB-CURRENCY-DEELWAARDE,
                       :SQL-WB-CRISIS-REFERENCE,   
                       :SQL-WB-LK-DEPTUITGEVER,    
                       :SQL-WB-LK-YEAR,            
                       :SQL-WB-LK-NUMBER         
                 end-exec                                
                 
                    
                 IF SQLCODE = ZEROES OR 1

                    ADD 1                    TO WS-IND-WB

                    INSPECT SQL-WB-DEELWAARDE REPLACING ALL '.' BY ','
                    MOVE FUNCTION NUMVAL(SQL-WB-DEELWAARDE)   
                                       TO TBCW-WAARDE(WS-IND-WB)                    
                 END-IF

              END-PERFORM

              *> close cursor
              exec sql
                 CLOSE C-GET-SQL-WAARDEBONNEN
              end-exec

           END-IF.

       GET-WB-PRICE.           
           MOVE ZEROS TO WS-WB-PRICE
           PERFORM VARYING WS-IND FROM 1 BY 1 
              UNTIL WS-IND > WS-IND-WB OR (TBCW-WAARDE(WS-IND) = ZEROES
              OR LOW-VALUES)                 
              COMPUTE TBCW-WAARDE(WS-IND) = TBCW-WAARDE(WS-IND) * -1
              ADD TBCW-WAARDE(WS-IND) TO WS-WB-PRICE

           END-PERFORM
           .           


x90112 SQL-CONNECT.

b41125     DISPLAY "** Using new ODBC connection MYSQL5"
d40324     exec sql
b41125        CONNECT TO MYSQL5
d40324     end-exec
d40324     IF SQLCODE NOT = 0
d40324        DISPLAY "cannot connect " SQLCODE " - " SQLERRMC
d40324        PERFORM X-EINDE
d40324     END-IF
           .       

x90112 SQL-DISCONNECT.

           exec sql
              DISCONNECT CURRENT
           end-exec
           IF SQLCODE NOT = 0
               IF SQLCODE = -2013 OR -2006
                  CONTINUE
               ELSE
                  DISPLAY "++++Error: cannot disconnect - " SQLCODE
                  DISPLAY SQLERRMC
                  STOP RUN
               END-IF
           END-IF
           .       

      /---
       Y-SEND-FILE.     

           MOVE SPACES                 TO CONF-VAR                                                 
           MOVE SPACES                 TO CONF-VALUE                                                
           MOVE "ODRIVE"               TO CONF-VAR                                   
           CALL "config_get_value"  USING CONF-VAR                                  
                                          CONF-VALUE                                           
 
           MOVE SPACES                 TO BA-COMMAND 
           STRING "<ntrcp " WS-PRICE-JITS-FILE  " "
                  CONF-VALUE DELIMITED BY "  "                 
                  "AFI0/Output/"                                   
                  DELIMITED BY SIZE
              INTO BA-COMMAND
           PERFORM Z-CALL-PIPE
           .

      /---
       X-EINDE.       
      
           PERFORM SQL-DISCONNECT
           
           DISPLAY "****".
           DISPLAY "Total bookings read. . . : "  TOT-INPUT-BOOKINGS
           DISPLAY "Total bookings written . : "  TOT-OUTPUT-BOOKING        


           CLOSE PRICE-JITS-FILE.
           CLOSE BOOKING-CHECK.
           CLOSE RECROOMD        JTVPRODUCTIP.
           CLOSE JTVBOOKINGS.
           CLOSE JTVBOOKINGSOLD.
           CLOSE JTGROUPNAMES.
           CLOSE JTACCOUNTS.
           CLOSE JTVAGENTS       JTMCHOTELS.
           CLOSE JTCLIENTS       JTVBDATEBOOKNR.
C60927     COPY "jtvhotels.close.cbl.cpy".
C60927     CLOSE JTVFLIGHTS.      
o10510     CLOSE JTINVOICE.
           CLOSE JTMCPRICE.
           CLOSE JTFAXMEMO.
071214     CLOSE NJPRODAUTOHTL.
071214     CLOSE NJPRODAUTOPRC.      
           CLOSE JTPRICE.
           CLOSE JPROMO1.
h90903     CLOSE NJPROMO1.
           CLOSE RECPROM3.
           CLOSE JTVPRODUCTIP.
           CLOSE JTLONGSTAY.
           CLOSE JTCOMMISS.
           CLOSE JTDIENSTREGELING.
           CLOSE RATES.
           CLOSE JTDWH JTMEMO.
           CLOSE RECFLIGH.
060519     CLOSE NJFLIGHTCOST
060519     CLOSE NJAIRPORTTAX
           CLOSE RECAIRPO.
           CLOSE RECHISTO.
030902     CLOSE JTTUITICKETS JTTUITICKALL.
041001     CLOSE JTLHTAX.
           CLOSE JTPOOFFER.
070124     CLOSE ERRFILET.
071019     CLOSE JTTUBALL.
d80215     CLOSE JTVMUNTKOERSEN
d80215     CLOSE JTVBFLEXPRICE                      
           CLOSE LISTMODCALC
           CLOSE JTTRANS1
a50831     CALL "modjitsseasclose"

           PERFORM Y-SEND-FILE

           DISPLAY "++++ normal end of cbl - bb7935.cbl".
           EXIT PROGRAM.
           STOP RUN.
           .

           Z-CALL-PIPE.
           MOVE 2                      TO BA-PIPE-REPLY

           CALL "cblpipe" USING BA-COMMAND
                                BA-PIPE-REPLY

           IF ( BA-PIPE-REPLY NOT = 0 )
              MOVE BA-PIPE-REPLY TO BA-DISP-2
              DISPLAY "++++ REPLY " BA-DISP-2 " CALLING PIPE"
           END-IF
           .