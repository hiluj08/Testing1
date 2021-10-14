       IDENTIFICATION DIVISION.
      ****************************************************************
      * Program-ID.....: rb7935
      * Author.........: jluque01
      * Date Written.. : 22/10/2020
      * Last Updated...: 22/10/2020 at 10:39:32 by jluque01
      * Function.......: Start for bb7935
      *****************************************************************
     /*/Description ***************************************************
     /* Start for bb7935
     /*
     /*
     /*/End-description ***********************************************
      * Input-file.....:
      * I-O-file.......:
      * Output-file....:
      ****************************************************************

      ****************************************************************
       PROGRAM-ID.                rb7935.
       AUTHOR.                    jluque01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. UNIX-SVR4.
       OBJECT-COMPUTER. UNIX-SVR4.

       SPECIAL-NAMES.

          CRT STATUS IS BA-CRT-STATUS
          CURSOR IS BA-CURSOR.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "jobparam.sel".

       DATA DIVISION.

       FILE SECTION.

       COPY "jobparam.cpy".

       WORKING-STORAGE SECTION.

       01 WS-NAME              PIC X(20).
       01 HULP-DAG             PIC 9(02).
       01 WS-FILE-NAME1        PIC X(35).
       01 WS-FAX-NAME          PIC X(35).

       01 WS-PROG-DESC         PIC X(38)
                               VALUE
                               "Making of RECHISTODWH".
       01 WS-PROG-NAME         PIC X(07)
                               VALUE "rb7935 ".
       01 B0-PROG-DESC         PIC X(38)
                               VALUE
                               "Making of RECHISTODWH".
       01 B0-PROG-NAME         PIC X(07)
                               VALUE "rb7935 ".

       01 WS-VALUE             PIC X(03).
       01 WS-REPLY             PIC X(02).

       01 WS-TODAY                         PIC 9(8).
       01 WS-BIN-TODAY                     PIC 9(9)   COMP SYNC RIGHT.
       01 WS-BINDATE                       PIC 9(9)   COMP SYNC RIGHT.

       01 WS-DATA.
          03 WS-DATE.
             05 WS-CC                      PIC 99.
             05 WS-YYMMDD                  PIC 9(6).
             05 REDEFINES WS-YYMMDD.
                07 WS-YY                   PIC 99.
                07 WS-MM                   PIC 99.
                07 WS-DD                   PIC 99.
          03 WS-DATE-NUM                   PIC 9(8)   REDEFINES WS-DATE.
          03                                          REDEFINES WS-DATE.
             05 WS-CCYY                    PIC 9999.
             05 WS-MMDD                    PIC 9999.

       01 WS-DATA-INPUT.
           03 WS-DATE-INPUT.
              05 WS-DDMMYY                 PIC 9(6).
              05 REDEFINES WS-DDMMYY.
                 07 WS-DD-INPUT            PIC 99.
                 07 WS-MM-INPUT            PIC 99.
                 07 WS-YY-INPUT            PIC 99.

       COPY "wsl80.cpy".
       COPY "y2kdata.cpy".

       COPY "wsbatch1.cpy".
       01 REDEFINES BA-USER-PARAMS.
          03 BA-7935-INPUT-FILE    PIC X(30).
          03 BA-7935-DATE-FROM     PIC 9(8).
          03 BA-7935-DATE-TO       PIC 9(8).
          03 BA-7935-OK            PIC X.
          03 BA-7935-FILE-SOURCE   PIC X.
          
              

       SCREEN SECTION.

      ****************************************************
      * Change the name of the screen or insert the screen
      ****************************************************

       01 SCR-SELECT-SCREEN.
          03 SCR-CONSTANTS.
             05 LINE 2  COL 30
                VALUE "Extract JITS price".
             05 LINE 2  COL 74  VALUE "/".
             05 LINE 2  COL 77  VALUE "/".
             05 LINE 3  COL 77  VALUE ":".
             05 LINE 8  COL 15  VALUE "Choose one option:".
             05 LINE 10 COL 15  VALUE "- Input File : ".
             05 LINE 10 COL 45  
                               VALUE "[______________________________]".
             05 LINE 11 COL 15  VALUE "  Or ".
             05 LINE 12 COL 15  VALUE "- Insert book date period : ".
             05 LINE 13 COL 17  VALUE "DATE FROM : ".
             05 LINE 13 COL 45  VALUE "[________]  (YYYYMMDD)".
             05 LINE 14 COL 17  VALUE "DATE TO   :".
             05 LINE 14 COL 45  VALUE "[________]  (YYYYMMDD)". 
             05 LINE 18 COL 15  VALUE "- File source:".
             05 LINE 19 COL 17
                VALUE "L->data, T->backup. . . . : [.]".

             05 LINE 21 COL 17
                VALUE "Parameters OK . . . . . . : [.]".

          03 SCR-VARAIBLES.
             05 LINE 2  COL 2            PIC X(06)
                                         FROM WS-TUI-COMPANY-NAME.
             05 LINE 2  COL 72           PIC X(02) FROM  BA-DATE-DD.
             05 LINE 2  COL 75           PIC X(02) FROM  BA-DATE-MM.
             05 LINE 2  COL 78           PIC X(02) FROM  BA-DATE-YY.
             05 LINE 3  COL 2            PIC X(15) FROM  BE-PROG-NAME.
             05 LINE 3  COL 75           PIC X(02) FROM  BA-TIME-HH.
             05 LINE 3  COL 78           PIC X(02) FROM  BA-TIME-MM.
             05 LINE 10 COL 46 HIGHLIGHT PIC X(30) 
                                         USING  BA-7935-INPUT-FILE.
             05 LINE 13 COL 46 HIGHLIGHT PIC 9(8)     
                                         USING  BA-7935-DATE-FROM.
             05 LINE 14 COL 46 HIGHLIGHT PIC 9(8)     
                                         USING  BA-7935-DATE-TO.                                         
             05 LINE 19 COL 46 HIGHLIGHT PIC X
                                         USING  BA-7935-FILE-SOURCE.
             05 LINE 21 COL 46 HIGHLIGHT PIC X
                                         USING  BA-7935-OK.

       PROCEDURE DIVISION.

           DISPLAY "++++ Last Amended: 10/02/2017 10:39:32 dviers02".


      ************************************************************
      * Main controle                                            *
      ************************************************************
       A-MAIN-CONTROL SECTION.
       A-000.
      ************************************************************
      *                                                          *
      * Direct program depending on run type                     *
      *                                                          *
      * run-type -s : 1. get jobnumber                           *
      *               2. handle screen and validate input        *
      *               3. save parameters in parameter file       *
      *                                                          *
      * run-type -e : 1. read params (created by -s or manualy)  *
      *               2. execute program                         *
      *                                                          *
      * run-type -i : 1. handle screen and validate input        *
      *               2. execute program                         *
      *                                                          *
      ************************************************************

           ACCEPT BA-ARGUMENTS FROM COMMAND-LINE.
           MOVE WS-PROG-NAME TO BE-PROG-NAME.

           COPY "rbbatcha100.cbl.cpy".

      /---
       B-GET-PARAMS-FROM-SCREEN SECTION.
       B-000-DEFAULT.

      * Initialize params
           MOVE SPACES TO BA-USER-PARAMS.

      * Set default values
           MOVE SPACES             TO BA-7935-INPUT-FILE.           
           MOVE "Y"                TO BA-7935-OK.
           MOVE "L"                TO BA-7935-FILE-SOURCE.

           MOVE BA-DATE-DD         TO WS-DD.
           MOVE BA-DATE-MM         TO WS-MM.
           MOVE BA-DATE-YY         TO WS-YY.

           MOVE WS-YYMMDD          TO Y2K-IN-YYMMDD
           MOVE "Y"                TO Y2K-IN-TYPE-DATE.
           CALL "datey2k"          USING Y2K-IN-YYMMDD
                                         Y2K-IN-TYPE-DATE
                                         Y2K-OUT-CCYYMMDD
                                         Y2K-REPLY
                                         Y2K-BINDATE.
           MOVE Y2K-BINDATE        TO WS-BIN-TODAY
           MOVE Y2K-BINDATE        TO WS-BINDATE
           MOVE Y2K-OUT-CCYYMMDD   TO WS-TODAY
            
           MOVE Y2K-BINDATE  TO WS-BINDATE
      
           CALL "dateb2c"    USING WS-BINDATE
                                   WS-DATE

           MOVE WS-DD        TO WS-DD-INPUT
           MOVE WS-MM        TO WS-MM-INPUT
           MOVE WS-YY        TO WS-YY-INPUT

           MOVE WS-TODAY     TO BA-7935-DATE-FROM
           MOVE WS-TODAY     TO BA-7935-DATE-TO

           CALL "box1".
           CALL "dfuncs".

       B-100-DISPLAY.

           DISPLAY SCR-SELECT-SCREEN.

           MOVE 1028 TO BA-CURSOR.

       B-100-ACCEPT.

           ACCEPT SCR-SELECT-SCREEN.

      * Clear message line

           DISPLAY BA-CLEAR-LINE AT 2501.

      * Continue if retrun/action/enter pressed

           IF BA-CRT-STAT-1 = 0
              GO TO B-200-CHECK-PARAMS
           END-IF.


      * Redisplay if fuction-key 2

           IF BA-CRT-STAT-1 = 1
              IF BA-CRT-STAT-2 = 2
                 GO TO B-100-DISPLAY
              END-IF
           END-IF.

      * Abort program when fuction-key 6

           IF BA-CRT-STAT-1 = 1
              IF BA-CRT-STAT-2 = 6
                 DISPLAY SPACES AT 0101
                 DISPLAY "Job abandoned on user request"
                 PERFORM X-EINDE
              END-IF
           END-IF.

      * Message 'cause something went wrong

           DISPLAY "Invalid key pressed"
                    LINE 25
                    COL 1
           END-DISPLAY.

           GO TO B-100-DISPLAY.

       B-200-CHECK-PARAMS.

      * Now check parameters

      *     TRANSFORM BA-USER-PARAMS FROM BA-LOWER TO BA-UPPER.


           IF BA-7935-INPUT-FILE = SPACES OR LOW-VALUES
              IF (BA-7935-DATE-FROM IS NOT NUMERIC OR
                  BA-7935-DATE-FROM =  ZEROS)   OR 
                 (BA-7935-DATE-TO   IS NOT NUMERIC OR 
                  BA-7935-DATE-FROM= ZEROS)
                 DISPLAY "If Input File is empty, date should be filled"                         
                         LINE 25
                         COL 1
                 END-DISPLAY
                 GO TO B-100-DISPLAY                              
              END-IF
           ELSE
              NEXT SENTENCE                 
           END-IF

           IF BA-7935-OK = "Y"
              NEXT SENTENCE
           ELSE
              DISPLAY "Submit job has to be Y"
                       LINE 25
                       COL 1
              END-DISPLAY
              GO TO B-100-DISPLAY
           END-IF.


           IF BA-7935-FILE-SOURCE = "L" OR "T"
              NEXT SENTENCE
           ELSE
              DISPLAY "Allowed values L or T"
                       LINE 25
                       COL 1
              END-DISPLAY
              GO TO B-100-DISPLAY
           END-IF

           COPY "rbbatchb300.cbl.cpy".

      /---
       E-EXECUTE SECTION.
       E-000.

           MOVE SPACES TO BA-USER-MESSAGEGROUP
                          BA-PRINT-FILE-NAMEGROUP.

           MOVE "bb7935          "  TO BA-USER-PROG(1).
           MOVE "copy            "  TO BA-USER-PROG(2).
           MOVE "end of cbl     "   TO BA-USER-PROG(3).

       E-500-PROG1.

           CALL "bb7935".

           DISPLAY "++++ normal end of cbl - rb7935 ".

       E-999-EXIT. EXIT.
      /---
       S-SAVE-PARAMS SECTION.

           COPY "rbbatchs000.cbl.cpy".

       S-100-MOVE-PARAMS.

           MOVE WS-PROG-DESC   TO JOBPARAM-PROG-DESCR.
           MOVE BA-USER-PARAMS TO JOBPARAM-USER-PARAMS.
           MOVE "8"            TO JOBPARAM-TTL.

           COPY "rbbatchend.cbl.cpy".

       Z-LAST-LABEL.
