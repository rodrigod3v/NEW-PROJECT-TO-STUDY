      ******************************************************************
      * Author:Rodrigo Orlando
      * Date:23/10/2022
      * Purpose:CONSULTA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSULTAR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                SELECT ALUNOS ASSIGN TO
                'C:\Users\777\Desktop\BOLETIMV2\ALUNOS.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS RANDOM
                RECORD KEY IS ID-ALUNO
                FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ALUNOS.
           COPY DATABASE.

       WORKING-STORAGE SECTION.
         01 WS-REGISTRO                     PIC X(32) VALUE SPACES.
         01 FILLER REDEFINES WS-REGISTRO.
           03 WS-ID-ALUNO                 PIC 9(02).
           03 WS-NM-ALUNO                 PIC X(30).
         77 WS-FS                                   PIC 99.
           88 FS-OK                                VALUE 0.
         77 WS-EOF                          PIC X.
           88 EOF-OK                        VALUE 'S' FALSE 'N'.
         77 WS-EXIT                         PIC X     VALUE 'N'.
           88 EXIT-OK                       VALUE 'N' FALSE 'S'.

       LINKAGE SECTION.
         01 LK-COM-AREA.
           03 LK-MENSAGEM                        PIC X(20).

       PROCEDURE DIVISION USING LK-COM-AREA.

       MAIN-PROCEDURE.
           DISPLAY '*** CONSULTAR STATUS DO ALUNO ***'
           SET EXIT-OK               TO FALSE
           PERFORM P300-CONSULTA     THRU P300-FIM UNTIL EXIT-OK
           PERFORM P900-FIM
           .

       P300-CONSULTA.
           SET EOF-OK               TO FALSE
           SET FS-OK                TO TRUE

           OPEN INPUT ALUNOS


           IF FS-OK  THEN
               DISPLAY 'INFORME O ID DO ALUNO :'
               ACCEPT ID-CONTATO


            READ ALUNOS INTO WS-REGISTRO
              KEY IS ID-CONTATO
                  INVALID KEY
                  DISPLAY 'ALUNO NAO REGISTRADO'
                  NOT INVALID KEY
                  DISPLAY WS-ID-ALUNO ' - ' WS-NM-ALUNO
           END-READ
           ELSE
            DISPLAY 'ERRO AO LER A DATABASE ALUNOS'
            DISPLAY 'FILE STATUS: ' WS-FS
            END-IF

            CLOSE ALUNOS

           DISPLAY 'DESEJA CONTINUAR? <S> - <N>'
           ACCEPT WS-EXIT

           .


       P300-FIM.
       P900-FIM.


           GOBACK.
       END PROGRAM CONSULTAR.
