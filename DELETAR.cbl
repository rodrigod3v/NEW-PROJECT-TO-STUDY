      ******************************************************************
      * Author:Rodrigo Orlando
      * Date:23/10/2022
      * Purpose:EXCLUIR CONTATO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELETAR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
                SELECT CONTATOS ASSIGN TO
                'C:\Users\777\Desktop\BOLETIMV2\ALUNOS.DAT'
                ORGANISATION IS INDEXED
                ACCESS MODE IS RANDOM
                RECORD KEY IS ID-CONTATO
                FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD CONTATOS.
           COPY DATABASE.

       WORKING-STORAGE SECTION.
         01 WS-REGISTRO                     PIC X(32) VALUE SPACES.
         01 FILLER REDEFINES WS-REGISTRO.
           03 WS-ID-CONTATO                 PIC 9(02).
           03 WS-NM-CONTATO                 PIC X(30).
         77 WS-FS                                   PIC 99.
           88 FS-OK                                VALUE 0.
         77 WS-EOF                          PIC X.
           88 EOF-OK                        VALUE 'S' FALSE 'N'.
         77 WS-EXIT                         PIC X     VALUE 'N'.
           88 EXIT-OK                       VALUE 'N' FALSE 'S'.
         77 WS-CONFIRM                      PIC X VALUE SPACES.

       LINKAGE SECTION.
         01 LK-COM-AREA.
           03 LK-MENSAGEM                        PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.

       MAIN-PROCEDURE.
           DISPLAY LK-MENSAGEM
           SET EXIT-OK               TO FALSE
           PERFORM P300-EXCLUIR     THRU P300-FIM UNTIL EXIT-OK
           PERFORM P900-FIM
           .

       P300-EXCLUIR.
           SET EOF-OK               TO FALSE
           SET FS-OK                TO TRUE

           MOVE SPACES             TO WS-CONFIRM

           OPEN I-O CONTATOS


           IF FS-OK  THEN
               DISPLAY 'INFORME O NUM ID DO CONTATO :'
               ACCEPT ID-CONTATO

            READ CONTATOS INTO WS-REGISTRO
              KEY IS ID-CONTATO
              INVALID KEY
                  DISPLAY 'CONTATO NAO EXISTE'
              NOT INVALID KEY
                  DISPLAY 'NOME ATUAL: '  WS-NM-CONTATO
                  DISPLAY 'TECLE : '
                  DISPLAY '<S> PARA CONFIRMAR OU <N>'
                  DISPLAY 'PARA CONTINUAR COM O ATUAL.. '
                  ACCEPT WS-CONFIRM
                  IF WS-CONFIRM EQUAL 'S' THEN
                  DELETE CONTATOS RECORD
                  DISPLAY 'CONTATO EXCLUIDO COM SUCESSO...'
                  ELSE
                  DISPLAY 'EXCLUSAO FALHOU...'
                  END-IF
           END-READ
           ELSE
            DISPLAY 'ERRO AO ABRIR O ARQUIVO DE CONTATOS'
            DISPLAY 'FILE STATUS: ' WS-FS
            END-IF

            CLOSE CONTATOS

           DISPLAY 'DESEJA CONTINUAR? <S> - <N>'
           ACCEPT WS-EXIT

           .


       P300-FIM.
       P900-FIM.


           GOBACK.
       END PROGRAM DELETAR.
