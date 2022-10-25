      ******************************************************************
      * Author:Rodrigo Orlando
      * Date:23/10/2022
      * Purpose:CADASTRO
      * Tectonics: cobc
      * UPDATE: 24/10/2022 CADASTRO DE ALUNOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADALUNO.

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
           03 WS-ID-ALUNO                        PIC 99.
           03 WS-NM-ALUNO                        PIC X(20).
           03 WS-TEL-ALUNO                       PIC 9(20).
           03 WS-ID-MATERIA                      PIC 9(20).
           03 WS-NM-MATERIA                      PIC X(20).
           03 WS-NT-PORT                         PIC 9(02)V99.
           03 WS-NT-MAT                          PIC 9(02)V99.
           03 WS-NT-CIEN                         PIC 9(02)V99.
           03 WS-MEDIA                           PIC 9(02)V99.
           03 WS-ST-APROVACAO                    PIC X(20).

         77 WS-FS                                   PIC 99.
           88 FS-OK                                VALUE 0.
         77 WS-EOF                          PIC X.
           88 EOF-OK                        VALUE 'S' FALSE 'N'.
         77 WS-EXIT                         PIC X     VALUE 'N'.
           88 EXIT-OK                       VALUE 'N' FALSE 'S'.


       LINKAGE SECTION.
         01 LK-COM-AREA.
           03 LK-MENSAGEM                        PIC X(40).

       PROCEDURE DIVISION USING LK-COM-AREA.

       MAIN-PROCEDURE.
           DISPLAY '**** PARA CADASTRAR INFORME **** '
           SET EXIT-OK               TO FALSE
           PERFORM P300-CADASTRA     THRU P300-FIM UNTIL EXIT-OK
           PERFORM P900-FIM
           .

       P300-CADASTRA.
           SET EOF-OK               TO FALSE
           SET FS-OK                TO TRUE
           DISPLAY ' '
           DISPLAY 'ID DO ALUNO:'
           ACCEPT WS-ID-ALUNO
           DISPLAY ' '
           DISPLAY 'NOME DO ALUNO: '
           ACCEPT WS-NM-ALUNO
           DISPLAY ' '
           DISPLAY 'TELEFONE DO ALUNO: '
           ACCEPT WS-TEL-ALUNO
           DISPLAY ' '
           DISPLAY 'NOTA EM PORTUGUES: '
           ACCEPT WS-NT-PORT
           DISPLAY ' '
           DISPLAY 'NOTA EM MATEMATICA: '
           ACCEPT WS-NT-MAT
           DISPLAY ' '
           DISPLAY 'NOTA EM CIENCIAS: '
           ACCEPT WS-NT-CIEN
           DISPLAY ' '


           OPEN I-O ALUNOS


           IF WS-FS EQUAL 35 THEN
               OPEN OUTPUT ALUNOS
           END-IF

           IF FS-OK  THEN
            MOVE WS-ID-ALUNO           TO ID-ALUNO
            MOVE WS-NM-ALUNO           TO NM-ALUNO

            WRITE REG-CONTATOS
                  INVALID KEY
                  DISPLAY '*** ALUNO JA CADASTRADO ***'
                  DISPLAY ' '
                  NOT INVALID KEY
                  DISPLAY '*** ALUNO CADASTRADO COM SUCESSO! ***'
                  DISPLAY ' '
           END-WRITE
           ELSE
            DISPLAY 'ERRO AO ABRIR O ARQUIVO DE ALUNO'
            DISPLAY 'FILE STATUS: ' WS-FS
            END-IF

            CLOSE ALUNOS

           DISPLAY 'DESEJA CONTINUAR? <S> (SIM) - <N> (NAO)'
           ACCEPT WS-EXIT

           .


       P300-FIM.
       P900-FIM.


            GOBACK.
       END PROGRAM CADALUNO.
