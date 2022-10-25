      ******************************************************************
      * Author:RODRIGO ORLANDO
      * Date:24/10/2022
      * Purpose: MENU CAD CONTATO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUCNTT.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

         01 WS-COM-AREA.
           03 WS-MENSAGEM                  PIC X(40).
         77 WS-OPCAO                       PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM P300-PROCESSA    THRU P300-FIM UNTIL WS-OPCAO = 'F'
           PERFORM P900-FIM
           .
       P300-PROCESSA.
           MOVE SPACES                    TO WS-OPCAO
           DISPLAY '**************************************************'
           DISPLAY '*               SISTEMA DE ALUNOS                *'
           DISPLAY '**************************************************'
           DISPLAY '|      1 - CADASTRAR ALUNOS                      |'
           DISPLAY '|      2 - LISTAR ALUNOS                         |'
           DISPLAY '|      3 - CONSULTAR ALUNO                       |'
           DISPLAY '|      4 - ALTERAR CADASTRO DO ALUNO             |'
           DISPLAY '|      5 - EXCLUIR CADASTRO DO ALUNO             |'
           DISPLAY '|            OU TECLE (X) PARA SAIR              |'
           DISPLAY '**************************************************'
           ACCEPT WS-OPCAO

           EVALUATE WS-OPCAO

                   WHEN '1'
                       MOVE '**** INCLUSAO DE CONTATO ****'
                                          TO WS-MENSAGEM
            CALL 'C:\Users\777\Desktop\BOLETIMV2\bin\CADALUNO'

                   WHEN '2'
                       MOVE '**** LISTAGEM DE CONTATO ****'
                                          TO WS-MENSAGEM
            CALL 'C:\Users\777\Desktop\BOLETIMV2\bin\LISTARALUNO'
                                               USING WS-COM-AREA

                   WHEN '3'
                       MOVE '**** CONSULTA DE CONTATO ****'
                                          TO WS-MENSAGEM
            CALL 'C:\Users\777\Desktop\BOLETIMV2\bin\CONSULTAR'
                                               USING WS-COM-AREA

                   WHEN '4'
                       MOVE '**** ALTERACAO DE CONTATO ****'
                                          TO WS-MENSAGEM
            CALL 'C:\Users\777\Desktop\BOLETIMV2\bin\ALTERAR'
                                               USING WS-COM-AREA
                   WHEN '5'
                       MOVE '**** EXCLUSAO DE CONTATO ****'
                                          TO WS-MENSAGEM
            CALL 'C:\Users\777\Desktop\BOLETIMV2\bin\DELETAR'
                                               USING WS-COM-AREA
                   WHEN 'X'
                      DISPLAY 'OBRIGADO VOLTE SEMPRE'
                      MOVE 'F'             TO WS-OPCAO
                  WHEN 'x'
                      DISPLAY 'OBRIGADO VOLTE SEMPRE'
                      MOVE 'F'             TO WS-OPCAO
                   WHEN OTHER
                        DISPLAY 'OPCAO INVALIDA'
           END-EVALUATE

           .
       P300-FIM.
       P900-FIM.


            STOP RUN.
       END PROGRAM MENUCNTT.
