       IDENTIFICATION DIVISION.
           PROGRAM-ID. ALUNOSMEDIA.
           AUTHOR Felipe Sinico.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ALUNOS ASSIGN TO "Alunos.data"   
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ALUNOS.
       01 REGISTRO-ALUNO.
           05 NOME-ALUNO     PIC X(20).
           05 NOTA-1         PIC 9(3)V9(2).
           05 NOTA-2         PIC 9(3)V9(2).
           05 MEDIA          PIC 9(3)V9(2).
           05 RESULTADO      PIC X(10).

       WORKING-STORAGE SECTION.
       01 CONTADORES.
           05 TOTAL-APROVADO      PIC 9(3) VALUE 0.
           05 TOTAL-REPROVADO    PIC 9(3) VALUE 0.

       01 LEITURA-FINALIZADA    PIC X VALUE "N".

       PROCEDURE DIVISION. 
       PRINCIPAL.
           DISPLAY "=====================================".
           DISPLAY " Contagem de Alunos".
           DISPLAY "=====================================".
      
           OPEN INPUT ARQUIVO-ALUNOS.  
           PERFORM PREOCESSAR-REGISTROS.
           CLOSE ARQUIVO-ALUNOS.
           PERFORM EXIBIR-RESUMO.
           STOP RUN.
           
       PREOCESSAR-REGISTROS.
           PERFORM UNTIL LEITURA-FINALIZADA = "S"
              PERFORM LER-REGISTRO
              IF LEITURA-FINALIZADA NOT = "S"
                 PERFORM EXIBIR-ALUNOS
                 PERFORM CONTAR-APROVADOS
              END-IF   
           END-PERFORM.

       LER-REGISTRO.
           READ ARQUIVO-ALUNOS INTO REGISTRO-ALUNO
              AT END
                  MOVE "S" TO LEITURA-FINALIZADA
              NOT AT END
                  CONTINUE
           END-READ.

       EXIBIR-ALUNOS.
           DISPLAY NOME-ALUNO "MEDIA: " MEDIA " " 
                 "  STATUS: " RESULTADO.

       CONTAR-APROVADOS.
           IF RESULTADO = "Aprovado"
              ADD 1 TO TOTAL-APROVADO
           ELSE
              IF RESULTADO = "Reprovador"   
                 ADD 1 TO TOTAL-REPROVADO
              END-IF
           END-IF.      

       EXIBIR-RESUMO.
           DISPLAY "=====================================".
           DISPLAY "Resumo: ".
           DISPLAY "Total de Aprovados: " TOTAL-APROVADO.
           DISPLAY "Total de Reprovados: " TOTAL-REPROVADO.
           DISPLAY "=====================================".
