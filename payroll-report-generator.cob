      ******************************************************************
      * COBOL 85 program that processes employee payroll data.         *
      * Reads an input file with the raw data, and produces an output  *
      * file with the calculated/formatted data.                       *
      * Adapted from Structured COBOL Programming 8ed by Stern/Stern.  *
      * User-defined symbols are lower-case, COBOL keywords are UC.    *
      ******************************************************************

       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. payroll-report-generator.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.   
      *
      * Input filename: `emp-dat.txt`.
      *
           SELECT employee-data ASSIGN TO "emp-dat.txt"
      *
      * Don't interpret newline chars as data.
      *
               ORGANIZATION IS LINE SEQUENTIAL.
      *
      * Output filename: `emp-dat-fmt.txt`.
      *
           SELECT payroll-listing ASSIGN TO "emp-dat-fmt.txt"
      *
               ORGANIZATION IS LINE SEQUENTIAL.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD employee-data LABEL RECORDS ARE STANDARD.
      *
      * Define the expected format of the input data.
      *
       01 employee-record.
      *
           05 employee-name-in PIC X(20).
      *
           05 hours-worked-in PIC 9(2).
      *
           05 hourly-rate-in PIC 9V99.
      *
       FD payroll-listing LABEL RECORDS ARE STANDARD.
      *
      * Define the format of the output data.
      *
       01 print-rec.
      *
           05 PIC X(20).
      *
           05 name-out PIC X(20).
      *
           05 PIC X(10).
      *
           05 hours-out PIC 9(2).
      *
           05 PIC X(8).
      *
           05 rate-out PIC 9.99.
      *
           05 PIC X(6).
      *
           05 weekly-wages-out PIC 999.99.
      *
       WORKING-STORAGE SECTION.
      *
       01 are-there-more-records PIC X VALUE 'Y'.
      *
       PROCEDURE DIVISION.
      *
       100-MAIN-MODULE.
      *
           OPEN INPUT employee-data
      *
               OUTPUT payroll-listing
      *
      * Repeatedly read one data record into the input area.
      *
           PERFORM UNTIL are-there-more-records = 'N'
      *
               READ employee-data
      *
                   AT END
      *
                       MOVE 'N' TO are-there-more-records
      *
                   NOT AT END
      *
                       PERFORM 200-WAGE-ROUTINE
      *
               END-READ
      *
           END-PERFORM
      *
           CLOSE employee-data
      *
               payroll-listing
      *
           STOP RUN.
      *
       200-WAGE-ROUTINE.
      *
           MOVE SPACES TO print-rec
      *
           MOVE employee-name-in TO name-out
      *
           MOVE hours-worked-in TO hours-out
      *
           MOVE hourly-rate-in TO rate-out
      *
           MULTIPLY hours-worked-in BY hourly-rate-in
      *
               GIVING weekly-wages-out
      *
           WRITE print-rec.
