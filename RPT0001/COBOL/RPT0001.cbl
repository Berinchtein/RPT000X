      *****************************************************************
      * Program name:    RPT0001
      * Original author: MANUEL JARRY
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  ---------------------------------------
      * 25/11/2025 MANUEL JARRY  Created in COBOL learning journey
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  RPT0001.
       AUTHOR. MANUEL JARRY Z85614.
       INSTALLATION. Berinchtein Github Repository.
       DATE-WRITTEN. 04/02/2026.
       DATE-COMPILED. DD/02/2026.
       SECURITY. NON-CONFIDENTIAL.
      *****************************************************************
      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL-COMPILER.
       OBJECT-COMPUTER. GNUCOBOL-COMPILER.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT CUSTMAST
              ASSIGN TO "INPUT-FILES/CUSTMAST"
              ORGANIZATION IS SEQUENTIAL.
      *
           SELECT SALESRPT
              ASSIGN TO "OUTPUT-FILES/SALESRPT"
              ORGANIZATION IS SEQUENTIAL.
      *****************************************************************
      *
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
      *
       FD  CUSTMAST.
       01 CUSTOMER-MASTER-RECORD.
          05 CM-BRANCH-NUMBER       PIC 9(2).
          05 CM-SALESREP-NUMBER     PIC 9(2).
          05 CM-CUSTOMER-NUMBER     PIC 9(5).
          05 CM-CUSTOMER-NAME       PIC X(20).
          05 CM-SALES-THIS-YTD      PIC S9(5)V9(2).
          05 CM-SALES-LAST-YTD      PIC S9(5)V9(2).
          05 FILLER                 PIC X.
      *
       FD  SALESRPT.
       01 PRINT-AREA                PIC X(132).
      *****************************************************************
      *
      *****************************************************************
       WORKING-STORAGE SECTION. 
       01 SWITCHES.
          05 CUSTMAST-EOF-SWITCH    PIC X             VALUE "N".
             88 CUSTMAST-EOF                          VALUE "Y".
      *
       01 PRINT-FIELD.
          05 SPACE-CONTROL          PIC S9.
      *
       01 TOTAL-FIELDS.
          05 GRAND-TOTAL-THIS-YTD   PIC S9(7)V99      VALUE ZERO.
          05 GRAND-TOTAL-LAST-YTD   PIC S9(7)V99      VALUE ZERO.
      *
       01 CURRENT-DATE-AND-TIME.
          05 CURRENT-DATE.
             10 CD-CURRENT-YEAR     PIC 9(4).
             10 CD-CURRENT-MONTH    PIC 9(2).
             10 CD-CURRENT-DAY      PIC 9(2).
          05 CURRENT-TIME.
             10 CD-CURRENT-HOURS    PIC 9(2).
             10 CD-CURRENT-MINUTES  PIC 9(2).
          05 FILLER                 PIC X(9).
      *
       01 HEADING-LINE-1.
          05 FILLER                 PIC X(7)          VALUE "DATE:  ".
          05 HL1-DAY                PIC 9(2).
          05 FILLER                 PIC X(1)          VALUE "/".
          05 HL1-MONTH              PIC 9(2).
          05 FILLER                 PIC X(1)          VALUE "/".
          05 HL1-YEAR               PIC 9(4).
          05 FILLER                 PIC X(12)         VALUE SPACE.
          05 FILLER                 PIC X(12)         VALUE
                "MANU'S COBOL".
          05 FILLER                 PIC X(12)         VALUE
                " REPORT PREP".
          05 FILLER                 PIC X(12)         VALUE
                "ARATION PROG".
          05 FILLER                 PIC X(12)         VALUE
                "RAM.        ".
          05 FILLER                 PIC X(55)         VALUE SPACE.
      *
       01 HEADING-LINE-2.
          05 FILLER                 PIC X(7)          VALUE "TIME:  ".
          05 HL2-HOURS              PIC 9(2).
          05 FILLER                 PIC X(1)          VALUE ":".
          05 HL2-MINUTES            PIC 9(2).
          05 FILLER                 PIC X(55)         VALUE SPACE.
          05 FILLER                 PIC X(7)          VALUE "RPT0001".
          05 FILLER                 PIC X(58)         VALUE SPACE.
      *
       01 HEADING-LINE-3.
          05 FILLER                 PIC X(4)          VALUE "CUST".
          05 FILLER                 PIC X(28)         VALUE SPACE.
          05 FILLER                 PIC X(5)          VALUE "SALES".
          05 FILLER                 PIC X(10)         VALUE SPACE.
          05 FILLER                 PIC X(5)          VALUE "SALES".
          05 FILLER                 PIC X(80)         VALUE SPACE.
      *
       01 HEADING-LINE-4.
          05 FILLER                 PIC X(3)          VALUE "NUM".
          05 FILLER                 PIC X(4)          VALUE SPACE.
          05 FILLER                 PIC X(13)         VALUE
                "CUSTOMER NAME"
           .
          05 FILLER                 PIC X(11)         VALUE SPACE.
          05 FILLER                 PIC X(8)          VALUE "THIS YTD".
          05 FILLER                 PIC X(6)          VALUE SPACE.
          05 FILLER                 PIC X(8)          VALUE "LAST YTD".
          05 FILLER                 PIC X(79)         VALUE SPACE.
      *
       01 CUSTOMER-LINE.
          05 CL-CUSTOMER-NUMBER     PIC 9(5).
          05 FILLER                 PIC X(2)          VALUE SPACE.
          05 CL-CUSTOMER-NAME       PIC X(20).
          05 FILLER                 PIC X(3)          VALUE SPACE.
          05 CL-SALES-THIS-YTD      PIC ZZ,ZZ9.99-.
          05 FILLER                 PIC X(4)          VALUE SPACE.
          05 CL-SALES-LAST-YTD      PIC ZZ,ZZ9.99-.
          05 FILLER                 PIC X(80)         VALUE SPACE.
      *
       01 GRAND-TOTAL-LINE.
          05 FILLER                 PIC X(27)         VALUE SPACE.
          05 GTL-SALES-THIS-YTD     PIC Z,ZZZ,ZZ9.99-.
          05 FILLER                 PIC X(1)          VALUE SPACE.
          05 GTL-SALES-LAST-YTD     PIC Z,ZZZ,ZZ9.99-.
          05 FILLER                 PIC X(78)         VALUE SPACE.
      *
       01 FOOTER-LINE.
          05 FILLER                 PIC X(12)         VALUE
                "THANK YOU FO".
          05 FILLER                 PIC X(12)         VALUE
                "R USING MANU".
          05 FILLER                 PIC X(12)         VALUE
                "'S COBOL REP".
          05 FILLER                 PIC X(12)         VALUE
                "ORT PREPARAT".
          05 FILLER                 PIC X(12)         VALUE
                "ION PROGRAM.".
          05 FILLER                 PIC X(12)         VALUE
                " ALL INITIAL".
          05 FILLER                 PIC X(12)         VALUE
                " CODE PROVID".
          05 FILLER                 PIC X(12)         VALUE
                "ED BY MIKE M".
          05 FILLER                 PIC X(12)         VALUE
                "URACH'S 'MAI".
          05 FILLER                 PIC X(12)         VALUE
                "NFRAME COBOL".
          05 FILLER                 PIC X(2)          VALUE "'.".
          05 FILLER                 PIC X(10)          VALUE SPACE.
      *****************************************************************
      *
      *****************************************************************
       PROCEDURE DIVISION.
      *****************************************************************
      *
      *****************************************************************
       000-PREPARE-SALES-REPORT.
      *
           OPEN INPUT CUSTMAST
                OUTPUT SALESRPT.
           PERFORM 100-FORMAT-REPORT-HEADING.
           PERFORM 200-PRINT-HEADING-LINES.
           PERFORM 300-PREPARE-SALES-LINES
              UNTIL CUSTMAST-EOF.
           PERFORM 400-PRINT-GRAND-TOTALS.
           PERFORM 500-PRINT-FOOTER.
           CLOSE CUSTMAST
                 SALESRPT.
           STOP RUN.
      *
      *****************************************************************
      *
      *****************************************************************
       100-FORMAT-REPORT-HEADING.
      *
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CD-CURRENT-MONTH TO HL1-MONTH.
           MOVE CD-CURRENT-DAY TO HL1-DAY.
           MOVE CD-CURRENT-YEAR TO HL1-YEAR.
           MOVE CD-CURRENT-HOURS TO HL2-HOURS.
           MOVE CD-CURRENT-MINUTES TO HL2-MINUTES.
      *
      *****************************************************************
      *
      *****************************************************************
       200-PRINT-HEADING-LINES.
      *
           MOVE HEADING-LINE-1 TO PRINT-AREA.
           WRITE PRINT-AREA.
           MOVE HEADING-LINE-2 TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 1 LINES.
           MOVE HEADING-LINE-3 TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 2 LINES.
           MOVE HEADING-LINE-4 TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 1 LINES.
           MOVE 2 TO SPACE-CONTROL.
      *
      *****************************************************************
      *
      *****************************************************************
       300-PREPARE-SALES-LINES.
      *
           PERFORM 310-READ-CUSTOMER-RECORD.
           IF NOT CUSTMAST-EOF
              PERFORM 320-PRINT-CUSTOMER-LINE.
      *
      *****************************************************************
      *
      *****************************************************************
       310-READ-CUSTOMER-RECORD.
      *
           READ CUSTMAST
           AT END
              MOVE "Y" TO CUSTMAST-EOF-SWITCH.
      *
      *****************************************************************
      *
      *****************************************************************
       320-PRINT-CUSTOMER-LINE.
      *
           MOVE CM-CUSTOMER-NUMBER TO CL-CUSTOMER-NUMBER.
           MOVE CM-CUSTOMER-NAME TO CL-CUSTOMER-NAME.
           MOVE CM-SALES-THIS-YTD TO CL-SALES-THIS-YTD.
           MOVE CM-SALES-LAST-YTD TO CL-SALES-LAST-YTD.
           MOVE CUSTOMER-LINE TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING SPACE-CONTROL LINES.
           ADD CM-SALES-THIS-YTD TO GRAND-TOTAL-THIS-YTD.
           ADD CM-SALES-LAST-YTD TO GRAND-TOTAL-LAST-YTD.
           MOVE 1 TO SPACE-CONTROL.
      *
      *****************************************************************
      *
      *****************************************************************
       400-PRINT-GRAND-TOTALS.
      *
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 2 LINES.
      *
      *****************************************************************
      *
      *****************************************************************
       500-PRINT-FOOTER.
      *
           MOVE FOOTER-LINE TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 2 LINES.
      *
