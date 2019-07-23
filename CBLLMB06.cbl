       IDENTIFICATION DIVISION.
	   PROGRAM-ID. 		CBLLMB06.
	   AUTHOR.			LIBBY BAKALAR. 
	   DATE-WRITTEN. 	04/19/2019.
	   DATE-COMPILED.   04/29/2019.

      ******************************************************************
	  *    THIS PROGRAM WILL PRINT 2 SUMMARY REPORTS FOR JUST FITS     *
      *   FURNITURE EMPORIUM. FIRST REPORT IS A WEEKLY SALES REPORT,   *
      *  SECOND REPORT IS A SUMMARY OF TOTAL SALES WEEKLY BY FURNITURE * 
      *     TYPE. THIS PROGRAM WILL INCLUDE A TWO DIMENSIONAL TABLE.   *
	  ******************************************************************
	  
	   ENVIRONMENT DIVISION. 
	   INPUT-OUTPUT SECTION. 
	   FILE-CONTROL.

           SELECT MASTER
           ASSIGN TO 'C:\COBOL\FURN.DAT'
		   ORGANIZATION IS LINE SEQUENTIAL.
			   
		   SELECT PRTOUT
           ASSIGN TO 'C:\COBOL\CBLSMRY.PRT'
	       ORGANIZATION IS LINE SEQUENTIAL.
	 
       DATA DIVISION.
	   FILE SECTION.
	       
	   FD  MASTER
	       LABEL RECORD IS STANDARD 
		   RECORD CONTAINS 42 CHARACTERS
		   DATA RECORD IS ST-REC.

       01  ST-REC.
		   05 ST-SLM-NUM         PIC 99.
           05 ST-DAY             PIC 9.
           05 ST-AMOUNT          PIC S9(5)V99. 
           05 ST-FUR-CODE        PIC 9. 
           05 ST-SALES-DATE      PIC 9(6).
           05 ST-SLM-NAME        PIC X(25). 

	   FD  PRTOUT 
		   LABEL RECORD IS OMITTED 
		   RECORD CONTAINS 132 CHARACTERS 
		   LINAGE IS 60 WITH FOOTING AT 55
		   DATA RECORD IS PRTLINE.

       01  PRTLINE				   PIC X(132).
	   
	   WORKING-STORAGE SECTION. 
	   01  WORK-AREA. 
		   05  C-PCTR 			   PIC 99	         VALUE ZERO.
           05  MORE-RECS 		   PIC XXX 	         VALUE 'YES'.
           05  DAY-SUB             PIC 99            VALUE ZERO. 
           05  PERSON-SUB          PIC 99            VALUE ZERO. 
           05  FURN-SUB            PIC 99            VALUE ZERO.
           05  C-FURN-GRAND-TOTAL  PIC 9(12)V99      VALUE ZERO. 

	   01  I-DATE. 
		   05  I-YEAR			   PIC 9(4).
		   05  I-MONTH 			   PIC 99.
		   05  I-DAY			   PIC 99.
		   05  I-TIME 			   PIC X(11).

       01 TOTALS-REC-TABLE.    
           05 PERSON OCCURS 16. 
               10 PERSON-TOTAL     PIC 9(9)V99. 
               10 DAY-TOTAL        PIC 9(9)V99     OCCURS 8.

       01  FURN-TABLE.
	       05  FILLER	           PIC X(22)   VALUE 'SOFAS/LOVESEATS'. 
           05  FILLER	           PIC X(22)   VALUE 'CHAIRS'.
           05  FILLER	           PIC X(22)   VALUE 
            'COFFEE/END TABLES'.
           05  FILLER	           PIC X(22)   VALUE 
            'DINING ROOM TABLES'.
           05  FILLER	           PIC X(22)   VALUE 
            'DINING ROOM CHAIRS'.
           05  FILLER	           PIC X(22)   VALUE 
            'HUTCHES/CURIO CABINETS'.
           05  FILLER	           PIC X(22)   VALUE 'LAMPS'.
           05  FILLER	           PIC X(22)   VALUE 
            'MATTRESS/BOX SPRINGS'.
           05  FILLER	           PIC X(22)   VALUE 
            'BEDROOM FURNITURE'.

       01 TABLE-FURN REDEFINES FURN-TABLE. 
           05  FURN-TYPE           PIC X(22)   OCCURS 9.
	   
       01 FURN-TYPE-TOTAL-TABLE. 
           05  FURN-TYPE-TOTAL     PIC 9(10)V99 OCCURS 9.

       01 PERSON-TOTAL-TABLE. 
           05  PERSON-GRAND-TOTAL  PIC 9(10)V99 OCCURS 15.

       01 TABLE-NAME. 
           05  NAME-TABLE          PIC X(25)   OCCURS 15.

	   01  COMPANY-TITLE. 
		   05  FILLER			   PIC X(6)    VALUE "DATE: ".
		   05  O-MONTH 			   PIC 99.
		   05  FILLER 			   PIC X 	   VALUE '/'.
		   05  O-DAY 			   PIC 99. 
		   05  FILLER 			   PIC X 	   VALUE '/'.
		   05  O-YEAR 			   PIC 9999.
		   05  FILLER 			   PIC X(36)   VALUE SPACES.
		   05  FILLER 			   PIC X(28)   VALUE 'JUST FITS FURNITUR
	  -										   'E EMPORIUM'. 
           05  FILLER 			   PIC X(44)   VALUE SPACES.
		   05  FILLER 			   PIC X(6)    VALUE "PAGE: ".
		   05  O-PCTR 			   PIC Z9.

       01  REPORT-TITLE.
           05  FILLER              PIC X(56)   VALUE SPACES.
           05  FILLER 			   PIC X(19)   VALUE 'WEEKLY SALES REPOR
	  -										   'T'.

       01  TOTAL-REPORT-TITLE.
           05  FILLER              PIC X(58)   VALUE SPACES.
           05  FILLER 			   PIC X(16)   VALUE 'FURNITURE REPORT'.

       01  COLUMN-HEADING-1.
           05 FILLER               PIC X(11)   VALUE 'SALESPERSON'. 
           05 FILLER               PIC X(21)   VALUE SPACES. 
           05 FILLER               PIC X(6)    VALUE 'SUNDAY'. 
           05 FILLER               PIC X(7)    VALUE SPACES. 
           05 FILLER               PIC X(6)    VALUE 'MONDAY'.
           05 FILLER               PIC X(6)    VALUE SPACES.
           05 FILLER               PIC X(7)    VALUE 'TUESDAY'.
           05 FILLER               PIC X(4)    VALUE SPACES. 
           05 FILLER               PIC X(9)    VALUE 'WEDNESDAY'. 
           05 FILLER               PIC X(5)    VALUE SPACES. 
           05 FILLER               PIC X(8)    VALUE 'THURSDAY'.
           05 FILLER               PIC X(7)    VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE 'FRIDAY'.
           05 FILLER               PIC X(5)    VALUE SPACES. 
           05 FILLER               PIC X(8)    VALUE 'SATURDAY'.
           05 FILLER               PIC X(10)   VALUE SPACES.
           05 FILLER               PIC X(6)    VALUE 'WEEKLY'.

       01  COLUMN-HEADING-2.
           05 FILLER               PIC X(3)    VALUE SPACES. 
           05 FILLER               PIC X(4)    VALUE 'NAME'. 
           05 FILLER               PIC X(119)  VALUE SPACES. 
           05 FILLER               PIC X(6)    VALUE 'TOTALS'. 

       01  TOTAL-HEADING-1.
           05 FILLER               PIC X(33)   VALUE SPACES. 
           05 FILLER               PIC X(9)    VALUE 'FURNITURE'. 
           05 FILLER               PIC X(49)   VALUE SPACES. 
           05 FILLER               PIC X(9)    VALUE 'FURNITURE'. 

       01  TOTAL-HEADING-2.
           05 FILLER               PIC X(33)   VALUE SPACES. 
           05 FILLER               PIC X(4)    VALUE 'TYPE'. 
           05 FILLER               PIC X(53)   VALUE SPACES. 
           05 FILLER               PIC X(10)   VALUE 'TYPE TOTAL'.

       01 DASH-LINE. 
           05 FILLER               OCCURS 132.
               10 FILLER               PIC X(1)  VALUE '-'.
                            
       01  DETAIL-LINE. 
           05 O-NAME               PIC X(25).    
           05 FILLER               PIC X(3)    VALUE SPACES.
           05 FILLER               OCCURS 7.
               10 O-DAY-TOTAL      PIC ZZZ,ZZZ.99.
               10 FILLER           PIC X(3)    VALUE SPACES. 
           05 O-TOTAL-DAY-TOTAL    PIC ZZ,ZZZ,ZZZ.99.

       01  TOTAL-DETAIL-LINE. 
           05 FILLER               PIC X(33)   VALUE SPACES.
           05 FILLER               OCCURS 9. 
               10 O-FURN-TYPE      PIC X(22).    
               10 FILLER           PIC X(32)   VALUE SPACES. 
               10 O-FURN-TYPE-TOTAL PIC ZZ,ZZZ,ZZZ.99.

       01  GRAND-TOTAL-LINE-1.
           05 FILLER               PIC X(13)   VALUE 'GRAND TOTALS:'.
           05 FILLER               PIC X(11)   VALUE SPACES. 
           05 O-SUN-TOTAL          PIC $$$,$$$,$$$.99. 
           05 FILLER               PIC X(12)   VALUE SPACES. 
           05 O-TUES-TOTAL         PIC $$$,$$$,$$$.99.
           05 FILLER               PIC X(12)   VALUE SPACES. 
           05 O-THUR-TOTAL         PIC $$$,$$$,$$$.99. 
           05 FILLER               PIC X(13)   VALUE SPACES. 
           05 O-SAT-TOTAL          PIC $$,$$$,$$$.99.

       01  GRAND-TOTAL-LINE-2.
           05 FILLER               PIC X(37)   VALUE SPACES. 
           05 O-MON-TOTAL          PIC $$$,$$$,$$$.99. 
           05 FILLER               PIC X(12)   VALUE SPACES. 
           05 O-WED-TOTAL          PIC $$$,$$$,$$$.99.
           05 FILLER               PIC X(12)   VALUE SPACES. 
           05 O-FRI-TOTAL          PIC $$$,$$$,$$$.99. 
           05 FILLER               PIC X(12)   VALUE SPACES. 
           05 O-TOT-TOTAL          PIC $$,$$$,$$$,$$$.99.

       01  TOT-GRAND-TOTAL-LINE.
           05 FILLER               PIC X(33)   VALUE SPACES.
           05 FILLER 			   PIC X(21)   VALUE 'FURNITURE GRAND TO
	  -										   'TAL'. 
           05 FILLER               PIC X(29)   VALUE SPACES. 
           05 O-FURN-GRAND-TOTAL   PIC $$,$$$,$$$,$$$.99. 

       PROCEDURE DIVISION. 

       0000-CBLLMB06.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN. 

       1000-INIT. 
           MOVE FUNCTION CURRENT-DATE TO I-DATE. 
           MOVE I-DAY TO O-DAY.
           MOVE I-YEAR TO O-YEAR.
           MOVE I-MONTH TO O-MONTH.

           OPEN INPUT MASTER.
           OPEN OUTPUT PRTOUT. 

           PERFORM VARYING FURN-SUB FROM 1 BY 1
               UNTIL FURN-SUB > 9    
                   MOVE 0 TO FURN-TYPE-TOTAL(FURN-SUB).

           PERFORM VARYING PERSON-SUB FROM 1 BY 1
               UNTIL PERSON-SUB > 8   
                   MOVE 0 TO PERSON-GRAND-TOTAL(PERSON-SUB).

           PERFORM VARYING PERSON-SUB FROM 1 BY 1
               UNTIL PERSON-SUB > 15   
                   MOVE SPACES TO NAME-TABLE(PERSON-SUB).

           PERFORM 1100-CLR-TABLE 
               VARYING PERSON-SUB FROM 1 BY 1 
                   UNTIL PERSON-SUB > 16.

           PERFORM 9100-READ.
           PERFORM 9200-HDG.

       1100-CLR-TABLE. 
           MOVE 0 TO PERSON-TOTAL(PERSON-SUB). 
           PERFORM VARYING DAY-SUB FROM 1 BY 1
               UNTIL DAY-SUB > 8
                   MOVE 0 TO DAY-TOTAL(PERSON-SUB, DAY-SUB).

       2000-MAINLINE. 
           PERFORM 2300-CALCS.
           PERFORM 9100-READ.  
           
       2300-CALCS.  
      *    ADDING TO FURNITURE GRAND TOTAL
           ADD ST-AMOUNT TO C-FURN-GRAND-TOTAL. 

      *    ADDING TO FURNITURE TYPE TOTAL 
           PERFORM VARYING FURN-SUB FROM 1 BY 1 
             UNTIL FURN-SUB > 9
                IF FURN-SUB  =  ST-FUR-CODE
                   ADD ST-AMOUNT TO FURN-TYPE-TOTAL(FURN-SUB).
       
      *    ADDING TO GRAND TOTAL OF DAYS AND PERSONS (16, 8)
           ADD ST-AMOUNT TO DAY-TOTAL(16, 8).
       
      *    ADDING TO DAY GRAND TOTAL (16, DAY-SUB) 
           PERFORM VARYING DAY-SUB FROM 1 BY 1 
             UNTIL DAY-SUB > 8
               IF ST-DAY = DAY-SUB
                   ADD ST-AMOUNT TO DAY-TOTAL(16, DAY-SUB).
       
      *    ADDING TO PERSON GRAND TOTAL (PERSON-SUB, 8)
           PERFORM VARYING PERSON-SUB FROM 1 BY 1  
             UNTIL PERSON-SUB > 16
               IF ST-SLM-NUM = PERSON-SUB 
                   ADD ST-AMOUNT TO DAY-TOTAL(PERSON-SUB, 8).
       
      *    ADDING TO CORRECT PERSON/DAY (PERSON-SUB, DAY-SUB)
           PERFORM 2310-CALCS-TWO 
               VARYING PERSON-SUB FROM 1 BY 1 
                   UNTIL PERSON-SUB > 16.

      *    ADDING SALESPERSON NAME TO TABLE(SUB MATCHES THEIR NUMBER)
           PERFORM VARYING PERSON-SUB FROM 1 BY 1 
             UNTIL PERSON-SUB = ST-SLM-NUM 
               IF PERSON-SUB = ST-SLM-NUM
                   MOVE ST-SLM-NAME TO NAME-TABLE(ST-SLM-NUM).
                   MOVE ST-SLM-NAME TO NAME-TABLE(ST-SLM-NUM).

      *    ADDING PERSON GRAND TOTAL FOR THE WEEK TO PERSON-TOTAL-TABLE
           PERFORM VARYING PERSON-SUB FROM 1 BY 1
             UNTIL PERSON-SUB > 15
               MOVE DAY-TOTAL(PERSON-SUB, 8) TO
                 PERSON-GRAND-TOTAL(PERSON-SUB).
       
       2310-CALCS-TWO. 
           PERFORM VARYING DAY-SUB FROM 1 BY 1 
               UNTIL DAY-SUB > 8 
                   IF ST-DAY = DAY-SUB AND ST-SLM-NUM = PERSON-SUB
                       ADD ST-AMOUNT TO DAY-TOTAL(PERSON-SUB, DAY-SUB).
                     
       2400-OUTPUT.
           MOVE NAME-TABLE(PERSON-SUB) TO O-NAME.
           MOVE PERSON-GRAND-TOTAL(PERSON-SUB) TO O-TOTAL-DAY-TOTAL.
           PERFORM 2410-MOVES-WEEKLY
               VARYING DAY-SUB FROM 1 BY 1 
                   UNTIL DAY-SUB > 7.
           IF PERSON-GRAND-TOTAL(PERSON-SUB) > 0
               WRITE PRTLINE
                   FROM DETAIL-LINE
                       AFTER ADVANCING 1 LINE
                           AT EOP
                               PERFORM 9200-HDG. 

       2410-MOVES-WEEKLY.
           MOVE DAY-TOTAL(PERSON-SUB, DAY-SUB) TO O-DAY-TOTAL(DAY-SUB).

       2500-TOTAL-OUTPUT. 
           PERFORM VARYING FURN-SUB FROM 1 BY 1
               UNTIL FURN-SUB > 9
                   MOVE FURN-TYPE(FURN-SUB) TO O-FURN-TYPE 
                   MOVE FURN-TYPE-TOTAL(FURN-SUB) TO O-FURN-TYPE-TOTAL  
                   WRITE PRTLINE
                       FROM TOTAL-DETAIL-LINE
                           AFTER ADVANCING 1 LINE.
                            
       3000-CLOSING.
           PERFORM 2400-OUTPUT
               VARYING PERSON-SUB FROM 1 BY 1 
                   UNTIL PERSON-SUB > 15.

           PERFORM 3100-GRAND-TOTAL.
           PERFORM 9300-TOT-HDG.
           PERFORM 2500-TOTAL-OUTPUT.
           PERFORM 3200-TOT-GRAND-TOTAL.
           
           CLOSE MASTER. 
           CLOSE PRTOUT.

       3100-GRAND-TOTAL. 
           MOVE DAY-TOTAL(16,1) TO O-SUN-TOTAL. 
           MOVE DAY-TOTAL(16,2) TO O-MON-TOTAL. 
           MOVE DAY-TOTAL(16,3) TO O-TUES-TOTAL.
           MOVE DAY-TOTAL(16,4) TO O-WED-TOTAL. 
           MOVE DAY-TOTAL(16,5) TO O-THUR-TOTAL. 
           MOVE DAY-TOTAL(16,6) TO O-FRI-TOTAL.
           MOVE DAY-TOTAL(16,7) TO O-SAT-TOTAL. 
           MOVE DAY-TOTAL(16,8) TO O-TOT-TOTAL. 

           WRITE PRTLINE 
               FROM DASH-LINE
                   AFTER ADVANCING 1 LINES.
           WRITE PRTLINE 
               FROM GRAND-TOTAL-LINE-1
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE 
               FROM GRAND-TOTAL-LINE-2
                   AFTER ADVANCING 1 LINES.

       3200-TOT-GRAND-TOTAL. 
           MOVE C-FURN-GRAND-TOTAL TO O-FURN-GRAND-TOTAL.
           WRITE PRTLINE 
               FROM DASH-LINE
                   AFTER ADVANCING 1 LINES.
           WRITE PRTLINE 
               FROM TOT-GRAND-TOTAL-LINE
                   AFTER ADVANCING 2 LINES.
       
       9100-READ. 
           READ MASTER 
               AT END 
                   MOVE 'NO' TO MORE-RECS.
          
       9200-HDG. 
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR. 

           WRITE PRTLINE 
               FROM  COMPANY-TITLE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE 
               FROM  REPORT-TITLE
                   AFTER ADVANCING 1 LINE. 
           WRITE PRTLINE 
               FROM COLUMN-HEADING-1
                   AFTER ADVANCING 2 LINES. 
           WRITE PRTLINE 
               FROM COLUMN-HEADING-2
                   AFTER ADVANCING 1 LINES.
           WRITE PRTLINE 
               FROM DASH-LINE
                   AFTER ADVANCING 1 LINES.

       9300-TOT-HDG. 
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR. 

           WRITE PRTLINE 
               FROM  COMPANY-TITLE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE 
               FROM TOTAL-REPORT-TITLE
                   AFTER ADVANCING 1 LINE. 
           WRITE PRTLINE 
               FROM  TOTAL-HEADING-1
                   AFTER ADVANCING 2 LINE.
           WRITE PRTLINE 
               FROM  TOTAL-HEADING-2
                   AFTER ADVANCING 1 LINE.
           WRITE PRTLINE 
               FROM DASH-LINE
                   AFTER ADVANCING 1 LINES.
           