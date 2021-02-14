       identification division.
       program-id. A3-SalesComm.
       author. Kaifkhan Vakil.
       date-written. 2021-02-08.
      *Program Description: This code will be reading data from the .dat
      *and then outputting in other file, in which we willbe formatting
      * the output as we want and make it a report with some additional
      * calculation and summary of it.
       environment division.
    

      *Declaring a new input and output section for declaring the files
      *which will be having the output and input from.
       input-output section.
       file-control.
      
      *Input file declarattion
           select sales-file
               assign to "../../../A3.dat"
               organization is line sequential.

      *Output file declaration.
           select report-file
               assign to "../../../A3-SalesComm.out"
               organization is line sequential.
      *
       data division.
       file section.

      *Gatting characters from the input file
       fd sales-file
           data record is sales-rec
           record contains 32 characters.
      *We are decalaring teh variables from the input file in proper 
      *format.
       01 sales-rec.
         05 sr-sman-num    pic 999.
         05 sr-name        pic x(8).
         05 sr-sales       pic 9(6).
         05 sr-min         pic 9(6).
         05 sr-max         pic 9(6).
         05 sr-rate        pic 99v9.

      *Decalaring output file in which we will be writing the data 
      *
       fd report-file
           data record is report-line
           record contains 120 characters.
      *
       01 report-line      pic x(120).

      *
       working-storage section.
      *Eof flag for making sure the end is reached.
       77 ws-eof-flag      pic x       value 'n'.


      *A heading variable which will show my name.
       01 ws-heading1-name-line.
         05 filler         pic x(63)   value spaces.
         05 filler         pic x(28)   value 
         "Kaifkhan Vakil, Assignment 3".
       

      *Second heading used for showing the heading for each page 
       01 ws-heading2-title.
         05 filler         pic x(41)   value spaces.
         05 filler         pic x(23)   value "SALES COMMISSION REPORT".
         05 filler         pic x(13)   value spaces.

      *Thirs heading is the column headings for each data.
       01 ws-heading3-headingns.
         05 filler         pic x(3)    value "No.".
         05 filler         pic x(5)    value spaces.
         05 filler         pic x(4)    value "NAME".
         05 filler         pic x(6)    value spaces.
         05 filler         pic x(5)    value "SALES".
         05 filler         pic x(5)    value spaces.
         05 filler         pic x(3)    value "MIN".
         05 filler         pic x(6)    value spaces.
         05 filler         pic x(3)    value "MAX".
         05 filler         pic x(4)    value spaces.
         05 filler         pic x(4)    value "RATE".
         05 filler         pic x(6)    value spaces.
         05 filler         pic x(6)    value "EARNED".
         05 filler         pic x(6)    value spaces.
         05 filler         pic x(4)    value "PAID".
         05 filler         pic x(5)    value spaces.
         05 filler         pic x(14)   value "BONUS/NO BONUS".

      *Fourth heaading is for hyphens below the column name.
       01 ws-heading4-underlines.
         05 filler         pic x(3)    value "---".
         05 filler         pic x(3)    value spaces.
         05 filler         pic x(8)    value "--------".
         05 filler         pic x(3)    value spaces.
         05 filler         pic x(7)    value "-------".
         05 filler         pic x(2)    value spaces.
         05 filler         pic x(7)    value "-------".
         05 filler         pic x(2)    value spaces.
         05 filler         pic x(7)    value "-------".
         05 filler         pic x(2)    value spaces.
         05 filler         pic x(5)    value "-----".
         05 filler         pic x(2)    value spaces.
         05 filler         pic x(10)   value "----------".
         05 filler         pic x(2)    value spaces.
         05 filler         pic x(10)   value "----------".
         05 filler         pic x(2)    value spaces.
         05 filler         pic x(16)   value "----------------".

      *Report detail line which will be showing the data from the input 
      *  file
       01 ws-report-detail-line.
         05 ws-item-number pic x(3).
         05 filler         pic x(3)    value spaces.
         05 ws-name        pic x(8).
         05 filler         pic x(3)    value spaces.
         05 ws-sales       pic zzz,zz9.
         05 filler         pic x(2)    value spaces.
         05 ws-minimum     pic zzz,zz9.
         05 filler         pic x(2)    value spaces.
         05 ws-maximum     pic zzz,zz9.
         05 filler         pic x(2)    value spaces.
         05 ws-rate        pic z9.9.
         05 ws-percenateg-sign
                           pic x       value "%".
         05 filler         pic x(2)    value spaces.
         05 ws-earned      pic zz,zzz,zz9.
         05 filler         pic x(2)    value spaces.
         05 ws-paid        pic $*****,**9.
         05 filler         pic x(2)    value spaces.
         05 ws-bonus       pic x(16).

      *A working storage section for the variables helpful in 
      *calcualtion like earned, paad and bonus.
       01 ws-calcs.
         05 ws-earned-calc pic 9(8).
         05 ws-paid-calc   pic 9(8).
         05 ws-bonus-remark
                           pic x(16).

      *This section will take care of the total line at the bottom of 
      *each page.
       01 ws-total-line.
         05 filler         pic x(43)   value spaces.
         05 filler         pic x(8)    value "Totals".
         05 ws-total-earned-count
                           pic $$,$$$,$$9.
         05 filler         pic x(2)    value spaces.
         05 ws-total-paid-count
                           pic $$,$$$,$$9.

      *This section will be included in summary which will be showing 
      *the number of people with bonus greater than max
       01 ws-num-max-line.
         05 filler         pic x(31)   value 
         "NUMBER WITH BONUS MORE THAN MAX".
         05 filler         pic x(7)    value spaces.
         05 ws-total-maximum-count
                           pic zz9.


      *This section will be included in the summary which will be 
      *showing the number of people who did not get bonus and whose 
      *earned is less than minimum
       01 ws-num-min-line.
         05 filler         pic x(34)   value 
         "NUMBER WITH NO BONUS LESS THAN MIN".
         05 filler         pic x(4)    value spaces.
         05 ws-total-minimum-count
                           pic zz9.

      *This will be included in summary section and this will be showing 
      *number of total salespeople recorded in the report who got bonus.
       01 ws-bonus-people.
         05 filler         pic x(32)   value 
         "NUMBER OF SALESPEOPLE WITH BONUS".
         05 filler         pic x(6)    value spaces.
         05 ws-total-bonus-count
                           pic zz9.

      *This will be included in the summary section and this will show 
      *the number of sealespeople who got no bonus
       01 ws-no-bonus-people.
         05 filler         pic x(35)   value 
         "NUMBER OF SALESPEOPLE WITHOUT BONUS".
         05 filler         pic x(3)    value spaces.
         05 ws-total-no-bonus-count
                           pic zz9.

      *This will be showing the number of salespeople in the report 
       01 ws-salespoeple-count.
         05 filler         pic x(21)   value
                   "NUMBER OF SALESPEOPLE".
         05 filler         pic x(17)   value spaces.
         05 ws-total-people-count
                           pic zz9.

      *This will be showing the number of salespeople who have their 
      *earned amount equal to paid amount
       01 ws-equal-paid-earned.
         05 filler         pic x(29)   value 
         "NUMBER WITH PAID EQUAL EARNED".
         05 filler         pic x(9)    value spaces.
         05 ws-total-paid-earned-count
                           pic zz9.

      *This will be showing percent of the people who got earned amount 
      *equal to paid amount
       01 ws-equal-paid-earned-percent.
         05 filler         pic x(30)   value 
         "PERCENT WITH PAID EQUAL EARNED".
         05 filler         pic x(8)    value spaces.
         05 ws-total-paid-earned-percent
                           pic zz9.99.
         05 ws-percent-sign-paid-earned
                           pic x.

      *This section will show the percent of people getting bonus
       01 ws-percent-bonus-people.
         05 filler         pic x(31)   value 
         "PERCENT WITH BONUS     >300,000".
         05 filler         pic x(7)    value spaces.
         05 ws-percent-bonus
                           pic zz9.99.
         05 ws-percent-sign-bonus
                           pic x.

      *  This will be showing the percent of people who did not get 
      *  bonus
       01 ws-percent-no-bonus-people.
         05 filler         pic x(31)   value 
         "PERCENT WITHOUT BONUS <=300,000".
         05 filler         pic x(7)    value spaces.
         05 ws-percent-no-bonus
                           pic zz9.99.
         05 ws-percent-sign-no-bonus
                           pic x.

      * THis will be used for summary calculation, to keep the count of
      *each value.
      *01 ws-counters.
       77 ws-earned-total  pic 9(8)    value 0.
       77 ws-paid-total    pic 9(8)    value 0.
       77 ws-bonus-total   pic 9(3)    value 0.
       77 ws-no-bonus-total
                           pic 9(3)    value 0.
       77 ws-people-total  pic 9(3)    value 0.
       77 ws-max-bonus-total
                           pic 999     value 0.
       77 ws-min-bonus-total
                           pic 999     value 0.
       77 ws-paid-earned-total
                           pic 9(3)    value 0.
       77 ws-paid-earned-percent
                           pic 999v99  value 0.
       77 ws-bonus-percent pic 999v99  value 0.
       77 ws-no-bonus-percent
                           pic 999v99  value 0.
       77 ws-100           pic 999     value 100.
       77 ws-300-hundred   pic 9(6)    value 300000.
       77 ws-one           pic 9       value 1.
       77 ws-15-quarter    pic 99v99   value 15.25.
       


      *This will keep track of some file reading function like flags and 
      *page count and lines per page and more.
       77 ws-lines-per-page
                           pic 99      value 10.
       77 ws-page-count    pic 99      value 0.
       77 ws-line-count    pic 99      value 0.
       77 ws-file-empty    pic x       value "e".
       77 ws-file-opened   pic x       value "o".



      *
       procedure division.
       000-main.
      *
           perform 10-open-files.
           move ws-file-opened     to ws-eof-flag.
           perform 20-write-report-heading.
           perform 30-read-input-file.
           move spaces             to ws-report-detail-line.
           write report-line       from ws-report-detail-line.

      *Add data to the output file.
           perform 100-process-pages
           until ws-eof-flag equals ws-file-empty.
           perform 400-print-totals.
           perform 500-summary-calculations.
           perform 600-close-files.
           goback.

      *This parapgraph takes care of the printing of the page 
       100-process-pages.
      *
           perform 200-print-headings.
           perform 300-process-lines
               varying ws-line-count from ws-one by ws-one 
               until (ws-line-count > ws-lines-per-page)
               OR (ws-eof-flag = ws-file-empty).
          
      *This paragraph takes care of teh printing headings.
       200-print-headings.
           add ws-one to ws-page-count.
           if(ws-page-count > ws-one) then
               write report-line from spaces
               after advancing page
               write report-line from ws-heading2-title
               after advancing ws-one line
               write report-line from ws-heading3-headingns
               after advancing 2 lines
               write report-line from spaces
           write report-line from ws-heading4-underlines
           write report-line from spaces
           else 
               write report-line from ws-heading2-title
               write report-line from spaces
               write report-line from ws-heading3-headingns
               write report-line from spaces
               write report-line from ws-heading4-underlines
               write report-line from spaces
           end-if.
          
      *

      *This paragraph takes care of the printing each line in the 
      *report.
       300-process-lines.
           perform 310-bonus-greater-than.
           perform  320-bonus-less-than.
           perform 330-paid-price-calculation.
           perform 340-check-bonus-earned.
       
           if(ws-earned-calc = ws-paid-calc) then
               add ws-one to ws-paid-earned-total
           end-if

           add ws-earned-calc      to ws-earned-total.
           add ws-paid-calc        to ws-paid-total.
           add ws-one                   to ws-people-total.


           move spaces             to ws-report-detail-line.
           move sr-sman-num        to ws-item-number.
           move sr-name            to ws-name.
           move sr-rate            to ws-rate.
           move "%"                to ws-percenateg-sign.
           move sr-min             to ws-minimum.
           move sr-max             to ws-maximum.
           move sr-sales           to ws-sales.
           move ws-earned-calc     to ws-earned.
           move ws-paid-calc       to ws-paid.
           move ws-bonus-remark    to ws-bonus.


           write report-line from ws-report-detail-line
           before advancing 2 lines.
          perform 30-read-input-file.

      *This paragraph takes care of the totals in the report.
       400-print-totals.
           move ws-earned-total    to ws-total-earned-count.
           move ws-paid-total      to ws-total-paid-count.

           write report-line       from ws-total-line.
      *This paragraph takes care of the summary calculations for the 
      *report
       500-summary-calculations.
           perform 510-percent-calculation.
          
           move ws-max-bonus-total to ws-total-maximum-count.
           move ws-min-bonus-total to ws-total-minimum-count.
           move ws-bonus-total     to ws-total-bonus-count.
           move ws-no-bonus-total  to ws-total-no-bonus-count.
           move ws-people-total    to ws-total-people-count.
           move ws-paid-earned-total
                                   to ws-total-paid-earned-count.
           move ws-paid-earned-percent
                                   to ws-total-paid-earned-percent.
           move "%"                to ws-percent-sign-paid-earned
           move ws-bonus-percent   to ws-percent-bonus.
           move ws-no-bonus-percent
                                   to ws-percent-no-bonus.
           move "%"                to ws-percent-sign-bonus
           move "%"                to ws-percent-sign-no-bonus

           write report-line       from ws-num-max-line
           after advancing ws-one line.
           write report-line       from ws-num-min-line.
           write report-line       from ws-bonus-people
           after advancing ws-one line.
           write report-line       from ws-no-bonus-people.
           write report-line       from ws-salespoeple-count.
           write report-line       from ws-equal-paid-earned
           after advancing ws-one line.
           write report-line       from ws-equal-paid-earned-percent.
           write report-line       from ws-percent-bonus-people
           after advancing ws-one line.
           write report-line       from ws-percent-no-bonus-people.
        
      *open files
       10-open-files.
           open input sales-file.
           open output report-file.
          

      *Write report headings
             20-write-report-heading.
           write report-line       from ws-heading1-name-line
             after advancing ws-one line.


          
      *Read input from the file.
       30-read-input-file.
           read sales-file
               at end
                   move ws-file-empty to ws-eof-flag.


           
      *close files
        600-close-files.
           close report-file, sales-file.

      *This paragraph takes care of the bonus greater than calcualtion 
       310-bonus-greater-than.
           if (sr-sales <= ws-300-hundred) then
               compute ws-earned-calc rounded = sr-sales * (sr-rate /
                 ws-100)
               if (ws-earned-calc < sr-min) then
                   add ws-one to ws-min-bonus-total
               end-if.

      *This pragraph takes care of he bonus less than minimum 
      *calcualtion
        320-bonus-less-than.
               if (sr-sales > ws-300-hundred) then
                   compute ws-earned-calc rounded =
                     (sr-sales * (sr-rate / ws-100))
                     +
                     ((ws-15-quarter / ws-100) * (sr-sales - 
                     ws-300-hundred))
                   if (((ws-15-quarter / ws-100) * (sr-sales - 
                   ws-300-hundred))
                   >
                   sr-max)
                     then
                       add ws-one to ws-max-bonus-total
                   end-if
               end-if.

      *This paragraph takes care of the paid price calculation for the 
      *report
       330-paid-price-calculation.
           if (sr-sales > ws-300-hundred) then
               move ws-earned-calc to ws-paid-calc
               if (ws-earned-calc > sr-max) then
                   move sr-max to ws-paid-calc
               end-if
           end-if.

           if (sr-sales <= ws-300-hundred) then
               move ws-earned-calc to ws-paid-calc
               if (ws-earned-calc < sr-min) then
                   move sr-min to ws-paid-calc
               end-if
           end-if.

      *This paragraph takes care of the bonus earned or not check on 
      *each salesperson
       340-check-bonus-earned.
           if (sr-sales > ws-300-hundred) then
               move "BONUS EARNED" to ws-bonus-remark
               add ws-one to ws-bonus-total

           else
               if (sr-sales <= ws-300-hundred) then
                   move "BONUS NOT EARNED" to ws-bonus-remark
                   add ws-one to ws-no-bonus-total
               end-if
           end-if.

      *This paragraph takes care of the percentage caluclation in the 
      *summary of the report
       510-percent-calculation.
           compute ws-paid-earned-percent rounded =
             (ws-paid-earned-total * ws-100) / ws-people-total.
           compute ws-bonus-percent rounded =
             (ws-bonus-total * ws-100) / ws-people-total.
           compute ws-no-bonus-percent rounded =
             (ws-no-bonus-total * ws-100) / ws-people-total.
      *
       end program A3-SalesComm.