

needed_variables_2003_2021  =
  data.frame(
    year_wave =   years_21st_century,
    ############## HEAD
    # ER21127 "BC3 WTR WORKED SINCE JAN 1 OF PRIOR YEAR" 
    # BC3. Have you done any work for money since January 1, 2001? Please include any type of work, no matter how small
    WORK_SINCE_PRIOR_YEAR_HE = psidR::getNamesPSID("ER21127",cwf,years= years_21st_century)$variable,
    # ER21369 "BC67 YRS LOOK WRK (H-U)"
    years_look_for_work_HE =  psidR::getNamesPSID("ER21369",cwf,years= years_21st_century)$variable, 
    # ER21171 "BC41 YRS PRES EMP (H-E)"  How many years' experience do you (HEAD) have altogether with your present employer?--YEARS FOR CURRENT MAIN JOB
    years_pres_emp_HE =  psidR::getNamesPSID("ER21171",cwf,years= years_21st_century)$variable, 
    # ER21370 "BC67 MOS LOOK WRK (H-U)" How long have you been looking for work?--MONTHS
    months_look_for_work_HE =  psidR::getNamesPSID("ER21370",cwf,years= years_21st_century)$variable, 
    # ER21359 "BC63 YR LAST WORKED"  In what month and year did you last work? [IF NECESSARY: What would be your best estimate?]-YEAR
    YR_LAST_WORKED_HE =  psidR::getNamesPSID("ER21359",cwf,years= years_21st_century)$variable,
    # ER21358 "BC63 MO LAST WORKED"  In what month and year did you last work? [IF NECESSARY: What would be your best estimate?]-MONTH
    MO_LAST_WORKED_HE =  psidR::getNamesPSID("ER21358",cwf,years= years_21st_century)$variable,
    ############## SPOUSE
    # ER21377 "DE3 WTR WORKED SINCE JAN 1 OF PRIOR YEAR"
    # DE3. Has she done any work for money since January 1, 2001? Please include any type of
    # work, no matter how small.
    WORK_SINCE_PRIOR_YEAR_SP = psidR::getNamesPSID("ER21377",cwf,years= years_21st_century)$variable,
    # ER21619 "DE67 YRS LOOK WRK (W-U)" DE67. 
    # How long has she been looking for work?--YEARS
    years_look_for_work_SP =  psidR::getNamesPSID("ER21619",cwf,years= years_21st_century)$variable, 
    # ER21421 "DE41 YRS PRES EMP (W-E)"
    # DE41. How many years' experience does your (wife/"WIFE") have altogether with her present
    # employer?--YEARS FOR CURRENT MAIN JOB
    years_pres_emp_SP =  psidR::getNamesPSID("ER21421",cwf,years= years_21st_century)$variable, 
    # ER21620 "DE67 MOS LOOK WRK (W-U)"
    # DE67. How long has she been looking for work?--MONTHS
    months_look_for_work_SP =  psidR::getNamesPSID("ER21620",cwf,years= years_21st_century)$variable,
    # ER21609 "DE63 YR LAST WORKED"
    # DE63. In what month and year did she last work? [IF NECESSARY: What would be your best guess?]--YEAR
    YR_LAST_WORKED_SP =  psidR::getNamesPSID("ER21609",cwf,years= years_21st_century)$variable,
    # ER21608 "DE63 MO LAST WORKED" DE63. In what month and year did she last work? [IF NECESSARY: What would be your best guess?]--MONTH
    MO_LAST_WORKED_SP =  psidR::getNamesPSID("ER21608",cwf,years= years_21st_century)$variable,
    
    #################### OTHER #################################################
    INTERVIEW_NUMBER=psidR::getNamesPSID("ER21002",cwf,years= years_21st_century)$variable,
    years_work_since_18_SP=psidR::getNamesPSID("ER23384",cwf,years= years_21st_century)$variable,
    START_M_1_SP=psidR::getNamesPSID("ER21379",cwf,years= years_21st_century)$variable,
    START_Y_1_SP=psidR::getNamesPSID("ER21380",cwf,years= years_21st_century)$variable,
    END_M_1_SP=psidR::getNamesPSID("ER21381",cwf,years= years_21st_century)$variable,
    END_Y_1_SP=psidR::getNamesPSID("ER21382",cwf,years= years_21st_century)$variable,
    START_M_2_SP=psidR::getNamesPSID("ER21435",cwf,years= years_21st_century)$variable,
    START_Y_2_SP=psidR::getNamesPSID("ER21436",cwf,years= years_21st_century)$variable,
    END_Y_2_SP=psidR::getNamesPSID("ER21438",cwf,years= years_21st_century)$variable,
    END_M_2_SP=psidR::getNamesPSID("ER21437",cwf,years= years_21st_century)$variable,
    START_M_3_SP=psidR::getNamesPSID("ER21467",cwf,years= years_21st_century)$variable,
    START_Y_3_SP=psidR::getNamesPSID("ER21468",cwf,years= years_21st_century)$variable,
    END_M_3_SP=psidR::getNamesPSID("ER21469",cwf,years= years_21st_century)$variable,
    END_Y_3_SP=psidR::getNamesPSID("ER21470",cwf,years= years_21st_century)$variable,
    START_M_4_SP=psidR::getNamesPSID("ER21499",cwf,years= years_21st_century)$variable,
    START_Y_4_SP=psidR::getNamesPSID("ER21500",cwf,years= years_21st_century)$variable,
    END_M_4_SP=psidR::getNamesPSID("ER21501",cwf,years= years_21st_century)$variable,
    END_Y_4_SP=psidR::getNamesPSID("ER21502",cwf,years= years_21st_century)$variable,
    
    jan_emp_SP=psidR::getNamesPSID("ER23702J8",cwf,years= years_21st_century)$variable,
    feb_emp_SP=psidR::getNamesPSID("ER23702J9",cwf,years= years_21st_century)$variable,
    mar_emp_SP=psidR::getNamesPSID("ER23702K1",cwf,years= years_21st_century)$variable,
    apr_emp_SP=psidR::getNamesPSID("ER23702K2",cwf,years= years_21st_century)$variable,
    may_emp_SP=psidR::getNamesPSID("ER23702K3",cwf,years= years_21st_century)$variable,
    jun_emp_SP=psidR::getNamesPSID("ER23702K4",cwf,years= years_21st_century)$variable,
    jul_emp_SP=psidR::getNamesPSID("ER23702K5",cwf,years= years_21st_century)$variable,
    aug_emp_SP=psidR::getNamesPSID("ER23702K6",cwf,years= years_21st_century)$variable,
    sep_emp_SP=psidR::getNamesPSID("ER23702K7",cwf,years= years_21st_century)$variable,
    oct_emp_SP=psidR::getNamesPSID("ER23702K8",cwf,years= years_21st_century)$variable,
    nov_emp_SP=psidR::getNamesPSID("ER23702K9",cwf,years= years_21st_century)$variable,
    dec_emp_SP=psidR::getNamesPSID("ER23702L1",cwf,years= years_21st_century)$variable,
    jan_un_SP=psidR::getNamesPSID("ER23702F6",cwf,years= years_21st_century)$variable,
    feb_un_SP=psidR::getNamesPSID("ER23702F7",cwf,years= years_21st_century)$variable,
    mar_un_SP=psidR::getNamesPSID("ER23702F8",cwf,years= years_21st_century)$variable,
    apr_un_SP=psidR::getNamesPSID("ER23702F9",cwf,years= years_21st_century)$variable,
    may_un_SP=psidR::getNamesPSID("ER23702G1",cwf,years= years_21st_century)$variable,
    jun_un_SP=psidR::getNamesPSID("ER23702G2",cwf,years= years_21st_century)$variable,
    jul_un_SP=psidR::getNamesPSID("ER23702G3",cwf,years= years_21st_century)$variable,
    aug_un_SP=psidR::getNamesPSID("ER23702G4",cwf,years= years_21st_century)$variable,
    sep_un_SP=psidR::getNamesPSID("ER23702G5",cwf,years= years_21st_century)$variable,
    oct_un_SP=psidR::getNamesPSID("ER23702G6",cwf,years= years_21st_century)$variable,
    nov_un_SP=psidR::getNamesPSID("ER23702G7",cwf,years= years_21st_century)$variable,
    dec_un_SP=psidR::getNamesPSID("ER23702G8",cwf,years= years_21st_century)$variable,
    jan_lf_SP=psidR::getNamesPSID("ER23702H2",cwf,years= years_21st_century)$variable,
    feb_lf_SP=psidR::getNamesPSID("ER23702H3",cwf,years= years_21st_century)$variable,
    mar_lf_SP=psidR::getNamesPSID("ER23702H4",cwf,years= years_21st_century)$variable,
    apr_lf_SP=psidR::getNamesPSID("ER23702H5",cwf,years= years_21st_century)$variable,
    may_lf_SP=psidR::getNamesPSID("ER23702H6",cwf,years= years_21st_century)$variable,
    jun_lf_SP=psidR::getNamesPSID("ER23702H7",cwf,years= years_21st_century)$variable,
    jul_lf_SP=psidR::getNamesPSID("ER23702H8",cwf,years= years_21st_century)$variable,
    aug_lf_SP=psidR::getNamesPSID("ER23702H9",cwf,years= years_21st_century)$variable,
    sep_lf_SP=psidR::getNamesPSID("ER23702J1",cwf,years= years_21st_century)$variable,
    oct_lf_SP=psidR::getNamesPSID("ER23702J2",cwf,years= years_21st_century)$variable,
    nov_lf_SP=psidR::getNamesPSID("ER23702J3",cwf,years= years_21st_century)$variable,
    dec_lf_SP=psidR::getNamesPSID("ER23702J4",cwf,years= years_21st_century)$variable,
    
    main_job_indicator_SP=psidR::getNamesPSID("ER21378",cwf,years= years_21st_century)$variable,
    emp_status_1_SP=psidR::getNamesPSID("ER21373",cwf,years= years_21st_century)$variable,
    jan_1_SP=psidR::getNamesPSID("ER21383",cwf,years= years_21st_century)$variable,
    feb_1_SP=psidR::getNamesPSID("ER21384",cwf,years= years_21st_century)$variable,
    mar_1_SP=psidR::getNamesPSID("ER21385",cwf,years= years_21st_century)$variable,
    apr_1_SP=psidR::getNamesPSID("ER21386",cwf,years= years_21st_century)$variable,
    may_1_SP=psidR::getNamesPSID("ER21387",cwf,years= years_21st_century)$variable,
    jun_1_SP=psidR::getNamesPSID("ER21388",cwf,years= years_21st_century)$variable,
    jul_1_SP=psidR::getNamesPSID("ER21389",cwf,years= years_21st_century)$variable,
    aug_1_SP=psidR::getNamesPSID("ER21390",cwf,years= years_21st_century)$variable,
    sep_1_SP=psidR::getNamesPSID("ER21391",cwf,years= years_21st_century)$variable,
    oct_1_SP=psidR::getNamesPSID("ER21392",cwf,years= years_21st_century)$variable,
    nov_1_SP=psidR::getNamesPSID("ER21393",cwf,years= years_21st_century)$variable,
    dec_1_SP=psidR::getNamesPSID("ER21394",cwf,years= years_21st_century)$variable,
    jan_2_SP=psidR::getNamesPSID("ER21439",cwf,years= years_21st_century)$variable,
    feb_2_SP=psidR::getNamesPSID("ER21440",cwf,years= years_21st_century)$variable,
    mar_2_SP=psidR::getNamesPSID("ER21441",cwf,years= years_21st_century)$variable,
    apr_2_SP=psidR::getNamesPSID("ER21442",cwf,years= years_21st_century)$variable,
    may_2_SP=psidR::getNamesPSID("ER21443",cwf,years= years_21st_century)$variable,
    jun_2_SP=psidR::getNamesPSID("ER21444",cwf,years= years_21st_century)$variable,
    jul_2_SP=psidR::getNamesPSID("ER21445",cwf,years= years_21st_century)$variable,
    aug_2_SP=psidR::getNamesPSID("ER21446",cwf,years= years_21st_century)$variable,
    sep_2_SP=psidR::getNamesPSID("ER21447",cwf,years= years_21st_century)$variable,
    oct_2_SP=psidR::getNamesPSID("ER21448",cwf,years= years_21st_century)$variable,
    nov_2_SP=psidR::getNamesPSID("ER21449",cwf,years= years_21st_century)$variable,
    dec_2_SP=psidR::getNamesPSID("ER21450",cwf,years= years_21st_century)$variable,
    jan_3_SP=psidR::getNamesPSID("ER21471",cwf,years= years_21st_century)$variable,
    feb_3_SP=psidR::getNamesPSID("ER21472",cwf,years= years_21st_century)$variable,
    mar_3_SP=psidR::getNamesPSID("ER21473",cwf,years= years_21st_century)$variable,
    apr_3_SP=psidR::getNamesPSID("ER21474",cwf,years= years_21st_century)$variable,
    may_3_SP=psidR::getNamesPSID("ER21475",cwf,years= years_21st_century)$variable,
    jun_3_SP=psidR::getNamesPSID("ER21476",cwf,years= years_21st_century)$variable,
    jul_3_SP=psidR::getNamesPSID("ER21477",cwf,years= years_21st_century)$variable,
    aug_3_SP=psidR::getNamesPSID("ER21478",cwf,years= years_21st_century)$variable,
    sep_3_SP=psidR::getNamesPSID("ER21479",cwf,years= years_21st_century)$variable,
    oct_3_SP=psidR::getNamesPSID("ER21480",cwf,years= years_21st_century)$variable,
    nov_3_SP=psidR::getNamesPSID("ER21481",cwf,years= years_21st_century)$variable,
    dec_3_SP=psidR::getNamesPSID("ER21482",cwf,years= years_21st_century)$variable,
    jan_4_SP=psidR::getNamesPSID("ER21503",cwf,years= years_21st_century)$variable,
    feb_4_SP=psidR::getNamesPSID("ER21504",cwf,years= years_21st_century)$variable,
    mar_4_SP=psidR::getNamesPSID("ER21505",cwf,years= years_21st_century)$variable,
    apr_4_SP=psidR::getNamesPSID("ER21506",cwf,years= years_21st_century)$variable,
    may_4_SP=psidR::getNamesPSID("ER21507",cwf,years= years_21st_century)$variable,
    jun_4_SP=psidR::getNamesPSID("ER21508",cwf,years= years_21st_century)$variable,
    jul_4_SP=psidR::getNamesPSID("ER21509",cwf,years= years_21st_century)$variable,
    aug_4_SP=psidR::getNamesPSID("ER21510",cwf,years= years_21st_century)$variable,
    sep_4_SP=psidR::getNamesPSID("ER21511",cwf,years= years_21st_century)$variable,
    oct_4_SP=psidR::getNamesPSID("ER21512",cwf,years= years_21st_century)$variable,
    nov_4_SP=psidR::getNamesPSID("ER21513",cwf,years= years_21st_century)$variable,
    dec_4_SP=psidR::getNamesPSID("ER21514",cwf,years= years_21st_century)$variable,
  
    ever_worked_SP = psidR::getNamesPSID("ER21607",cwf,years= years_21st_century)$variable,
    
    year_retired_SP=psidR::getNamesPSID("ER21376", cwf,years= years_21st_century)$variable,
    
    
    whyear_HE = psidR::getNamesPSID("ER77255", cwf,years= years_21st_century)$variable,
    whyear_SP = psidR::getNamesPSID("ER77276", cwf,years= years_21st_century)$variable,
    
    ######################## head #######################################
    ever_worked_HE = psidR::getNamesPSID("ER21357",cwf,years= years_21st_century)$variable,
    # 1. ER21128 "BC16-17 MAIN JOB INDICATOR" Status of 2003 Head's Job Number 1
    main_job_indicator_HE=psidR::getNamesPSID("ER21128",cwf,years= years_21st_century)$variable,
    # 2. "BC1 EMPLOYMENT STATUS-1ST MENTION" We would like to know about what you do--are you (HEAD) working now, looking for
    # work, retired, keeping house, a student, or what?--FIRST MENTION
    emp_status_1_HE=psidR::getNamesPSID("ER21123",cwf,years= years_21st_century)$variable,
    emp_status_2_HE=psidR::getNamesPSID("ER21124",cwf,years= years_21st_century)$variable,
    emp_status_3_HE=psidR::getNamesPSID("ER21125",cwf,years= years_21st_century)$variable,
    year_retired_HE=psidR::getNamesPSID("ER21126",cwf,years= years_21st_century)$variable,
    # 3. BC6. When did you (HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (this employer/yourself) more than once.--JANUARY 2002 FOR CURRENT OR MOST RECENT MAIN JOB
    # one year before
    jan_1_HE=psidR::getNamesPSID("ER21133",cwf,years= years_21st_century)$variable,
    feb_1_HE=psidR::getNamesPSID("ER21134",cwf,years= years_21st_century)$variable,
    mar_1_HE=psidR::getNamesPSID("ER21135",cwf,years= years_21st_century)$variable,
    apr_1_HE=psidR::getNamesPSID("ER21136",cwf,years= years_21st_century)$variable,
    may_1_HE=psidR::getNamesPSID("ER21137",cwf,years= years_21st_century)$variable,
    jun_1_HE=psidR::getNamesPSID("ER21138",cwf,years= years_21st_century)$variable,
    jul_1_HE=psidR::getNamesPSID("ER21139",cwf,years= years_21st_century)$variable,
    aug_1_HE=psidR::getNamesPSID("ER21140",cwf,years= years_21st_century)$variable,
    sep_1_HE=psidR::getNamesPSID("ER21141",cwf,years= years_21st_century)$variable,
    oct_1_HE=psidR::getNamesPSID("ER21142",cwf,years= years_21st_century)$variable,
    nov_1_HE=psidR::getNamesPSID("ER21143",cwf,years= years_21st_century)$variable,
    dec_1_HE=psidR::getNamesPSID("ER21144",cwf,years= years_21st_century)$variable,
    # 4. BC6. When did you (HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (this  employer/yourself) more than once.--JANUARY 2002 FOR JOB 2
    jan_2_HE=psidR::getNamesPSID("ER21189",cwf,years= years_21st_century)$variable,
    feb_2_HE=psidR::getNamesPSID("ER21190",cwf,years= years_21st_century)$variable,
    mar_2_HE=psidR::getNamesPSID("ER21191",cwf,years= years_21st_century)$variable,
    apr_2_HE=psidR::getNamesPSID("ER21192",cwf,years= years_21st_century)$variable,
    may_2_HE=psidR::getNamesPSID("ER21193",cwf,years= years_21st_century)$variable,
    jun_2_HE=psidR::getNamesPSID("ER21194",cwf,years= years_21st_century)$variable,
    jul_2_HE=psidR::getNamesPSID("ER21195",cwf,years= years_21st_century)$variable,
    aug_2_HE=psidR::getNamesPSID("ER21196",cwf,years= years_21st_century)$variable,
    sep_2_HE=psidR::getNamesPSID("ER21197",cwf,years= years_21st_century)$variable,
    oct_2_HE=psidR::getNamesPSID("ER21198",cwf,years= years_21st_century)$variable,
    nov_2_HE=psidR::getNamesPSID("ER21199",cwf,years= years_21st_century)$variable,
    dec_2_HE=psidR::getNamesPSID("ER21200",cwf,years= years_21st_century)$variable,
    # 5. BC6. When did you (HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (this employer/yourself) more than once.--JANUARY 2002 FOR JOB 3
    jan_3_HE=psidR::getNamesPSID("ER21221",cwf,years= years_21st_century)$variable,
    feb_3_HE=psidR::getNamesPSID("ER21222",cwf,years= years_21st_century)$variable,
    mar_3_HE=psidR::getNamesPSID("ER21223",cwf,years= years_21st_century)$variable,
    apr_3_HE=psidR::getNamesPSID("ER21224",cwf,years= years_21st_century)$variable,
    may_3_HE=psidR::getNamesPSID("ER21225",cwf,years= years_21st_century)$variable,
    jun_3_HE=psidR::getNamesPSID("ER21226",cwf,years= years_21st_century)$variable,
    jul_3_HE=psidR::getNamesPSID("ER21227",cwf,years= years_21st_century)$variable,
    aug_3_HE=psidR::getNamesPSID("ER21228",cwf,years= years_21st_century)$variable,
    sep_3_HE=psidR::getNamesPSID("ER21229",cwf,years= years_21st_century)$variable,
    oct_3_HE=psidR::getNamesPSID("ER21230",cwf,years= years_21st_century)$variable,
    nov_3_HE=psidR::getNamesPSID("ER21231",cwf,years= years_21st_century)$variable,
    dec_3_HE=psidR::getNamesPSID("ER21232",cwf,years= years_21st_century)$variable,
    # 6. BC6. When did you (HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (this employer/yourself) more than once.--JANUARY 2002 FOR JOB 4
    jan_4_HE=psidR::getNamesPSID("ER21253",cwf,years= years_21st_century)$variable,
    feb_4_HE=psidR::getNamesPSID("ER21254",cwf,years= years_21st_century)$variable,
    mar_4_HE=psidR::getNamesPSID("ER21255",cwf,years= years_21st_century)$variable,
    apr_4_HE=psidR::getNamesPSID("ER21256",cwf,years= years_21st_century)$variable,
    may_4_HE=psidR::getNamesPSID("ER21257",cwf,years= years_21st_century)$variable,
    jun_4_HE=psidR::getNamesPSID("ER21258",cwf,years= years_21st_century)$variable,
    jul_4_HE=psidR::getNamesPSID("ER21259",cwf,years= years_21st_century)$variable,
    aug_4_HE=psidR::getNamesPSID("ER21260",cwf,years= years_21st_century)$variable,
    sep_4_HE=psidR::getNamesPSID("ER21261",cwf,years= years_21st_century)$variable,
    oct_4_HE=psidR::getNamesPSID("ER21262",cwf,years= years_21st_century)$variable,
    nov_4_HE=psidR::getNamesPSID("ER21263",cwf,years= years_21st_century)$variable,
    dec_4_HE=psidR::getNamesPSID("ER21264",cwf,years= years_21st_century)$variable,
    # 7. BC8. Was there any time last year, in 2002, when you were unemployed and looking for work? 
    # [IF YES, ASK: When was that?] [IF DK WHEN, ASK: How much time was that in 2002?]--
    jan_un_HE=psidR::getNamesPSID("ER21324",cwf,years= years_21st_century)$variable,
    feb_un_HE=psidR::getNamesPSID("ER21325",cwf,years= years_21st_century)$variable,
    mar_un_HE=psidR::getNamesPSID("ER21326",cwf,years= years_21st_century)$variable,
    apr_un_HE=psidR::getNamesPSID("ER21327",cwf,years= years_21st_century)$variable,
    may_un_HE=psidR::getNamesPSID("ER21328",cwf,years= years_21st_century)$variable,
    jun_un_HE=psidR::getNamesPSID("ER21329",cwf,years= years_21st_century)$variable,
    jul_un_HE=psidR::getNamesPSID("ER21330",cwf,years= years_21st_century)$variable,
    aug_un_HE=psidR::getNamesPSID("ER21331",cwf,years= years_21st_century)$variable,
    sep_un_HE=psidR::getNamesPSID("ER21332",cwf,years= years_21st_century)$variable,
    oct_un_HE=psidR::getNamesPSID("ER21333",cwf,years= years_21st_century)$variable,
    nov_un_HE=psidR::getNamesPSID("ER21334",cwf,years= years_21st_century)$variable,
    dec_un_HE=psidR::getNamesPSID("ER21335",cwf,years= years_21st_century)$variable,
    # 8. BC7. Was there any time in 2001 or 2002 when you did not have a job and were not looking
    # for one? [IF YES, ASK: When was that?] [IF DK WHEN, ASK: How much time was that in 2002?]--JANUARY 2002
    jan_lf_HE=psidR::getNamesPSID("ER21343",cwf,years= years_21st_century)$variable,
    feb_lf_HE=psidR::getNamesPSID("ER21344",cwf,years= years_21st_century)$variable,
    mar_lf_HE=psidR::getNamesPSID("ER21345",cwf,years= years_21st_century)$variable,
    apr_lf_HE=psidR::getNamesPSID("ER21346",cwf,years= years_21st_century)$variable,
    may_lf_HE=psidR::getNamesPSID("ER21347",cwf,years= years_21st_century)$variable,
    jun_lf_HE=psidR::getNamesPSID("ER21348",cwf,years= years_21st_century)$variable,
    jul_lf_HE=psidR::getNamesPSID("ER21349",cwf,years= years_21st_century)$variable,
    aug_lf_HE=psidR::getNamesPSID("ER21350",cwf,years= years_21st_century)$variable,
    sep_lf_HE=psidR::getNamesPSID("ER21351",cwf,years= years_21st_century)$variable,
    oct_lf_HE=psidR::getNamesPSID("ER21352",cwf,years= years_21st_century)$variable,
    nov_lf_HE=psidR::getNamesPSID("ER21353",cwf,years= years_21st_century)$variable,
    dec_lf_HE=psidR::getNamesPSID("ER21354",cwf,years= years_21st_century)$variable,
    ############### Two years before
    # 1. Constructed variable from the Employment History Calendar (EHC): Whether Head was employed in 2001 (two years before year_wave)
    ehc_sum_HE=psidR::getNamesPSID("ER23702D2",cwf,years= years_21st_century)$variable,
    # 2. EMPLOYMENT: BC8. When did (you/HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (this employer/yourself) more than once.--JANUARY 2001
    jan_add_HE=psidR::getNamesPSID("ER23702D5",cwf,years= years_21st_century)$variable,
    feb_add_HE=psidR::getNamesPSID("ER23702D6",cwf,years= years_21st_century)$variable,
    mar_add_HE=psidR::getNamesPSID("ER23702D7",cwf,years= years_21st_century)$variable,
    apr_add_HE=psidR::getNamesPSID("ER23702D8",cwf,years= years_21st_century)$variable,
    may_add_HE=psidR::getNamesPSID("ER23702D9",cwf,years= years_21st_century)$variable,
    jun_add_HE=psidR::getNamesPSID("ER23702E1",cwf,years= years_21st_century)$variable,
    jul_add_HE=psidR::getNamesPSID("ER23702E2",cwf,years= years_21st_century)$variable,
    aug_add_HE=psidR::getNamesPSID("ER23702E3",cwf,years= years_21st_century)$variable,
    sep_add_HE=psidR::getNamesPSID("ER23702E4",cwf,years= years_21st_century)$variable,
    oct_add_HE=psidR::getNamesPSID("ER23702E5",cwf,years= years_21st_century)$variable,
    nov_add_HE=psidR::getNamesPSID("ER23702E6",cwf,years= years_21st_century)$variable,
    dec_add_HE=psidR::getNamesPSID("ER23702E7",cwf,years= years_21st_century)$variable,
    # 3. UNEMPLOYMENT BC8. Were there any times during 2001 when (you/HEAD) (were/was) not employed (not working at all for pay)? 
    # IF YES: (Were/Was) (you/he/she) looking for a job, or not looking (for a job)?--JANUARY 2001
    jan_un_add_HE=psidR::getNamesPSID("ER23702A3",cwf,years= years_21st_century)$variable,
    feb_un_add_HE=psidR::getNamesPSID("ER23702A4",cwf,years= years_21st_century)$variable,
    mar_un_add_HE=psidR::getNamesPSID("ER23702A5",cwf,years= years_21st_century)$variable,
    apr_un_add_HE=psidR::getNamesPSID("ER23702A6",cwf,years= years_21st_century)$variable,
    may_un_add_HE=psidR::getNamesPSID("ER23702A7",cwf,years= years_21st_century)$variable,
    jun_un_add_HE=psidR::getNamesPSID("ER23702A8",cwf,years= years_21st_century)$variable,
    jul_un_add_HE=psidR::getNamesPSID("ER23702A9",cwf,years= years_21st_century)$variable,
    aug_un_add_HE=psidR::getNamesPSID("ER23702B1",cwf,years= years_21st_century)$variable,
    sep_un_add_HE=psidR::getNamesPSID("ER23702B2",cwf,years= years_21st_century)$variable,
    oct_un_add_HE=psidR::getNamesPSID("ER23702B3",cwf,years= years_21st_century)$variable,
    nov_un_add_HE=psidR::getNamesPSID("ER23702B4",cwf,years= years_21st_century)$variable,
    dec_un_add_HE=psidR::getNamesPSID("ER23702B5",cwf,years= years_21st_century)$variable,
    # 4. BC7. Were there any times during 2001 when (you were/HEAD was) not working at all for pay
    # and not looking for work? [IF YES, ASK: When was that?] [IF DK WHEN, ASK: How much time was that in 2001?]--JANUARY 2001
    jan_lf_add_HE=psidR::getNamesPSID("ER23702B8",cwf,years= years_21st_century)$variable,
    feb_lf_add_HE=psidR::getNamesPSID("ER23702B9",cwf,years= years_21st_century)$variable,
    mar_lf_add_HE=psidR::getNamesPSID("ER23702C1",cwf,years= years_21st_century)$variable,
    apr_lf_add_HE=psidR::getNamesPSID("ER23702C2",cwf,years= years_21st_century)$variable,
    may_lf_add_HE=psidR::getNamesPSID("ER23702C3",cwf,years= years_21st_century)$variable,
    jun_lf_add_HE=psidR::getNamesPSID("ER23702C4",cwf,years= years_21st_century)$variable,
    jul_lf_add_HE=psidR::getNamesPSID("ER23702C5",cwf,years= years_21st_century)$variable,
    aug_lf_add_HE=psidR::getNamesPSID("ER23702C6",cwf,years= years_21st_century)$variable,
    sep_lf_add_HE=psidR::getNamesPSID("ER23702C7",cwf,years= years_21st_century)$variable,
    oct_lf_add_HE=psidR::getNamesPSID("ER23702C8",cwf,years= years_21st_century)$variable,
    nov_lf_add_HE=psidR::getNamesPSID("ER23702C9",cwf,years= years_21st_century)$variable,
    dec_lf_add_HE=psidR::getNamesPSID("ER23702D1",cwf,years= years_21st_century)$variable,
    
    #  How many years altogether have you (HEAD) worked for money since you were 18?
    years_work_since_18_HE=psidR::getNamesPSID("ER23476",cwf,years= years_21st_century)$variable,
    # 1. BC6. When did you (HEAD) start and when did you stop working for this employer? Please give me all of 
    # the start and stop dates if you have gone to work for (this
    # employer/yourself) more than once.--MONTH FOR CURRENT OR MOST RECENT MAIN JOB
    START_M_1_HE=psidR::getNamesPSID("ER21129",cwf,years= years_21st_century)$variable,
    START_Y_1_HE=psidR::getNamesPSID("ER21130",cwf,years= years_21st_century)$variable,
    # 2. BC6. When did you (HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (thisemployer/yourself) more than once.--MONTH FOR MOST RECENT MAIN JOB
    END_M_1_HE=psidR::getNamesPSID("ER21131",cwf,years= years_21st_century)$variable,
    END_Y_1_HE=psidR::getNamesPSID("ER21132",cwf,years= years_21st_century)$variable,
    
    # 3. BC6. When did you (HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (this employer/yourself) more than once--MONTH FOR JOB 2
    START_M_2_HE=psidR::getNamesPSID("ER21185",cwf,years= years_21st_century)$variable,
    START_Y_2_HE=psidR::getNamesPSID("ER21186",cwf,years= years_21st_century)$variable,
    # 4. BC6. When did you (HEAD) start and when did you stop working for this employer? Please
    # give me all of the start and stop dates if you have gone to work for (this employer/yourself) more than once--MONTH FOR JOB 2
    END_M_2_HE=psidR::getNamesPSID("ER21187",cwf,years= years_21st_century)$variable,
    END_Y_2_HE=psidR::getNamesPSID("ER21188",cwf,years= years_21st_century)$variable,
    
    START_M_3_HE=psidR::getNamesPSID("ER21217",cwf,years= years_21st_century)$variable,
    START_Y_3_HE=psidR::getNamesPSID("ER21218",cwf,years= years_21st_century)$variable,
    
    END_M_3_HE=psidR::getNamesPSID("ER21219",cwf,years= years_21st_century)$variable,
    END_Y_3_HE=psidR::getNamesPSID("ER21220",cwf,years= years_21st_century)$variable,
    
    START_M_4_HE=psidR::getNamesPSID("ER21249",cwf,years= years_21st_century)$variable,
    START_Y_4_HE=psidR::getNamesPSID("ER21250",cwf,years= years_21st_century)$variable,
    
    END_M_4_HE=psidR::getNamesPSID("ER21251",cwf,years= years_21st_century)$variable,
    END_Y_4_HE=psidR::getNamesPSID("ER21252",cwf,years= years_21st_century)$variable)
