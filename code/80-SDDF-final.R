# ESS9-LV SDDF after fieldwork

# Documentation ####

# ESS9 Data Protocol ed. 1.4
# https://www.europeansocialsurvey.org/docs/round9/survey/ESS9_data_protocol_e01_4.pdf

# Syntaxes for computing outcome codes:
# "ESS9 Outcome code syntax.sps"
# https://essdata.nsd.uib.no/ESS9/intraweb/files/survey_documentation/ESS9%20Outcome%20code%20syntax.sps


# Options ####
options(encoding = "UTF-8")
options(max.print = 10e3)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)


# Packages ####
require(data.table)
require(haven)
require(openxlsx)


# Reset ####
rm(list = ls())
gc()


# Load data ####

data.sddf <- fread("data/ESS9_LV_SDDF.csv")

setorder(data.sddf, stratex1, stratim1, stratim2)

data.sddf

data.sddf[, .N, keyby = .(stratim1)]
data.sddf[, .N, keyby = .(stratex1, stratim1)]

data.sddf[, .N, keyby = .(stratim2)]
data.sddf[, .N, keyby = .(psu, stratim2)]

# Recode stratim1 - should be values 1 - 505
tab <- unique(data.sddf[, .(stratex1, psu, stratim1)])
setorder(tab, stratex1, stratim1)
tab

tab[, stratim1 := .I]

data.sddf[, stratim1 := NULL]

data.sddf <- merge(data.sddf, tab, by = c("stratex1", "psu"), sort = F)

data.sddf[, .N, keyby = .(stratim1)]
data.sddf[, .N, keyby = .(stratex1, stratim1)]


# Recode stratim2 - should be values 1 - 5
setorder(data.sddf, stratex1, stratim1, stratim2)

data.sddf[, stratim2 := rep(1:5, times = 505)]

data.sddf[, .N, keyby = .(stratim2)]
data.sddf[, .N, keyby = .(psu, stratim2)]




# Contact form data

data.cf <- read_spss("data/ESS9cf_LV_before-anon.sav")
setDT(data.cf)

data.main <- read_spss("data/ESS_R9_main_questionnaire_data_file_before_anon.sav",
                       col_select = "idno")
setDT(data.main)


# IDNO correction

data.main[, .(idno)]
data.sddf[, .(idno)]
data.cf[, .(idno)]

data.sddf[idno < 260000L, idno := 260000L + idno]


# Contact forms ####

# CNTRY
# Country
data.cf[, .N, keyby = .(cntry)]

# IDNO
# Respondent’s identification number
data.cf[, .N, keyby = .(idno)]

# RESULB1 - Result of the first visit
# RESULB2 - Result of the second visit
# RESULB3 - Result of the third visit
# RESULB4 - Result of the fourth visit
# RESULB5 - Result of the fifth visit
# RESULB6 - Result of the sixth visit
# RESULB7 - Result of the seventh visit
# RESULB8 - Result of the eighth visit
# RESULB9 - Result of the ninth visit
# RESULBN - Result of the n-th visit

# 01 - Completed interview
# 02 - Partial interview
# 03 - Contact with unidentified person
# 04 - Contact with respondent but no interview
# 05 - Contact with someone other than respondent
# 06 - No contact at all
# 07 - Invalid address
# 08 - Other information about sample unit
# 99 - Not available

grep("^resulb", names(data.cf), value = T)
data.cf[, .N, keyby = .(resulb1)]
data.cf[, .N, keyby = .(resulb2)]

# OUTNIC1 - Outcome when there was no interview (visit 1)
# OUTNIC2 - Outcome when there was no interview (visit 2)
# OUTNIC3 - Outcome when there was no interview (visit 3)
# OUTNIC4 - Outcome when there was no interview (visit 4)
# OUTNIC5 - Outcome when there was no interview (visit 5)
# OUTNIC6 - Outcome when there was no interview (visit 6)
# OUTNIC7 - Outcome when there was no interview (visit 7)
# OUTNIC8 - Outcome when there was no interview (visit 8)
# OUTNIC9 - Outcome when there was no interview (visit 9)
# OUTNICN - Outcome when there was no interview (visit n)

# 01 - An appointment was made
# 02 - Refusal of target respondent
# 03 - Refusal by proxy (family, acquaintance)
# 04 - Someone refused, not sure if respondent
# 05 - Respondent not available/away
# 06 - Respondent mentally/physically not able, ill/sick (short term)
# 07 - Respondent mentally/physically not able, ill/sick (long term)
# 08 - Respondent deceased
# 09 - Respondent moved abroad
# 10 - Respondent moved, unsure whether abroad
# 11 - Respondent moved, within country
# 12 - Language barrier
# 13 - Other
# 66 - Not applicable
# 99 - Not available

grep("^outnic", names(data.cf), value = T)
data.cf[, .N, keyby = .(outnic1)]
data.cf[, .N, keyby = .(outnic2)]

# OUTINVAL
# Outcome address invalid

# 01 - The house/address is derelict or demolished
# 02 - The home is not yet built or not ready for occupation
# 03 - The address is not occupied (empty, second home, seasonal home, ...)
# 04 - The address is not residential: only purpose is business, industrial
# 05 - The address is not residential: institution (retirement home, hospital, military unit, monastery, ...)
# 06 - The address is not traceable. The information I was given is insufficient
# 07 - Other
# 66 - Not applicable
# 99 - Not available

data.cf[, .N, keyby = .(outinval)]

# INTERVA
# Interview information for the sample unit

# 1 - Complete and valid interview related to CF
# 2 - Interview incomplete
# 3 - Interview invalid
# 4 - No interview because of opt out list
# 5 - No interview for other reason
# 9 - Not available

data.cf[, .N, keyby = .(interva)]

# DEFECTCF
# Contact form information for the sample unit

# 1 - Contact form filled in by interviewer
# 2 - Contact form missing

data.cf[, .N, keyby = .(defectcf)]


data.cf[, .N, keyby = .(numhh)]
data.cf[, .N, keyby = .(hhselect)]
data.cf[, .N, keyby = .(nhhmem)]


# get file '<<file>>'
#     /KEEP CNTRY IDNO RESULB1 to RESULB69 outnic1 to outnic69 outinval interva defectcf.
varlist <- grep("cntry|idno|resulb|outnic|outinval|interva|defectcf|numhh|hhselect|nhhmem",
                names(data.cf), value = T)
length(varlist)

data.cf <- data.cf[, ..varlist]

lapply(data.cf, class)

# DO IF (SYSMIS(RESULB1) = 1 or missing(RESULB1) = 1 or ANY(RESULB1,66,99)).
#     COMPUTE RESULB1 = 55.
# END IF.
data.cf[, .N, keyby = .(resulb1)]
data.cf[is.na(resulb1) | resulb1 == 66 | resulb1 == 99, resulb1 := 55]

# SORT CASES BY IDNO(A).
# EXECUTE.
setorder(data.cf, idno)

# * Reshape the data: from one observation pr respondent to one observation pr
# contact attempt pr respondent.
# VARSTOCASES
#     /MAKE RESULT FROM RESULB1 TO RESULB69
#     /MAKE OUTN FROM OUTNIC1 TO OUTNIC69
#     /INDEX=ATTEMPT
#     /KEEP=IDNO CNTRY OUTINVAL INTERVA DEFECTCF.

data.cf <- melt.data.table(data = data.cf,
                           id.vars = c("idno", "cntry",
                                       "numhh", "hhselect", "nhhmem",
                                       "outinval", "interva", "defectcf"))

data.cf[, attempt := as.integer(gsub("[a-z]", "", variable))]

data.cf[, variable := sub("resulb[0-9]*", "result", variable)]
data.cf[, variable := sub("outnic[0-9]*", "outn", variable)]

data.cf <- dcast.data.table(data = data.cf,
                            formula = ... ~ variable,
                            value.var = "value")

# RECODE OUTN (66=SYSMIS) (99=SYSMIS).
data.cf[, .N, keyby = .(outn)]
data.cf[outn == 66 | outn == 99, outn := NA]

# SELECT IF (RESULT<65).
# EXECUTE.
data.cf[, .N, keyby = .(result)]
data.cf <- data.cf[result < 65]

# * Make variables based on OUTN, showing numbers of: Refusal by respondent,
# Refusal by proxy, Refusal before selection (household or adress refusal).
# DO IF (ATTEMPT = 1 and SYSMIS(OUTN)=1).
#     COMPUTE REF_RESP = 0.
#     COMPUTE REF_PROX = 0.
#     COMPUTE REF_BEFS = 0.
# ELSE IF (ATTEMPT = 1 and SYSMIS(OUTN)=0).
#     COMPUTE REF_RESP = (OUTN=2).
#     COMPUTE REF_PROX = (OUTN=3).
#     COMPUTE REF_BEFS = (OUTN=4).
# ELSE IF SYSMIS(OUTN)=1.
#     COMPUTE REF_RESP = lag(REF_RESP,1).
#     COMPUTE REF_PROX = lag(REF_PROX,1).
#     COMPUTE REF_BEFS = lag(REF_BEFS,1).
# ELSE.
#     COMPUTE REF_RESP = lag(REF_RESP,1) + (OUTN = 2).
#     COMPUTE REF_PROX = lag(REF_PROX,1) + (OUTN = 3).
#     COMPUTE REF_BEFS = lag(REF_BEFS,1) + (OUTN = 4).
# END IF.
# EXECUTE.

varlist <- paste("ref", c("resp", "prox", "befs"), sep = "_")
data.cf[, c(varlist) := lapply(2:4, function(x) sum(outn == x, na.rm = T)),
        by = .(idno)]

# data.cf[idno %in% data.cf[ref_resp == max(ref_resp), idno]]
# data.cf[idno %in% data.cf[ref_prox == max(ref_prox), idno]]
# data.cf[idno %in% data.cf[ref_befs == max(ref_befs), idno]]


# * New dataset: Collapse dataset to one observation per respondent. Generate variable showing number of attempts.
# DATASET DECLARE max.
# AGGREGATE
#     /OUTFILE='max'
#     /BREAK=idno
#     /ATTEMPT_last=LAST(ATTEMPT).
#
# * Keep only data from last attempt in original dataset.
# MATCH FILES /FILE=*
#     /TABLE='max'
#     /BY idno.
# SELECT IF (ATTEMPT = ATTEMPT_last).
# MATCH FILES FILE=*
#     /DROP=ATTEMPT_last.

data.cf[, attempt_last := max(attempt), by = .(idno)]
data.cf <- data.cf[attempt == attempt_last]
data.cf[, attempt_last := NULL]


# * Prioritzed refuals variable.
# DO IF (REF_RESP GE 1).
#     COMPUTE REF = 1.
# ELSE IF (REF_PROX GE 1).
#     COMPUTE REF = 2.
# ELSE IF (REF_BEFS GE 1).
#     COMPUTE REF = 3.
# ELSE.
#     COMPUTE REF= 0.
# END IF.
# EXECUTE.

data.cf[ref_resp >= 1L, ref := 1L]
data.cf[is.na(ref) & ref_prox >= 1L, ref := 2L]
data.cf[is.na(ref) & ref_befs >= 1L, ref := 3L]
data.cf[is.na(ref), ref := 0L]
# data.cf[, .N, keyby = .(ref)]


# * Preliminary mapping into outcome codes.
# DO IF (DEFECTCF=2).
# 	COMPUTE FINALCODE = 0.  /*CONTACT FORMS missing.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=1 AND RESULT=1).
# 	COMPUTE FINALCODE = 10. /*VALID INTERVIEW.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=2 AND RESULT=1).
# 	COMPUTE FINALCODE = 10. /*VALID INTERVIEW.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=2 AND RESULT=2).
# 	COMPUTE FINALCODE = 11. /*PARTIAL INTERVIEW.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=3).
# 	COMPUTE FINALCODE = 12. /*INVALID INTERVIEW.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=4).
# 	COMPUTE FINALCODE = 30. /*OPT OUT LIST.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 1).
# 	COMPUTE FINALCODE = 31. /*BROKEN APPOINTMENT.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 2).
# 	COMPUTE FINALCODE = 32. /*REFUSAL BY RESPONDENT.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND  ANY(OUTN,3,4) AND REF = 1).
# 	COMPUTE FINALCODE = 32. /*REFUSAL BY RESPONDENT.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 3).
# 	COMPUTE FINALCODE = 33. /*REFUSAL BY PROXY.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 4).
# 	COMPUTE FINALCODE = 34. /*REFUSAL BEFORE SELECTION.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 5).
# 	COMPUTE FINALCODE = 41. /*AWAY OR NOT AVAILABLE.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 6).
# 	COMPUTE FINALCODE = 42. /*MENTALLY OR PHYSICALLY UNABLE / ILL / SICK (SHORT TERM).*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 7).
# 	COMPUTE FINALCODE = 46. /*MENTALLY OR PHYSICALLY UNABLE / ILL / SICK (LONG TERM).*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 8).
# 	COMPUTE FINALCODE = 43. /*DECEASED.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 9).
# 	COMPUTE FINALCODE = 51. /*MOVED OUT OF THE COUNTRY.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 10).
# 	COMPUTE FINALCODE = 52. /*MOVED TO UNKNOWN DESTINATION.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 11).
# 	COMPUTE FINALCODE = 53. /*MOVED, STILL IN COUNTRY.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND OUTN = 12).
# 	COMPUTE FINALCODE = 44. /*LANGUAGE BARRIER.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND ANY(RESULT,3,4,5,8) AND (OUTN = 13 OR  SYSMIS(OUTN)=1)).
# 	COMPUTE FINALCODE = 45. /*OTHER.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=6 AND REF = 0).
# 	COMPUTE FINALCODE = 20. /*NONCONTACT.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=6 AND REF > 0).
# 	COMPUTE FINALCODE = 31 + REF.
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=7 AND OUTINVAL=1).
# 	COMPUTE FINALCODE =  61. /*DERELICT OR DEMOLISHED HOUSE.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=7 AND OUTINVAL=2).
# 	COMPUTE FINALCODE =  62. /*NOT YET BUILD, NOT READY FOR OCCUPATION.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=7 AND OUTINVAL=3).
# 	COMPUTE FINALCODE =  63. /*NOT OCCUPIED.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=7 AND OUTINVAL=4).
# 	COMPUTE FINALCODE =  64. /*ADDRESS NOT RESIDENTIAL: BUSINESS.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=7 AND OUTINVAL=5).
# 	COMPUTE FINALCODE =  65. /*ADDRESS NOT RESIDENTIAL: INSTITUTION.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=7 AND OUTINVAL=6).
# 	COMPUTE FINALCODE =  54. /*ADDRESS NOT TRACEALBE.*/
# ELSE IF (DEFECTCF=1 AND INTERVA=5 AND RESULT=7 AND OUTINVAL=7).
# 	COMPUTE FINALCODE =  67. /*OTHER INELIGIBLE.*/
# ELSE.
# 	COMPUTE FINALCODE = 88. /*UNDEFINED.*/
# END IF.
# EXECUTE.

data.cf[defectcf == 2, finalcode := 0L] # CONTACT FORMS missing.
data.cf[is.na(finalcode) & defectcf == 1 & interva %in% 1:2 & result == 1,
        finalcode := 10L] # valid interview.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 2 & result == 2,
        finalcode := 11L] # partial interview.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 3,
        finalcode := 12L] # invalid interview.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 4,
        finalcode := 30L] # opt out list.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 1,
        finalcode := 31L] # broken appointment.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) &
          (outn == 2 | (outn %in% 3:4 & ref == 1)),
        finalcode := 32L] # refusal by respondent.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 3,
        finalcode := 33L] # refusal by proxy.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 4,
        finalcode := 34L] # refusal before selection.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 5,
        finalcode := 41L] # away or not available.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 6,
        finalcode := 42L] # mentally or physically unable / ill / sick (short term).
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 7,
        finalcode := 46L] # mentally or physically unable / ill / sick (long term).
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 8,
        finalcode := 43L] # deceased.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 9,
        finalcode := 51L] # moved out of the country.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 10,
        finalcode := 52L] # moved to unknown destination.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 11,
        finalcode := 53L] # moved, still in country.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) & outn == 12,
        finalcode := 44L] # language barrier.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result %in% c(3,4,5,8) &
          (outn == 13 | is.na(outn)),
        finalcode := 45L] # other.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 6 & ref == 0,
        finalcode := 20L] # noncontact.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 6 & ref > 0,
        finalcode := 31L + ref] # noncontact.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 7 & outinval == 1,
        finalcode := 61L] # derelict or demolished house.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 7 & outinval == 2,
        finalcode := 62L] # not yet build, not ready for occupation.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 7 & outinval == 3,
        finalcode := 63L] # not occupied.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 7 & outinval == 4,
        finalcode := 64L] # address not residential: business.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 7 & outinval == 5,
        finalcode := 65L] # address not residential: institution.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 7 & outinval == 6,
        finalcode := 54L] # address not tracealbe.
data.cf[is.na(finalcode) & defectcf == 1 & interva == 5 & result == 7 & outinval == 7,
        finalcode := 67L] # other ineligible.
data.cf[is.na(finalcode),
        finalcode := 88L] # undefined.


# *Variable with NTS (string) codes.
# string codeNTS (A80).
# EXECUTE.
# 
# DO IF any(FINALCODE,0).
#     COMPUTE codeNTS = '-: Contact form missing'.
# ELSE IF any(FINALCODE,10,11).
#     COMPUTE codeNTS = 'X: (partial) Interview'.
# ELSE IF any(FINALCODE,12).
#     COMPUTE codeNTS = 'W: Invalid interview'.
# ELSE IF any(FINALCODE,30).
#     COMPUTE codeNTS = 'D: Refusal because of drop out list'.
# ELSE IF any(FINALCODE,31,41,45).
#     COMPUTE codeNTS = 'H: Broken appointment, respondent unavailable, no interviews for other reasons'.
# ELSE IF any(FINALCODE,32).
#     COMPUTE codeNTS = 'B: Refusal by respondent'.
# ELSE IF any(FINALCODE,33,34).
#     COMPUTE codeNTS = 'C: Refusal by proxy or before selection'.
# ELSE IF any(FINALCODE,42,46).
#     COMPUTE codeNTS = 'G: Mentally / physically unable / ill / sick (short and long term)'.
# ELSE IF any(FINALCODE,43).
#     COMPUTE codeNTS = 'N: R deceased'.
# ELSE IF any(FINALCODE,51).
#     COMPUTE codeNTS = 'M: R moved out of the country'.
# ELSE IF any(FINALCODE,52,53,20).
#     COMPUTE codeNTS = 'E: Noncontact or moved to unknown destination or still in country'.
# ELSE IF any(FINALCODE,44).
#     COMPUTE codeNTS = 'F: Language barrier'.
# ELSE IF any(FINALCODE,61,62,63).
#     COMPUTE codeNTS = 'K: Derelict or demolished house, not yet build or not occupied'.
# ELSE IF any(FINALCODE,64,65).
#     COMPUTE codeNTS = 'J: Address not residential'.
# ELSE IF any(FINALCODE,54).
#     COMPUTE codeNTS = 'I: Address not traceable'.
# ELSE IF any(FINALCODE,67).
#     COMPUTE codeNTS = 'L: Other ineligible'.
# ELSE IF any(FINALCODE,88).
#     COMPUTE codeNTS = 'Undefined'.
# END IF.
# EXECUTE.

data.cf[finalcode == 0L,
        codents := '-: contact form missing']
data.cf[is.na(codents) & finalcode %in% c(10,11),
        codents := 'x: (partial) interview']
data.cf[is.na(codents) & finalcode == 12L,
        codents := 'w: invalid interview']
data.cf[is.na(codents) & finalcode == 30L,
        codents := 'd: refusal because of drop out list']
data.cf[is.na(codents) & finalcode %in% c(31,41,45),
        codents := 'h: broken appointment, respondent unavailable, no interviews for other reasons']
data.cf[is.na(codents) & finalcode == 32L,
        codents := 'b: refusal by respondent']
data.cf[is.na(codents) & finalcode %in% c(33,34),
        codents := 'c: refusal by proxy or before selection']
data.cf[is.na(codents) & finalcode %in% c(42,46),
        codents := 'g: mentally / physically unable / ill / sick (short and long term)']
data.cf[is.na(codents) & finalcode == 43L,
        codents := 'n: r deceased']
data.cf[is.na(codents) & finalcode == 51L,
        codents := 'm: r moved out of the country']
data.cf[is.na(codents) & finalcode %in% c(52,53,20),
        codents := 'e: noncontact or moved to unknown destination or still in country']
data.cf[is.na(codents) & finalcode == 44L,
        codents := 'f: language barrier']
data.cf[is.na(codents) & finalcode %in% c(61,62,63),
        codents := 'k: derelict or demolished house, not yet build or not occupied']
data.cf[is.na(codents) & finalcode %in% c(64,65),
        codents := 'j: address not residential']
data.cf[is.na(codents) & finalcode == 54L,
        codents := 'i: address not traceable']
data.cf[is.na(codents) & finalcode == 67L,
        codents := 'l: other ineligible']
data.cf[is.na(codents) & finalcode == 88L,
        codents := 'undefined']


# FREQUENCIES VARIABLES=FINALCODE
#     /ORDER=ANALYSIS.
# 
# FREQUENCIES VARIABLES=codeNTS
#     /ORDER=ANALYSIS.

data.cf[, .N, keyby = .(finalcode)]
data.cf[, .N, keyby = .(codents)]
data.cf[, .N, keyby = .(finalcode, codents)]


# OUTCOME ####

# FIELD OUTCOME
# F1.0
# Mandatory

# 1 Data in main data file OUTCOME:
# 2 Eligible non-respondent
# 3 Ineligible

# Summary field outcome.  Value = 1 for respondent (data in questionnaire data
# file), 2 for eligible non-respondent (refusal, non- contact, unable to be
# interviewed due to language, health, etc.), 3 for ineligible (outside of
# survey population: died, moved abroad, vacant address, etc.)

# Algorithm:
# OUTCOME=1 if data included in ms file
# OUTCOME=3 if no data in ms file and (at least) one call outcome in cf file in
# (43, 51, 61-67)
# OUTCOME=2 otherwise

ids <- data.main$idno

data.cf[idno %in% ids, outcome := 1L]
data.cf[!(idno %in% ids) & finalcode %in% c(43, 51, 61:67), outcome := 3L]
data.cf[is.na(outcome), outcome := 2L]

data.cf[, .N, keyby = .(outcome)]
length(ids)

data.cf[, .N, keyby = .(outcome, codents)]

data.cf[finalcode == 10, .N, keyby = .(outcome)]


# PROB3 ####
# F14.12
# Applies if there are at least 3 stages of selection.

# Selection probability at third stage (conditional upon first two stages).
# Numeric, positive, non-zero, 12 decimal places (or as many as necessary to
# provide at least 4 significant figures for all cases). For some designs,
# either PROB3 or PROB4 will correspond to the reciprocal of the number of
# persons aged 15 or over at the address. In this case the value should be blank
# for ineligibles (OUTCOME=3) and should be 9 for non-responding units (OUTCOME=2)
# where the number of persons aged 15+ was not established.

data.cf

data.cf[, .N, keyby = .(numhh)]
data.cf[numhh == 999, numhh := NA]

data.cf[, .N, keyby = .(hhselect)]
data.cf[numhh == 1, hhselect := numhh]
data.cf[is.na(numhh), hhselect := NA]

data.cf[, .N, keyby = .(nhhmem)]
data.cf[nhhmem == 66, nhhmem := NA]

data.cf[, prob3 := hhselect / numhh / nhhmem]
data.cf[outcome == 2 & is.na(nhhmem), prob3 := 9]
data.cf[outcome == 3, prob3 := NA]

data.cf[outcome < 3, as.list(summary(prob3)), keyby = .(outcome)]

data.cf[outcome == 2 & prob3 < 9]
data.cf[numhh > 1]


# Final SDDF ####

data.sddf[, c("prob3", "outcome") := NULL]

data.sddf <- merge(data.sddf, data.cf[, .(idno, outcome, prob3)], by = "idno")

data.sddf[, .N, keyby = .(outcome)]

data.sddf[, summary(prob3)]
data.sddf[prob3 < 9, summary(prob3)]


# cntry = LV
data.sddf[, cntry := "LV"]


setcolorder(data.sddf, c("idno", "cntry", "prob1", "prob2", "prob3",
                         "stratex1", "stratim1", "stratim2",
                         "strtval1", "strtval2", "psu", "outcome",
                         "frame1", "frame2", "frame3", "frame4"))

fwrite(data.sddf, file = "results/ESS9_LV_SDDF_final.csv")
write.xlsx(data.sddf, file = "results/ESS9_LV_SDDF_final.xlsx",
           headerStyle = createStyle(halign = "center"),
           firstRow = T,
           colWidths = "auto")
