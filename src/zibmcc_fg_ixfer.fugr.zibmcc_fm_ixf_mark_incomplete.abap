FUNCTION ZIBMCC_FM_IXF_MARK_INCOMPLETE .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ARCHIVE_ID) TYPE  SAEARCHIVI OPTIONAL
*"  EXPORTING
*"     VALUE(NUM_ROWS_REMOVED) TYPE  INT4
*"  TABLES
*"      ARC_DOC_IDS STRUCTURE  ZIBMCC_STRUCT_ARCHIVE_DOCID
*"----------------------------------------------------------------------

DATA:
    IXFER_STATUS TYPE ZIBMCC_IXFTAB.

IF ARCHIVE_ID IS INITIAL.
    SELECT * FROM ZIBMCC_IXFTAB INTO IXFER_STATUS WHERE
                 ( STATUS = 0 ).
            NUM_ROWS_REMOVED = NUM_ROWS_REMOVED + 1.
            DELETE ZIBMCC_IXFTAB FROM IXFER_STATUS.
    ENDSELECT.
ELSE.
    SELECT * FROM ZIBMCC_IXFTAB INTO IXFER_STATUS WHERE
                 ( ARCHIV_ID = ARCHIVE_ID ) AND
                 ( STATUS = 0 ).
            NUM_ROWS_REMOVED = NUM_ROWS_REMOVED + 1.
            DELETE ZIBMCC_IXFTAB FROM IXFER_STATUS.
    ENDSELECT.
ENDIF.

ENDFUNCTION.