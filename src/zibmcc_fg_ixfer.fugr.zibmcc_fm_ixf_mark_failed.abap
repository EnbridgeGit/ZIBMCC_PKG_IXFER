FUNCTION ZIBMCC_FM_IXF_MARK_FAILED .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ARCHIVE_ID) TYPE  SAEARCHIVI
*"  TABLES
*"      ARC_DOC_IDS STRUCTURE  ZIBMCC_STRUCT_ARCHIVE_DOCID
*"----------------------------------------------------------------------

DATA:
      IXFER_STATUS TYPE ZIBMCC_IXFTAB.

SELECT * FROM ZIBMCC_IXFTAB INTO IXFER_STATUS
    FOR ALL ENTRIES IN ARC_DOC_IDS WHERE
             ( ARCHIV_ID = ARCHIVE_ID ) AND
             ( ARC_DOC_ID = ARC_DOC_IDS-ARC_DOC_ID ).

        DELETE ZIBMCC_IXFTAB FROM IXFER_STATUS.
ENDSELECT.

ENDFUNCTION.
