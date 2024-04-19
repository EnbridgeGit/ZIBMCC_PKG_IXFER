FUNCTION ZIBMCC_FM_SELECT_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DELIMITER) TYPE  CHAR1 DEFAULT '|'
*"     VALUE(ROWSTOSKIP) TYPE  INT4 DEFAULT 0
*"     VALUE(RESULTLIMIT) TYPE  INT4 DEFAULT 0
*"     VALUE(ARCHIVE_ID) TYPE  SAEARCHIVI OPTIONAL
*"     VALUE(START_DATE) TYPE  SAEABADATE DEFAULT 0
*"  EXPORTING
*"     VALUE(ROWCOUNT) TYPE  INT4
*"     VALUE(RESULTSIZE) TYPE  INT4
*"  TABLES
*"      SELECTCLAUSE STRUCTURE  ZIBMCC_STRUCT_OPEN_SQL_CLAUSE
*"      FROMCLAUSE STRUCTURE  ZIBMCC_STRUCT_OPEN_SQL_CLAUSE
*"      WHERECLAUSE STRUCTURE  ZIBMCC_STRUCT_OPEN_SQL_CLAUSE
*"      ORDERBYCLAUSE STRUCTURE  ZIBMCC_STRUCT_OPEN_SQL_CLAUSE
*"      DATA STRUCTURE  ZIBMCC_STRUCT_RESULT_DATA
*"  EXCEPTIONS
*"      UNKNOWN_DATABASE_OBJECT
*"      TABLE_HAS_NO_FIELDS
*"      TABLE_NOT_ACCESSIBLE
*"      INSUFFICIANT_ACCESS_AUTHORITY
*"      UNKNOWN_FIELD
*"      ROW_DATA_TOO_LARGE
*"      INCREMENTAL_MODE_NOT_SUPPORTED
*"      INCORRECT_IMPORT_PARAMETER
*"      INCORRECT_SELECT_CLAUSE
*"      INCORRECT_FROM_CLAUSE
*"      INCORRECT_WHERE_CLAUSE
*"      OPEN_SQL_SYNTAX_ERROR
*"      OPEN_SQL_SEMANTICS_ERROR
*"      UNEXPECTED_ERROR
*"----------------------------------------------------------------------

*"----------------------------------------------------------------------
*" declaration of object[s]
*"----------------------------------------------------------------------
  TYPE-POOLS: abap.
  CONSTANTS:
      maximumDataLength  TYPE  INT4  VALUE 2000.
  DATA:
      errorObject  TYPE REF TO  cx_root,
      dataSize  TYPE  INT4,            " work variable for number of rows in the 'data' table
      anyCount  TYPE  INT4,            " work variable to handle rows to be skipped or returned
      fieldCount  TYPE  INT4,          " number of fields / columns in the 'select clause' input
      columnName  TYPE  SO_TEXT,       " name of one of the columns from the 'select clause' structure
      fieldName   TYPE  string,        " name of the current field
      fieldTable  LIKE TABLE OF  ZIBMCC_STRUCT_OPEN_SQL_CLAUSE,
                                       " list of field descriptions
      fieldDescriptionTable  TYPE  abap_component_tab,
                                       " the description of a field of the data structure
                                       " used to capture the 'SAP' data
      fieldDescription  TYPE  abap_componentdescr,
                                      " reference to a data description of the fields
      fieldDataDescrRef  TYPE REF TO  abap_componentdescr,
      typeDescription  TYPE REF TO  cl_abap_typedescr,
                                       " the 'cl_abap_structdescr' object covering the row description
      rowStructureDescription  TYPE REF TO  cl_abap_structdescr,
      rowReference  TYPE REF TO  DATA, " reference to the actual row data structure
      returnRowString  TYPE  string,   " the string containing the return row values
      dataFieldString  TYPE  string,   " the string containing the value of one field
      rowLength  TYPE  INT4,           " length of one row of data
      dataLine  LIKE  DATA,            " the actual data line inserted into the 'result data' table
*     fieldRow  LIKE LINE OF  fieldTable,
      clauseRow  TYPE  ZIBMCC_STRUCT_OPEN_SQL_CLAUSE,
      fromClauseString  TYPE  string,
*
      startDate  TYPE  SAEABADATE,
      transferStatus  TYPE  ZIBMCC_IXFTAB.

  FIELD-SYMBOLS:
      <dataRow>  TYPE  ANY,            " the 'field symbol' used by the 'select' statement to put the data into
      <dataField>  TYPE  ANY,          " the 'field symbol' for one data field
* Modification for Spectra, 1/5/12 - Replace empty Date in CPUDT
      <fieldName1>  TYPE  ANY.          " the 'field symbol' for data field name
* end 1/5/12 Modification
  TRY.
*   "-------------------------------------------------------------------
*   " check the 'integer' input parameter[s]
*   "-------------------------------------------------------------------
    IF ( ( rowsToSkip < 0 ) OR ( resultLimit < 0 )
       ).                              " => parameter[s] must not be negative
      RAISE INCORRECT_IMPORT_PARAMETER.
    ENDIF.

*   "-------------------------------------------------------------------
*   " check the 'tables' input parameter[s]
*   "-------------------------------------------------------------------
    IF ( ARCHIVE_ID IS NOT INITIAL
       ).                              " => 'incremental mode'
      DESCRIBE TABLE WHERECLAUSE LINES anyCount.
      IF ( anyCount = 0
         ).                            " => the 'where clause' must not be empty
        RAISE INCORRECT_WHERE_CLAUSE.
      ENDIF.
    ENDIF.                             " an empty clause does not cause a syntax error
                                       " but an empty result
    DESCRIBE TABLE SELECTCLAUSE LINES fieldCount.
    IF ( fieldCount > 0
       ).                              " => copy the 'select clause' into table 'fieldTable'
      LOOP AT SELECTCLAUSE INTO clauseRow.
                                       " verify that every 'column name' is fully qualified
        FIND '~' IN clauseRow RESPECTING CASE.
        IF ( sy-subrc = 0
           ).                          " write the current row into 'fieldTable'
          INSERT clauseRow INTO TABLE fieldTable.
        ELSE.                          " => no 'column selector' ['~'] was found
          RAISE INCORRECT_SELECT_CLAUSE.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ( ARCHIVE_ID IS NOT INITIAL
       ).                              " => 'incremental' mode
      DESCRIBE TABLE FROMCLAUSE LINES anyCount.
                                       " the 'from clause' has to consist of
      IF ( anyCount <> 1
         ).                            " one row containing the table name
        RAISE INCORRECT_FROM_CLAUSE.
      ENDIF.

      "-----------------------------------------------------------------
      " get the name of the 'single' input table from the 'from clause'
      " check that it is an element of {'TOA01','TOA02','TOA03','TOAHR','TOADL'}
      "-----------------------------------------------------------------
      " append two 'fully qualified' fields for 'archive document id'
      " and 'archive date' to the internal table 'fieldTable'
      "-----------------------------------------------------------------
      DATA tableName  LIKE LINE OF  FROMCLAUSE.

      READ TABLE FROMCLAUSE INTO tableName.

      CONCATENATE tableName '~ARC_DOC_ID' INTO fieldName.

      IF ( ( tableName = 'TOA01' ) OR ( tableName = 'TOA02' ) OR
           ( tableName = 'TOA03' ) OR ( tableName = 'TOAHR' )
         ).
        APPEND fieldName TO fieldTable.
        CONCATENATE tableName '~AR_DATE' INTO fieldName.
        APPEND fieldName TO fieldTable.

      ELSEIF ( tableName = 'TOADL' ).
        APPEND fieldName TO fieldTable.
        CONCATENATE tableName '~DATUM' INTO fieldName.
        APPEND fieldName TO fieldTable.

      ELSE.                            " => incorrect 'from clause' for 'incremental' mode
        RAISE INCORRECT_FROM_CLAUSE.
      ENDIF.

      CLEAR fieldName.
    ENDIF.

    CLEAR anyCount.

*   "-------------------------------------------------------------------
*   " concatenate the 'from clause' into a 'string'
*   " a remote 'select' statement using a 'from clause' table does not work
*   "-------------------------------------------------------------------
    fromClauseString = ''.
    LOOP AT FROMCLAUSE INTO clauseRow.
      CONCATENATE fromClauseString clauseRow-TEXT INTO fromClauseString.
    ENDLOOP.

    IF ( fieldCount = 0 ) AND ( ( ARCHIVE_ID IS INITIAL )
       ).                              " => execution of 'SELECT COUNT(*)'
*     "-----------------------------------------------------------------
*     " start the 'select' operation
*     "-----------------------------------------------------------------
      TRY.
        SELECT COUNT(*) FROM (fromClauseString) INTO anyCount
        WHERE (WHERECLAUSE).

        CATCH cx_sy_dynamic_osql_syntax INTO errorObject.
          RAISE OPEN_SQL_SYNTAX_ERROR.

        CATCH cx_sy_dynamic_osql_semantics INTO errorObject.
          RAISE OPEN_SQL_SEMANTICS_ERROR.
      ENDTRY.

    ELSE.
*     "-----------------------------------------------------------------
*     " create data structure with string names s1-sn, n being the number of
*     " fields selected, first we create the description of the data structure
*     "-----------------------------------------------------------------
                                       " read row-by-row from table 'fieldTable' into 'clauseRow'
      LOOP AT fieldTable INTO clauseRow.
        fieldName = sy-tabix.          " write the current table index to the 'fieldName' string
                                       " write the 'string' prefix to the 'fieldName' string
        CONCATENATE 'string' fieldName INTO fieldName.
        CONDENSE fieldName.            " remove 'white space[s]' from the 'fieldName' string
                                       " assign it to the 'name' field of the 'field description' 'RTTI' structure
        fieldDescription-name = fieldName.

        columnName = clauseRow-TEXT.
        REPLACE FIRST OCCURRENCE OF SUBSTRING '~' IN columnName
                                    WITH '-' RESPECTING CASE.

*       fieldDescription-type ?= cl_abap_typedescr=>describe_by_name( columnName ).

        CALL METHOD  cl_abap_typedescr=>describe_by_name
          EXPORTING  P_NAME         = columnName
          RECEIVING  P_DESCR_REF    = typeDescription
          EXCEPTIONS TYPE_NOT_FOUND = 1
                     OTHERS         = 2.

        IF ( sy-subrc <> 0 ).
          RAISE UNKNOWN_FIELD.
        ENDIF.

*       IF ( typeDescription IS NOT INITIAL ).
        IF ( typeDescription IS BOUND ).
          fieldDescription-type ?= typeDescription.
        ENDIF.

        APPEND fieldDescription TO fieldDescriptionTable.
      ENDLOOP.

      TRY.
        rowStructureDescription = cl_abap_structdescr=>create( fieldDescriptionTable ).

        CATCH cx_sy_struct_creation INTO errorObject.
          RAISE UNEXPECTED_ERROR.
      ENDTRY.

*     TRY.
*     "-----------------------------------------------------------------
*     " create the actual data structure in memory
*     "-----------------------------------------------------------------
      CREATE DATA rowReference TYPE HANDLE rowStructureDescription.

*     "-----------------------------------------------------------------
*     " assign it to the field symbol used by the 'select' statement
*     "-----------------------------------------------------------------
      ASSIGN rowReference->* TO <dataRow>.
*
*       CATCH cx_sy_create_data_error INTO errorObject.
*         RAISE UNEXPECTED_ERROR.
*     ENDTRY.

      IF ( ARCHIVE_ID IS INITIAL
         ).                            " => 'non-incremental' mode
*       "---------------------------------------------------------------
*       " start the 'select' operation
*       "---------------------------------------------------------------
        TRY.                           " set 'anyCount' to maximum number of rows to be read
          anyCount = rowsToSkip + resultLimit.

          SELECT (fieldTable) FROM (fromClauseString) INTO <dataRow>
          WHERE (WHERECLAUSE) ORDER BY (ORDERBYCLAUSE).

            IF ( sy-dbcnt > rowsToSkip
               ).                      " start copying from 'sy-dbcnt'
              CLEAR returnRowString.
              DO fieldCount TIMES.
                ASSIGN COMPONENT sy-index OF STRUCTURE <dataRow> TO <dataField>.
                dataFieldString = <dataField>.
* Modification for Spectra, 1/5/12 - Replace empty Date in CPUDT
                READ TABLE fieldTable ASSIGNING <fieldname1> INDEX sy-index.
                If <fieldname1> EQ 'ZFIDV_INVVENDDET~CPUDT' AND dataFieldString EQ '00000000'.
                  dataFieldString = '19000101'.
                ENDIF.
* End 1/5/12 Modification
                CONCATENATE returnRowString DELIMITER dataFieldString INTO returnRowString.
              ENDDO.
              rowLength = NUMOFCHAR( returnRowString ).

              IF ( rowLength > maximumDataLength
                 ).                    " raise exception if data + delimiter[s]
                RAISE ROW_DATA_TOO_LARGE.
              ENDIF.                   " do not fit into the target row
              dataLine = returnRowString.
              INSERT dataLine INTO TABLE DATA.

              IF ( ( anyCount > 0 ) AND ( sy-dbcnt = anyCount )
                 ).                    " => stop copying at position 'anyCount'
                EXIT.
              ENDIF.
            ENDIF.
          ENDSELECT.

          anyCount = sy-dbcnt.         " set 'anyCount' to number of rows read
                                       " set 'dataSize' to number of rows written to table 'DATA'
          IF ( rowsToSkip > 0
             ).
            IF ( anyCount > rowsToSkip
               ).
              dataSize = anyCount - rowsToSkip.
            ELSE.
              dataSize = 0.
            ENDIF.
          ELSE.
            dataSize = anyCount.
          ENDIF.

          CATCH cx_sy_dynamic_osql_syntax INTO errorObject.
*                returnRowString = errorObject->get_text( ).
*                MESSAGE returnRowString TYPE 'I'.
            RAISE OPEN_SQL_SYNTAX_ERROR.

          CATCH cx_sy_dynamic_osql_semantics INTO errorObject.
*                returnRowString = errorObject->get_text( ).
*                MESSAGE returnRowString TYPE 'I'.
            RAISE OPEN_SQL_SEMANTICS_ERROR.
        ENDTRY.

      ELSE.                            " => 'incremental' mode
        DATA:                          " index for 'archive document identifier'
            identifierIndex  TYPE  INT4,
                                       " index for 'archive date'
            dateIndex        TYPE  INT4.

        FIELD-SYMBOLS:
            <archiveDate>  TYPE  ZIBMCC_IXFTAB-AR_DATE,
            <documentId>  TYPE  ZIBMCC_IXFTAB-ARC_DOC_ID.

*       "---------------------------------------------------------------
*       " update the table 'ZIBMCC_IXFTAB'
*       "---------------------------------------------------------------
        SELECT AR_DATE FROM ZIBMCC_IXFTAB INTO startDate
          WHERE ( ARCHIV_ID = ARCHIVE_ID ) AND ( ARC_DOC_ID = '' ).
        ENDSELECT.

        IF ( START_DATE < startDate ).
          RAISE INCREMENTAL_MODE_NOT_SUPPORTED.
        ENDIF.

        IF ( START_DATE > startDate ).
          DELETE FROM ZIBMCC_IXFTAB
            WHERE ( ARCHIV_ID = ARCHIVE_ID ) AND ( AR_DATE < START_DATE ).
          transferStatus-ARCHIV_ID = ARCHIVE_ID.
          CLEAR transferStatus-ARC_DOC_ID.
          transferStatus-AR_DATE = START_DATE.
          INSERT INTO ZIBMCC_IXFTAB values transferStatus.
        ENDIF.

*       "---------------------------------------------------------------
*       " start the 'select' operation, the 'order by' clause will be ignored
*       "---------------------------------------------------------------
                                       " set index for 'archive document id'
        identifierIndex = fieldCount + 1.
                                       " set index for 'archive date'
        dateIndex = identifierIndex + 1.

        TRY.
          anyCount = resultLimit.      " set 'anyCount' to maximum number of rows to be read
                                       " the 'rowsToSkip' parameter will be ignored !!!
          SELECT (fieldTable) FROM (fromClauseString) INTO <dataRow>
          WHERE (WHERECLAUSE) AND
                ( ARC_DOC_ID NOT IN ( SELECT ARC_DOC_ID FROM ZIBMCC_IXFTAB
                                      WHERE ARCHIV_ID = ARCHIVE_ID ) ).
*           IF ( sy-dbcnt > rowsToSkip
*              ).                      " start copying from 'sy-dbcnt'
              CLEAR returnRowString.
              DO fieldCount TIMES.
                ASSIGN COMPONENT sy-index OF STRUCTURE <dataRow> TO <dataField>.
                dataFieldString = <dataField>.
* Modification for Spectra, 1/5/12 - Replace empty Date in PSOBT
                READ TABLE fieldTable ASSIGNING <fieldname1> INDEX sy-index.
                If <fieldname1> EQ 'ZFIDV_INVVENDDET~CPUDT' AND dataFieldString EQ '00000000'.
                  dataFieldString = '19000101'.
                ENDIF.
* End 1/5/12 Modification
                CONCATENATE returnRowString DELIMITER dataFieldString INTO returnRowString.
              ENDDO.
              rowLength = NUMOFCHAR( returnRowString ).

              IF ( rowLength > maximumDataLength
                 ).                    " raise exception if data + delimiter[s]
                RAISE ROW_DATA_TOO_LARGE.
              ENDIF.                   " do not fit into the target row
              dataLine = returnRowString.
              INSERT dataLine INTO TABLE DATA.

*             "---------------------------------------------------------
*             " insert an entry into table 'ZIBMCC_IXFTAB'
*             "---------------------------------------------------------
              ASSIGN COMPONENT identifierIndex OF STRUCTURE <dataRow> TO <documentId>.
              ASSIGN COMPONENT dateIndex OF STRUCTURE <dataRow> TO <archiveDate>.
              CLEAR transferStatus.

              transferStatus-ARCHIV_ID = ARCHIVE_ID.
              transferStatus-ARC_DOC_ID = <documentId>.
              transferStatus-AR_DATE = <archiveDate>.
              transferStatus-STATUS = 0.
              INSERT INTO ZIBMCC_IXFTAB VALUES transferStatus.

              IF ( ( anyCount > 0 ) AND ( sy-dbcnt = anyCount )
                 ).                    " => stop copying at position 'anyCount'
                EXIT.
              ENDIF.
*           ENDIF.
          ENDSELECT.

          anyCount = sy-dbcnt.         " set 'anyCount' to number of rows read
          dataSize = anyCount.         " set 'dataSize' to number of rows written to table 'DATA'

          CATCH cx_sy_dynamic_osql_syntax INTO errorObject.
            RAISE OPEN_SQL_SYNTAX_ERROR.

          CATCH cx_sy_dynamic_osql_semantics INTO errorObject.
            RAISE OPEN_SQL_SEMANTICS_ERROR.
        ENDTRY.
      ENDIF.                           " end of 'non-incremental' or 'incremental' mode
    ENDIF.                             " end of 'select count(*)'
                                       " <=> empty 'select clause' and 'non-incremental' mode
*   "-------------------------------------------------------------------
*   " set the 'integer' output parameter[s]
*   "-------------------------------------------------------------------
    rowCount = anyCount.
    resultSize = dataSize.

    CATCH cx_root INTO errorObject.
      TRY.
        returnRowString = errorObject->get_longtext( ).
        rowLength = NUMOFCHAR( returnRowString ).

        IF ( ( rowLength > 0 ) AND ( rowLength < maximumDataLength )
           ).                          " => write the error text to the 'result data' table
          dataLine = returnRowString.
          INSERT dataLine INTO TABLE DATA.
          IF ( sy-subrc = 0
             ).                        " => update the 'integer' output parameter[s]
            resultSize = resultSize + 1.
          ENDIF.
        ENDIF.

        CATCH cx_root INTO errorObject.
          RAISE UNEXPECTED_ERROR.
      ENDTRY.
      RAISE UNEXPECTED_ERROR.
  ENDTRY.

ENDFUNCTION.
