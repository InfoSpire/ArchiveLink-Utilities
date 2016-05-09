*-----------------------------------------------------------------------
* Program: Utility to delete ArchiveLink entries
* Version: 1.0 (Apr. 25, 2016)
* Author : Martin Moosbrugger, InfoSpire LLC (www.infospire.net)
*-----------------------------------------------------------------------
* Description:
* This program deletes ArchiveLink entries from tables like TOA01 based
* on the selection screen criteria. Plus it can remove the document from
* the repository. For more details go to:
*     https://github.com/InfoSpire/ArchiveLink-Utilities
* Note: Don't run this program in a productive SAP system. Perform SAP
* data archiving for these entries instead: Archiving Object ARCHIVELNK.
* See SAP Note 1900309 - HowTo: Archiving ArchiveLink connection entries
*-----------------------------------------------------------------------
* Copyright 2016 InfoSpire LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*-----------------------------------------------------------------------
REPORT zbc_alink_delete_docs.
TYPE-POOLS: ABAP, SLIS.

************************************************************
* Data Declaration
************************************************************
CONSTANTS: zalv_yellow_int(4)   TYPE c VALUE 'C310',
           zalv_red_int(4)      TYPE c VALUE 'C610'.

TYPES: BEGIN OF t_link.
        INCLUDE STRUCTURE toav0.
TYPES:  msg_type     TYPE bapi_mtype,
        message(80)  TYPE c,
        msg_color(4) TYPE c.
TYPES: END OF t_link.
TYPES: t_links TYPE STANDARD TABLE OF t_link.

DATA: gt_links       TYPE t_links,
      gv_exit        TYPE abap_bool.


************************************************************
* Selection Screen
************************************************************
TABLES: toav0.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
SELECT-OPTIONS s_sapobj FOR toav0-sap_object.
SELECT-OPTIONS s_objid  FOR toav0-object_id.
SELECT-OPTIONS s_arobj  FOR toav0-ar_object.
SELECT-OPTIONS s_ardat  FOR toav0-ar_date.
SELECT-OPTIONS s_arcid  FOR toav0-archiv_id.
SELECT-OPTIONS s_docid  FOR toav0-arc_doc_id.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS: p_test   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rad.
PARAMETERS: p_del    RADIOBUTTON GROUP grp1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS: p_delrep AS CHECKBOX DEFAULT ' ' MODIF ID m1.
SELECTION-SCREEN COMMENT (30) FOR FIELD p_delrep.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.


************************************************************
* Selection Screen - Changes
************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF p_del = abap_false.
      IF screen-group1 = 'M1'.
        screen-input = 0.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.


************************************************************
* Main
************************************************************
START-OF-SELECTION.

  PERFORM checks CHANGING gv_exit.
  IF gv_exit = abap_true.
    RETURN.
  ENDIF.
  PERFORM select_entries CHANGING gt_links gv_exit.
  IF gv_exit = abap_true.
    RETURN.
  ENDIF.
  PERFORM delete_entries CHANGING gt_links.
  PERFORM output_result USING gt_links.


************************************************************
* Form Routines
************************************************************

*&---------------------------------------------------------------------*
*&      Form  CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM checks CHANGING ev_exit TYPE abap_bool.

  DATA: lv_dummy(1) TYPE c.

* Check - don't execute in production SAP system
  SELECT SINGLE cccategory INTO lv_dummy
         FROM t000
         WHERE mandt = sy-mandt
           AND cccategory = 'P'.
  IF sy-subrc = 0.
    MESSAGE 'Execution in production SAP system is not allowed.' TYPE 'I'.
    ev_exit = abap_true.
    RETURN.
  ENDIF.

* Check if any selection criteria was provided
  IF ( s_sapobj[] IS INITIAL AND s_objid[] IS INITIAL AND
       s_arcid[]  IS INITIAL AND s_docid[] IS INITIAL AND
       s_arobj[]  IS INITIAL AND s_ardat[] IS INITIAL ).
    MESSAGE 'No selection criteria was provided. At least one is mandatory.' TYPE 'I'.
    ev_exit = abap_true.
    RETURN.
  ENDIF.

* Authorization check - user must be ArchiveLink Admin of any repository
  AUTHORITY-CHECK OBJECT 'S_WFAR_OBJ'
                  ID 'ACTVT' FIELD '70'.
  IF sy-subrc <> 0.
    MESSAGE 'No ArchiveLink Admin. Missing authorization: Activity 70 for auth. obj. S_WFAR_OBJ.' TYPE 'I'.
    ev_exit = abap_true.
    RETURN.
  ENDIF.

ENDFORM.                    " CHECKS


*&---------------------------------------------------------------------*
*&      Form  SELECT_ENTRIES
*&---------------------------------------------------------------------*
*       Select ArchiveLink entries based on selection screen criteria
*       and get confirmation for deletion
*----------------------------------------------------------------------*
FORM select_entries CHANGING et_links TYPE t_links
                             ev_exit TYPE abap_bool.

  DATA: lt_toaco         TYPE STANDARD TABLE OF toaco,
        lv_count         TYPE i,
        lv_count_chr(12) TYPE c,
        lv_question(100) TYPE c,
        lv_answer(1)     TYPE c.

  FIELD-SYMBOLS: <toaco>    TYPE toaco.


* Get all ArchiveLink tables (e.g. TOA01, TOA02),
* so we can perform select for each of them
  SELECT * INTO TABLE lt_toaco
         FROM toaco.


  CLEAR: et_links[].

  LOOP AT lt_toaco ASSIGNING <toaco>.

    SELECT * APPENDING TABLE et_links
           FROM (<toaco>-connection)
           WHERE sap_object IN s_sapobj
             AND object_id  IN s_objid
             AND archiv_id  IN s_arcid
             AND arc_doc_id IN s_docid
             AND ar_object  IN s_arobj
             AND ar_date    IN s_ardat.

  ENDLOOP.

  SORT et_links BY sap_object object_id archiv_id arc_doc_id.

  DESCRIBE TABLE et_links LINES lv_count.
  WRITE lv_count TO lv_count_chr LEFT-JUSTIFIED.


* Get confirmation from user to delete documents
  IF p_del = abap_true AND lv_count > 0.

    CONCATENATE 'Do you really want to delete'
                lv_count_chr
                'ArchiveLink entries?'
                INTO lv_question SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmation to delete entries'
        text_question         = lv_question
*       TEXT_BUTTON_1         = 'Yes'(001)
*       ICON_BUTTON_1         = ' '
*       TEXT_BUTTON_2         = 'No'(002)
*       ICON_BUTTON_2         = ' '
*       DEFAULT_BUTTON        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc = 0.
      IF lv_answer <> '1'.
        ev_exit = abap_true.
      ENDIF.
    ELSE.
      MESSAGE 'Technical error at confirmation step.' TYPE 'I'.
      ev_exit = abap_true.
    ENDIF.

  ENDIF.

ENDFORM.                    " SELECT_ENTRIES


*&---------------------------------------------------------------------*
*&      Form  DELETE_ENTRIES
*&---------------------------------------------------------------------*
*       Go through all the ArchiveLink entries and delete them
*----------------------------------------------------------------------*
FORM delete_entries CHANGING et_links TYPE t_links.

  DATA: lv_delete_flag      TYPE i.
  DATA: lv_subrc_chr(4)     TYPE c.
  FIELD-SYMBOLS: <link>     TYPE t_link.


  IF p_test = abap_true.


* Check for all entries if permission to delete
    LOOP AT et_links ASSIGNING <link>.

      AUTHORITY-CHECK OBJECT 'S_WFAR_OBJ'
               ID 'OAARCHIV' FIELD <link>-archiv_id
               ID 'OAOBJEKTE' FIELD <link>-sap_object
               ID 'OADOKUMENT' FIELD <link>-ar_object
               ID 'ACTVT' FIELD '06'.   "06 - Delete
      IF sy-subrc = 0.
        <link>-msg_type = 'I'.
        <link>-message = 'TEST SELECTION. Authorization check for delete is ok.'.
      ELSE.
        <link>-msg_type = 'W'.
        <link>-message = 'Missing authorization to delete document.'.
        <link>-msg_color = zalv_yellow_int.
      ENDIF.

    ENDLOOP.


  ELSE.


    IF p_delrep = abap_true.
      lv_delete_flag = 2.   "2 => No dialog popup; Delete link and doc in repository
    ELSE.
      lv_delete_flag = 3.   "3 => No dialog popup; Only delete link
    ENDIF.

    LOOP AT et_links ASSIGNING <link>.

      CALL FUNCTION 'ARCHIV_DELETE_META'
        EXPORTING
          archiv_id                = <link>-archiv_id
          arc_doc_id               = <link>-arc_doc_id
          ar_object                = <link>-ar_object
          object_id                = <link>-object_id
          sap_object               = <link>-sap_object
          delete_flag              = lv_delete_flag
        EXCEPTIONS
          error_connectiontable    = 1
          error_parameter          = 2
          error_archiv             = 3
          error_kernel             = 4
          error_communicationtable = 5
          error_authority          = 6
          OTHERS                   = 7.
      WRITE sy-subrc TO lv_subrc_chr LEFT-JUSTIFIED.
      IF sy-subrc = 0.
        COMMIT WORK.
        IF p_delrep = abap_true.
          <link>-msg_type = 'I'.
          <link>-message = 'Deletion of link and document was successful.'.
        ELSE.
          <link>-msg_type = 'I'.
          <link>-message = 'Deletion of link was successful.'.
        ENDIF.
      ELSEIF sy-subrc = 6.
        ROLLBACK WORK.
        <link>-msg_type = 'W'.
        <link>-message = 'Missing authorization to delete document.'.
        <link>-msg_color = zalv_yellow_int.
      ELSE.
        ROLLBACK WORK.
        <link>-msg_type = 'E'.
        CONCATENATE 'Error at deleting document. SY-SUBRC =' lv_subrc_chr
                    INTO <link>-message SEPARATED BY space.
        <link>-msg_color = zalv_red_int.
      ENDIF.

    ENDLOOP.


  ENDIF.

ENDFORM.                    " DELETE_ENTRIES


*&---------------------------------------------------------------------*
*&      Form  OUTPUT_RESULT
*&---------------------------------------------------------------------*
*       Display ALV list of result
*----------------------------------------------------------------------*
FORM output_result USING it_links TYPE t_links.

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_layout   TYPE slis_layout_alv.


* Define fields for ALV
  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'SAP_OBJECT'.
  ls_fieldcat-seltext_m = 'SAP Object'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'OBJECT_ID'.
  ls_fieldcat-seltext_m = 'Object ID'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'ARCHIV_ID'.
  ls_fieldcat-seltext_m = 'Archive ID'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'ARC_DOC_ID'.
  ls_fieldcat-seltext_m = 'Document ID'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'AR_OBJECT'.
  ls_fieldcat-seltext_m = 'Document Type'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'AR_DATE'.
  ls_fieldcat-seltext_m = 'Storage Date'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'DEL_DATE'.
  ls_fieldcat-seltext_m = 'Delete Date'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'RESERVE'.
  ls_fieldcat-seltext_m = 'Doc Format'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'MSG_TYPE'.
  ls_fieldcat-seltext_m = 'Msg Type'.
  ls_fieldcat-emphasize = abap_true.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'MESSAGE'.
  ls_fieldcat-seltext_m = 'Message'.
  ls_fieldcat-emphasize = abap_true.
  APPEND ls_fieldcat TO lt_fieldcat.


* Set ALV visual settings
  ls_layout-no_input          = abap_true.
  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.
  ls_layout-info_fieldname    = 'MSG_COLOR'.


* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP-OF-PAGE'
      is_layout              = ls_layout
      it_fieldcat            = lt_fieldcat[]
      i_save                 = 'X'
    TABLES
      t_outtab               = it_links.

ENDFORM.                    " OUTPUT_RESULT


*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       ALV list header
*----------------------------------------------------------------------*
FORM top-of-page.

  DATA: lt_header          TYPE slis_t_listheader,
        ls_header          TYPE slis_listheader,
        lv_total           TYPE i,
        lv_total_chr(12)   TYPE c,
        lv_success         TYPE i,
        lv_success_chr(12) TYPE c,
        lv_warn            TYPE i,
        lv_warn_chr(12)    TYPE c,
        lv_error           TYPE i,
        lv_error_chr(12)   TYPE c.

* Determine result numbers
  DESCRIBE TABLE gt_links LINES lv_total.
  LOOP AT gt_links TRANSPORTING NO FIELDS WHERE msg_type = 'W'.
    lv_warn = lv_warn + 1.
  ENDLOOP.
  LOOP AT gt_links TRANSPORTING NO FIELDS WHERE msg_type = 'E'.
    lv_error = lv_error + 1.
  ENDLOOP.
  lv_success = lv_total - lv_warn - lv_error.
  WRITE lv_total   TO lv_total_chr LEFT-JUSTIFIED.
  WRITE lv_success TO lv_success_chr LEFT-JUSTIFIED.
  WRITE lv_warn    TO lv_warn_chr LEFT-JUSTIFIED.
  WRITE lv_error   TO lv_error_chr LEFT-JUSTIFIED.

* Total entries
  CLEAR: ls_header.
  ls_header-typ  = 'S'.
  ls_header-key = 'Run Mode'.
  IF p_test = abap_true.
    ls_header-info = ': TEST SELECTION'.
  ELSE.
    ls_header-info = ': Delete Entries'.
    IF p_delrep = abap_true.
      CONCATENATE ls_header-info '- plus delete docs in repository' INTO ls_header-info SEPARATED BY space.
    ENDIF.
  ENDIF.
  APPEND ls_header TO lt_header.

* Total entries
  CLEAR: ls_header.
  ls_header-typ  = 'S'.
  ls_header-key = 'Total'.
  CONCATENATE ':' lv_total_chr INTO ls_header-info SEPARATED BY space.
  APPEND ls_header TO lt_header.

* Result entries
  CLEAR: ls_header.
  ls_header-typ  = 'S'.
  ls_header-key = 'Result'.
  CONCATENATE ':' lv_success_chr 'successful |' lv_warn_chr 'warnings |' lv_error_chr 'errors'
              INTO ls_header-info SEPARATED BY space.
  APPEND ls_header TO lt_header.

* Add header to ALV list
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.

ENDFORM.                    " TOP-OF-PAGE
