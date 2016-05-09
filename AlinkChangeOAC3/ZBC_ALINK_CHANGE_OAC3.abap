*-----------------------------------------------------------------------
* Program: Utility to change OAC3 entries
* Version: 1.0 (May 03, 2016)
* Author : Eric Knabke, InfoSpire LLC (www.infospire.net)
*-----------------------------------------------------------------------
* Description:
* 	This program allows ArchiveLink Administrators to perform a mass
* 	change of the configuration that is managed through TCode OAC3.
*
* Note:
*	An authorization check for the authorization object 'S_WFAR_OBJ' is
*	performed. A user must have a value of '70' ('Administer') for
*	the 'ACTVT' field in order to run this utility.
*
* For more details go to:
*     https://github.com/InfoSpire/ArchiveLink-Utilities
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
REPORT zbc_alink_change_oac3.
TYPE-POOLS: ABAP, SLIS.

************************************************************
* Data Declaration
************************************************************
CONSTANTS: zalv_red_int(4)      TYPE c VALUE 'C610'.

TYPES: BEGIN OF ts_update,
         sap_object   TYPE saeanwdid,
         ar_object    TYPE saeobjart,
         ar_status    TYPE saearstat2,
         archiv_id    TYPE saearchivi,
         aid_new      TYPE saearchivi,
         msg_type     TYPE bapi_mtype,
         message(80)  TYPE c,
         msg_color(4) TYPE c,
       END OF ts_update.
TYPES: tt_update TYPE STANDARD TABLE OF ts_update.

DATA: gt_update      TYPE tt_update,
      gv_exit        TYPE abap_bool.


************************************************************
* Selection Screen
************************************************************
TABLES: toaom.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_sapobj FOR toaom-sap_object OBLIGATORY.
SELECT-OPTIONS: s_ar_obj FOR toaom-ar_object  OBLIGATORY.
SELECT-OPTIONS: s_ar_sta FOR toaom-ar_status  DEFAULT 'X'.
PARAMETERS:     p_aidold LIKE toaom-archiv_id OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS: p_aidnew     LIKE toaom-archiv_id OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-s03.
PARAMETERS: p_test   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND rad.
PARAMETERS: p_upd    RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b3.


************************************************************
* Main
************************************************************
START-OF-SELECTION.

  PERFORM checks CHANGING gv_exit.
  IF gv_exit = abap_true.
    RETURN.
  ENDIF.
  PERFORM select_entries CHANGING gt_update gv_exit.
  IF gv_exit = abap_true.
    RETURN.
  ENDIF.
  PERFORM update_entries CHANGING gt_update.
  PERFORM output_result USING gt_update.


************************************************************
* Form Routines
************************************************************

*&---------------------------------------------------------------------*
*&      Form  CHECKS
*&---------------------------------------------------------------------*
*       Check authorization and input parameters
*----------------------------------------------------------------------*
FORM checks CHANGING ev_exit TYPE abap_bool.

  DATA: lv_dummy(2)    TYPE c.
  DATA: lv_message(200) TYPE c.

* Authorization check - user must be ArchiveLink Admin of any repository
  AUTHORITY-CHECK OBJECT 'S_WFAR_OBJ'
                  ID 'ACTVT' FIELD '70'.
  IF sy-subrc <> 0.
    MESSAGE 'No ArchiveLink Admin. Missing authorization: Activity 70 for auth. obj. S_WFAR_OBJ.' TYPE 'I'.
    ev_exit = abap_true.
    RETURN.
  ENDIF.

* Verify that new archive parameter was provided
  SELECT SINGLE archiv_id FROM toaar INTO lv_dummy
         WHERE archiv_id EQ p_aidnew.
  IF sy-subrc <> 0.
    CONCATENATE 'New cont. rep.' p_aidnew 'does not exist in transaction OAC3.'
                INTO lv_message SEPARATED BY space.

    MESSAGE 'No ArchiveLink Admin. Missing authorization: Activity 70 for auth. obj. S_WFAR_OBJ.' TYPE 'I'.
    ev_exit = abap_true.
    RETURN.
  ENDIF.

ENDFORM.                    " CHECKS


*&---------------------------------------------------------------------*
*&      Form  SELECT_ENTRIES
*&---------------------------------------------------------------------*
*       Select OAC3 entries from table TOAOM based on selection criteria
*       and get confirmation for update
*----------------------------------------------------------------------*
FORM select_entries CHANGING et_update TYPE tt_update
                             ev_exit TYPE abap_bool.

  DATA: lv_count         TYPE i,
        lv_count_chr(12) TYPE c,
        lv_question(100) TYPE c,
        lv_answer(1)     TYPE c.


* Get entries from transaction OAC3
  SELECT sap_object ar_object ar_status archiv_id
         INTO TABLE et_update
         FROM toaom
         WHERE sap_object IN s_sapobj
           AND ar_object  IN s_ar_obj
           AND ar_status  IN s_ar_sta
           AND archiv_id  EQ p_aidold.

  DESCRIBE TABLE et_update LINES lv_count.
  WRITE lv_count TO lv_count_chr LEFT-JUSTIFIED.


* Get confirmation from user to update the entries
  IF p_upd = abap_true AND lv_count > 0.

    CONCATENATE 'Do you really want to update'
                lv_count_chr
                'entries of transaction OAC3?'
                INTO lv_question SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmation to update entries'
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
*&      Form  UPDATE_ENTRIES
*&---------------------------------------------------------------------*
*       Go through the selected TOAOM entries and update them
*----------------------------------------------------------------------*
FORM update_entries CHANGING et_update TYPE tt_update.

  FIELD-SYMBOLS: <update>     TYPE ts_update.


  IF p_test = abap_true.

    LOOP AT et_update ASSIGNING <update>.

* Test selection => do nothing
      <update>-aid_new = p_aidnew.
      <update>-msg_type = 'I'.
      <update>-message = 'TEST SELECTION.'.

    ENDLOOP.

  ELSE.

    LOOP AT et_update ASSIGNING <update>.

      UPDATE toaom SET archiv_id = p_aidnew
             WHERE sap_object = <update>-sap_object
               AND ar_object  = <update>-ar_object
               AND ar_status  = <update>-ar_status.
      IF sy-subrc = 0.
        COMMIT WORK.
        <update>-aid_new = p_aidnew.
        <update>-msg_type = 'I'.
        <update>-message = 'Update was successful.'.
      ELSE.
        ROLLBACK WORK.
        <update>-msg_type = 'E'.
        <update>-message = 'Error at updating entry.'.
        <update>-msg_color = zalv_red_int.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " UPDATE_ENTRIES


*&---------------------------------------------------------------------*
*&      Form  OUTPUT_RESULT
*&---------------------------------------------------------------------*
*       Display ALV list of result
*----------------------------------------------------------------------*
FORM output_result USING it_update TYPE tt_update.

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_layout   TYPE slis_layout_alv.


* Define fields for ALV
  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'SAP_OBJECT'.
  ls_fieldcat-seltext_m = 'SAP Object'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'AR_OBJECT'.
  ls_fieldcat-seltext_m = 'Document Type'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'AR_STATUS'.
  ls_fieldcat-seltext_m = 'Status'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'ARCHIV_ID'.
  ls_fieldcat-seltext_m = 'Cont. Rep. - Old'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR: ls_fieldcat.
  ls_fieldcat-fieldname = 'AID_NEW'.
  ls_fieldcat-seltext_m = 'Cont. Rep. - New'.
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
      t_outtab               = it_update.

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
        lv_error           TYPE i,
        lv_error_chr(12)   TYPE c.

* Determine result numbers
  DESCRIBE TABLE gt_update LINES lv_total.
  LOOP AT gt_update TRANSPORTING NO FIELDS WHERE msg_type = 'E'.
    lv_error = lv_error + 1.
  ENDLOOP.
  lv_success = lv_total - lv_error.
  WRITE lv_total   TO lv_total_chr LEFT-JUSTIFIED.
  WRITE lv_success TO lv_success_chr LEFT-JUSTIFIED.
  WRITE lv_error   TO lv_error_chr LEFT-JUSTIFIED.

* Run Mode
  CLEAR: ls_header.
  ls_header-typ  = 'S'.
  ls_header-key = 'Run Mode'.
  IF p_test = abap_true.
    ls_header-info = ': TEST SELECTION'.
  ELSE.
    ls_header-info = ': Update Entries'.
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
  CONCATENATE ':' lv_success_chr 'successful |' lv_error_chr 'errors'
              INTO ls_header-info SEPARATED BY space.
  APPEND ls_header TO lt_header.

* Add header to ALV list
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.

ENDFORM.                    " TOP-OF-PAGE
