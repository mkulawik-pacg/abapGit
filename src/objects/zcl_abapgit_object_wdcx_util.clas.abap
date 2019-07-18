class ZCL_ABAPGIT_OBJECT_WDCX_UTIL definition
  public
  create public .

public section.

  class-methods STRING_2_XSTRING
    importing
      !IV_DATA type STRING
    returning
      value(RV_RESULT) type XSTRING .
  class-methods XSTRING_2_STRING
    importing
      !IV_DATA type XSTRING
    returning
      value(RV_RESULT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_WDCX_UTIL IMPLEMENTATION.


  METHOD string_2_xstring.
************************************************************************
*
*  Project...........: DBM->DBE Conversion
*  Description.......: Covert string to xstring
*
*  Author............: Mariusz Kakol
*  Company...........: Proaxia
*  Creation Date.....: 2019.07.11
*
************************************************************************
*  Changed on:  Changed by:   Change ID:  Description:
*
*  2019.07.11   M.Kakol                   First ver.
************************************************************************
    DATA:
      l_convout   TYPE REF TO cl_abap_conv_out_ce.

    TRY.
        l_convout = cl_abap_conv_out_ce=>create(
                    encoding = 'UTF-8' ).

        l_convout->convert(
          EXPORTING
            data   = iv_data
          IMPORTING
            buffer = rv_result ).

      CATCH cx_root.
        ASSERT 1 = 2. "todo
    ENDTRY.
  ENDMETHOD.


  METHOD xstring_2_string.
************************************************************************
*
*  Project...........: DBM->DBE Conversion
*  Description.......: Covert xstring to string
*
*  Author............: Mariusz Kakol
*  Company...........: Proaxia
*  Creation Date.....: 2019.07.11
*
************************************************************************
*  Changed on:  Changed by:   Change ID:  Description:
*
*  2019.07.11   M.Kakol                   First ver.
************************************************************************
    DATA:
      l_convin     TYPE REF TO cl_abap_conv_in_ce.

    TRY.
        CALL METHOD cl_abap_conv_in_ce=>create
          EXPORTING
            encoding = 'UTF-8'
            input    = iv_data
          RECEIVING
            conv     = l_convin.

        CALL METHOD l_convin->read
          IMPORTING
            data = rv_result.

      CATCH cx_root.
        ASSERT 1 = 2. "todo
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
