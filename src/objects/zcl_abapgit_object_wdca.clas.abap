class ZCL_ABAPGIT_OBJECT_WDCA definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  final
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .
protected section.
private section.

  types:
    BEGIN OF ty_dd04_texts,
             ddlanguage TYPE dd04t-ddlanguage,
             ddtext     TYPE dd04t-ddtext,
             reptext    TYPE dd04t-reptext,
             scrtext_s  TYPE dd04t-scrtext_s,
             scrtext_m  TYPE dd04t-scrtext_m,
             scrtext_l  TYPE dd04t-scrtext_l,
           END OF ty_dd04_texts .
  types:
    tt_dd04_texts TYPE STANDARD TABLE OF ty_dd04_texts .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_WDCA IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA:
      ls_config_appl           TYPE wdy_config_appl.

    ls_config_appl-config_id = ms_item-obj_name+00(32).
    ls_config_appl-config_type = ms_item-obj_name+32(2).
    ls_config_appl-config_var  = ms_item-obj_name+34(6).

    SELECT SINGLE changedby FROM wdy_config_appl INTO rv_user
      WHERE
        config_id   = ls_config_appl-config_id AND
        config_type = ls_config_appl-config_type AND
        config_var  = ls_config_appl-config_var.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~DELETE.

* We do not support deletion for now


  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_config_appl  TYPE wdy_config_appl,
      lv_dummy        TYPE c LENGTH 1,
      lt_config_appt  TYPE STANDARD TABLE OF wdy_config_appt WITH NON-UNIQUE DEFAULT KEY,
      lt_config_compt TYPE STANDARD TABLE OF wdy_config_compt WITH NON-UNIQUE DEFAULT KEY,
      lv_config_str   TYPE string,
      ls_tadir        TYPE tadir.

    FIELD-SYMBOLS:
      <ls_config_appt>  LIKE LINE OF lt_config_appt[],
      <ls_config_compt> LIKE LINE OF lt_config_compt[].

************************************************************************
* Add the object to the transport
************************************************************************
    corr_insert( iv_package = iv_package ).

************************************************************************
* Get the data from the input XML tree
************************************************************************
    io_xml->read( EXPORTING iv_name = 'WDY_CONFIG_APPL'
                 CHANGING cg_data = ls_config_appl ).
    io_xml->read( EXPORTING iv_name = 'WDY_CONFIG_APPT'
                 CHANGING cg_data = lt_config_appt[] ).
    io_xml->read( EXPORTING iv_name = 'WDY_CONFIG_COMPT'
                 CHANGING cg_data = lt_config_compt[] ).
    io_xml->read( EXPORTING iv_name = 'CONFIG_DATA'
                 CHANGING cg_data = lv_config_str ).
    ls_config_appl-xcontent = zcl_abapgit_object_wdcx_util=>string_2_xstring( lv_config_str ).

************************************************************************
* Fill the identifiers back, which has been cleaned by the serializer
************************************************************************
    LOOP AT lt_config_appt[] ASSIGNING <ls_config_appt>.
      <ls_config_appt>-config_id   = ls_config_appl-config_id.
      <ls_config_appt>-config_type = ls_config_appl-config_type.
      <ls_config_appt>-config_var  = ls_config_appl-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_appt>.

    LOOP AT lt_config_compt[] ASSIGNING <ls_config_compt>.
      <ls_config_compt>-config_id   = ls_config_appl-config_id.
      <ls_config_compt>-config_type = ls_config_appl-config_type.
      <ls_config_compt>-config_var  = ls_config_appl-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_compt>.

************************************************************************
* Save the data in the WDY_CONFIG_APPL
************************************************************************
    SELECT SINGLE config_id FROM wdy_config_appl INTO lv_dummy
      WHERE
        config_id   = ls_config_appl-config_id AND
        config_type = ls_config_appl-config_type AND
        config_var  = ls_config_appl-config_var.
    IF sy-subrc <> 0.
      INSERT wdy_config_appl FROM ls_config_appl.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |insert into WDY_CONFIG_APPL failed, { sy-subrc }| ).
      ENDIF.
    ELSE.
      "Record already exists - we do not handle updating of the record
    ENDIF.

************************************************************************
* Save the data in the WDY_CONFIG_APPT
************************************************************************
    LOOP AT lt_config_appt[] ASSIGNING <ls_config_appt>.
      SELECT SINGLE config_id FROM wdy_config_appt INTO lv_dummy
        WHERE
          config_id   = <ls_config_appt>-config_id AND
          config_type = <ls_config_appt>-config_type AND
          config_var  = <ls_config_appt>-config_var AND
          langu       = <ls_config_appt>-langu.
      IF sy-subrc <> 0.
        INSERT wdy_config_appt FROM <ls_config_appt>.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |insert into WDY_CONFIG_APPT failed, { sy-subrc }| ).
        ENDIF.
      ELSE.
        "Record already exists - we do not handle updating of the record
      ENDIF.
    ENDLOOP.
    UNASSIGN <ls_config_appt>.

************************************************************************
* Save the data in the WDY_CONFIG_COMPT
************************************************************************
    LOOP AT lt_config_compt[] ASSIGNING <ls_config_compt>.
      SELECT SINGLE config_id FROM wdy_config_compt INTO lv_dummy
        WHERE
          config_id   = <ls_config_compt>-config_id AND
          config_type = <ls_config_compt>-config_type AND
          config_var  = <ls_config_compt>-config_var AND
          langu       = <ls_config_compt>-langu AND
          text_id     = <ls_config_compt>-text_id.
      IF sy-subrc <> 0.
        INSERT wdy_config_compt FROM <ls_config_compt>.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |insert into WDY_CONFIG_COMPT failed, { sy-subrc }| ).
        ENDIF.
      ELSE.
        "Record already exists - we do not handle updating of the record
      ENDIF.
    ENDLOOP.
    UNASSIGN <ls_config_compt>.


  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA:
      ls_config_appl           TYPE wdy_config_appl.

    ls_config_appl-config_id = ms_item-obj_name+00(32).
    ls_config_appl-config_type = ms_item-obj_name+32(2).
    ls_config_appl-config_var  = ms_item-obj_name+34(6).

    SELECT SINGLE config_id FROM wdy_config_appl INTO ls_config_appl
      WHERE
        config_id   = ls_config_appl-config_id AND
        config_type = ls_config_appl-config_type AND
        config_var  = ls_config_appl-config_var.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~GET_COMPARATOR.
    RETURN.
  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~IS_ACTIVE.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~IS_LOCKED.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA:
      ls_config_appl     TYPE wdy_config_appl,
      lt_config_appt     TYPE STANDARD TABLE OF wdy_config_appt WITH NON-UNIQUE DEFAULT KEY,
      lt_config_compt    TYPE STANDARD TABLE OF wdy_config_compt WITH NON-UNIQUE DEFAULT KEY,
      lv_config_xstr     TYPE xstring.

    FIELD-SYMBOLS:
      <ls_config_appt>  LIKE LINE OF lt_config_appt[],
      <ls_config_compt> LIKE LINE OF lt_config_compt[].

************************************************************************
* Read WDY_CONFIG_APPL - main data
************************************************************************
    ls_config_appl-config_id = ms_item-obj_name+00(32).
    ls_config_appl-config_type = ms_item-obj_name+32(2).
    ls_config_appl-config_var  = ms_item-obj_name+34(6).

    SELECT SINGLE * FROM wdy_config_appl INTO ls_config_appl
      WHERE
        config_id   = ls_config_appl-config_id AND
        config_type = ls_config_appl-config_type AND
        config_var  = ls_config_appl-config_var.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Not found in WDY_CONFIG_APPL' ) ##NO_TEXT.
    ENDIF.
    lv_config_xstr = ls_config_appl-xcontent.
    CLEAR ls_config_appl-xcontent.

************************************************************************
* Read WDY_CONFIG_APPT - descriptions
************************************************************************
    SELECT * FROM wdy_config_appt INTO TABLE lt_config_appt
      WHERE
        config_id   = ls_config_appl-config_id AND
        config_type = ls_config_appl-config_type AND
        config_var  = ls_config_appl-config_var.
    IF sy-subrc <> 0.
      "This is not a problem - the content of this table is optional
    ENDIF.

************************************************************************
* Process WDY_CONFIG_COMPT - Description Texts
************************************************************************
    SELECT * FROM wdy_config_compt INTO TABLE lt_config_compt
      WHERE
        config_id   = ls_config_appl-config_id AND
        config_type = ls_config_appl-config_type AND
        config_var  = ls_config_appl-config_var.
    IF sy-subrc <> 0.
      "This is not a problem - the content of this table is optional
    ENDIF.

************************************************************************
* Remove the identifiers which are not needed (do not repeat them,
* make the file smaller and cleaner
************************************************************************
    LOOP AT lt_config_appt[] ASSIGNING <ls_config_appt>.
      CLEAR <ls_config_appt>-config_id.
      CLEAR <ls_config_appt>-config_type.
      CLEAR <ls_config_appt>-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_appt>.

    LOOP AT lt_config_compt[] ASSIGNING <ls_config_compt>.
      CLEAR <ls_config_compt>-config_id.
      CLEAR <ls_config_compt>-config_type.
      CLEAR <ls_config_compt>-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_compt>.

************************************************************************
* Pass the data to the output XML tree
************************************************************************
    io_xml->add( iv_name = 'WDY_CONFIG_APPL'
                 ig_data = ls_config_appl ).
    io_xml->add( iv_name = 'WDY_CONFIG_APPT'
                 ig_data = lt_config_appt[] ).
    io_xml->add( iv_name = 'WDY_CONFIG_COMPT'
                 ig_data = lt_config_compt[] ).
    io_xml->add( iv_name = 'CONFIG_DATA'
                 ig_data = zcl_abapgit_object_wdcx_util=>xstring_2_string( lv_config_xstr ) ).
  ENDMETHOD.
ENDCLASS.
