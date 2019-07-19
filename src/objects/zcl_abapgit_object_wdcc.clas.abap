class ZCL_ABAPGIT_OBJECT_WDCC definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  final
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .
  PROTECTED SECTION.
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



CLASS ZCL_ABAPGIT_OBJECT_WDCC IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA:
      ls_config_data           TYPE wdy_config_data.

    ls_config_data-config_id = ms_item-obj_name+00(32).
    ls_config_data-config_type = ms_item-obj_name+32(2).
    ls_config_data-config_var  = ms_item-obj_name+34(6).

    SELECT SINGLE changedby FROM wdy_config_data INTO rv_user
      WHERE
        config_id   = ls_config_data-config_id AND
        config_type = ls_config_data-config_type AND
        config_var  = ls_config_data-config_var.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

* We do not support deletion for now

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_config_data  TYPE wdy_config_data,
      lv_dummy        TYPE c LENGTH 1,
      lt_config_datt  TYPE STANDARD TABLE OF wdy_config_datt WITH NON-UNIQUE DEFAULT KEY,
      lt_config_compt TYPE STANDARD TABLE OF wdy_config_compt WITH NON-UNIQUE DEFAULT KEY,
      lv_config_str   TYPE string,
      ls_tadir        TYPE tadir.

    FIELD-SYMBOLS:
      <ls_config_datt>  LIKE LINE OF lt_config_datt[],
      <ls_config_compt> LIKE LINE OF lt_config_compt[].

************************************************************************
* Add the object to the transport
************************************************************************
    corr_insert( iv_package = iv_package ).

************************************************************************
* Get the data from the input XML tree
************************************************************************
    io_xml->read( EXPORTING iv_name = 'WDY_CONFIG_DATA'
                 CHANGING cg_data = ls_config_data ).
    io_xml->read( EXPORTING iv_name = 'WDY_CONFIG_DATT'
                 CHANGING cg_data = lt_config_datt[] ).
    io_xml->read( EXPORTING iv_name = 'WDY_CONFIG_COMPT'
                 CHANGING cg_data = lt_config_compt[] ).
    ls_config_data-xcontent = zcl_abapgit_object_wdcx_util=>xmlnode_2_xstring(
      io_xml       = io_xml->get_raw( )
      iv_node_name = 'CONFIG_DATA' ).
*    io_xml->read( EXPORTING iv_name = 'CONFIG_DATA'
*                 CHANGING cg_data = lv_config_str ).
*    ls_config_data-xcontent = zcl_abapgit_object_wdcx_util=>string_2_xstring( lv_config_str ).


************************************************************************
* Fill the identifiers back, which has been cleaned by the serializer
************************************************************************
    LOOP AT lt_config_datt[] ASSIGNING <ls_config_datt>.
      <ls_config_datt>-config_id   = ls_config_data-config_id.
      <ls_config_datt>-config_type = ls_config_data-config_type.
      <ls_config_datt>-config_var  = ls_config_data-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_datt>.

    LOOP AT lt_config_compt[] ASSIGNING <ls_config_compt>.
      <ls_config_compt>-config_id   = ls_config_data-config_id.
      <ls_config_compt>-config_type = ls_config_data-config_type.
      <ls_config_compt>-config_var  = ls_config_data-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_compt>.

************************************************************************
* Save the data in the WDY_CONFIG_data
************************************************************************
    SELECT SINGLE config_id FROM wdy_config_data INTO lv_dummy
      WHERE
        config_id   = ls_config_data-config_id AND
        config_type = ls_config_data-config_type AND
        config_var  = ls_config_data-config_var.
    IF sy-subrc <> 0.
      INSERT wdy_config_data FROM ls_config_data.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |insert into WDY_CONFIG_DATA failed, { sy-subrc }| ).
      ENDIF.
    ELSE.
      "Record already exists - we do not handle updating of the record
    ENDIF.

************************************************************************
* Save the data in the WDY_CONFIG_datt
************************************************************************
    LOOP AT lt_config_datt[] ASSIGNING <ls_config_datt>.
      SELECT SINGLE config_id FROM wdy_config_datt INTO lv_dummy
        WHERE
          config_id   = <ls_config_datt>-config_id AND
          config_type = <ls_config_datt>-config_type AND
          config_var  = <ls_config_datt>-config_var AND
          langu       = <ls_config_datt>-langu.
      IF sy-subrc <> 0.
        INSERT wdy_config_datt FROM <ls_config_datt>.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |insert into WDY_CONFIG_DATT failed, { sy-subrc }| ).
        ENDIF.
      ELSE.
        "Record already exists - we do not handle updating of the record
      ENDIF.
    ENDLOOP.
    UNASSIGN <ls_config_datt>.

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
      ls_config_data           TYPE wdy_config_data.

    ls_config_data-config_id = ms_item-obj_name+00(32).
    ls_config_data-config_type = ms_item-obj_name+32(2).
    ls_config_data-config_var  = ms_item-obj_name+34(6).

    SELECT SINGLE config_id FROM wdy_config_data INTO ls_config_data
      WHERE
        config_id   = ls_config_data-config_id AND
        config_type = ls_config_data-config_type AND
        config_var  = ls_config_data-config_var.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~GET_COMPARATOR.
    RETURN.
  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~GET_METADATA.
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
      ls_config_data  TYPE wdy_config_data,
      lt_config_datt  TYPE STANDARD TABLE OF wdy_config_datt WITH NON-UNIQUE DEFAULT KEY,
      lt_config_compt TYPE STANDARD TABLE OF wdy_config_compt WITH NON-UNIQUE DEFAULT KEY,
      lv_config_xstr  TYPE xstring.

    FIELD-SYMBOLS:
      <ls_config_datt>  LIKE LINE OF lt_config_datt[],
      <ls_config_compt> LIKE LINE OF lt_config_compt[].


************************************************************************
* Read WDY_CONFIG_APPL - main data
************************************************************************
    ls_config_data-config_id = ms_item-obj_name+00(32).
    ls_config_data-config_type = ms_item-obj_name+32(2).
    ls_config_data-config_var  = ms_item-obj_name+34(6).

    SELECT SINGLE * FROM wdy_config_data INTO ls_config_data
      WHERE
        config_id   = ls_config_data-config_id AND
        config_type = ls_config_data-config_type AND
        config_var  = ls_config_data-config_var.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Not found in WDY_CONFIG_DATA' ) ##NO_TEXT.
    ENDIF.
    lv_config_xstr = ls_config_data-xcontent.
    CLEAR ls_config_data-xcontent.

************************************************************************
* Read WDY_CONFIG_APPT - descriptions
************************************************************************
    SELECT * FROM wdy_config_datt INTO TABLE lt_config_datt
      WHERE
        config_id   = ls_config_data-config_id AND
        config_type = ls_config_data-config_type AND
        config_var  = ls_config_data-config_var.
    IF sy-subrc <> 0.
      "This is not a problem - the content of this table is optional
    ENDIF.

************************************************************************
* Process WDY_CONFIG_COMPT - Description Texts
************************************************************************
    SELECT * FROM wdy_config_compt INTO TABLE lt_config_compt
      WHERE
        config_id   = ls_config_data-config_id AND
        config_type = ls_config_data-config_type AND
        config_var  = ls_config_data-config_var.
    IF sy-subrc <> 0.
      "This is not a problem - the content of this table is optional
    ENDIF.

************************************************************************
* Remove the identifiers which are not needed (do not repeat them,
* make the file smaller and cleaner
************************************************************************
    LOOP AT lt_config_datt[] ASSIGNING <ls_config_datt>.
      CLEAR <ls_config_datt>-config_id.
      CLEAR <ls_config_datt>-config_type.
      CLEAR <ls_config_datt>-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_datt>.

    LOOP AT lt_config_compt[] ASSIGNING <ls_config_compt>.
      CLEAR <ls_config_compt>-config_id.
      CLEAR <ls_config_compt>-config_type.
      CLEAR <ls_config_compt>-config_var.
    ENDLOOP.
    UNASSIGN <ls_config_compt>.

************************************************************************
* Pass the data to the output XML tree
************************************************************************
    io_xml->add( iv_name = 'WDY_CONFIG_DATA'
                 ig_data = ls_config_data ).
    io_xml->add( iv_name = 'WDY_CONFIG_DATT'
                 ig_data = lt_config_datt[] ).
    io_xml->add( iv_name = 'WDY_CONFIG_COMPT'
                 ig_data = lt_config_compt[] ).
    io_xml->add_xml( iv_name = 'CONFIG_DATA'
                 ii_xml = zcl_abapgit_object_wdcx_util=>xstring_2_xml( lv_config_xstr ) ).
*    io_xml->add( iv_name = 'CONFIG_DATA'
*                 ig_data = zcl_abapgit_object_wdcx_util=>xstring_2_string( lv_config_xstr ) ).
  ENDMETHOD.
ENDCLASS.
