class ZCL_ABAPGIT_OBJECT_OSOD definition
  public
  inheriting from ZCL_ABAPGIT_OBJECTS_SUPER
  create public .

public section.

  interfaces ZIF_ABAPGIT_OBJECT .

  methods CONSTRUCTOR
    importing
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
      !IV_LANGUAGE type SPRAS .
protected section.

  constants MC_OBJVERS_D type ROOSOURCE-OBJVERS value 'D' ##NO_TEXT.
  constants MC_OBJVERS_A type ROOSOURCE-OBJVERS value 'A' ##NO_TEXT.
  data MV_OBJVERS type ROOSOURCE-OBJVERS .
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



CLASS ZCL_ABAPGIT_OBJECT_OSOD IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor(
        is_item     = is_item
        iv_language = iv_language ).
    me->mv_objvers = me->mc_objvers_d.
  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE tstpnm FROM roosource INTO rv_user
      WHERE
        oltpsource = me->ms_item-obj_name AND
        objvers    = me->mv_objvers.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_ABAPGIT_OBJECT~DELETE.

* We do not support deletion for now


  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      ls_oltpsource                 TYPE rsaot_s_osource,
      lt_oltpsource_field           TYPE rsaot_t_osfield,
      lt_oltpsource_text            TYPE rsaot_t_ostext,
      lt_messages                   TYPE rsaot_t_messages,
      lv_subrc_rsa1                 TYPE sy-subrc,
      lv_subrc                      TYPE sy-subrc,
      ls_new_oltpsource             TYPE rsaot_s_osgen,
      lt_new_oltpsource_field       TYPE rsaot_t_osgenfd,
      ls_new_oltpsource_field       LIKE LINE OF lt_new_oltpsource_field[],
      lt_new_oltpsource_text        TYPE rsaot_t_osgentext,
      ls_new_oltpsource_text        LIKE LINE OF lt_new_oltpsource_text[],
      ls_rsadmin_content_system_org TYPE rsadmin,
      ls_rsadmin_content_system_tmp TYPE rsadmin,
      lv_trnum                      TYPE trkorr,
      lv_message_text               TYPE c LENGTH 1024.

    FIELD-SYMBOLS:
      <ls_oltpsource_text>  LIKE LINE OF lt_oltpsource_text[],
      <ls_oltpsource_field> LIKE LINE OF lt_oltpsource_field[],
      <ls_message>          LIKE LINE OF lt_messages[].

************************************************************************
* Determine the transport to be used
************************************************************************
    lv_trnum = zcl_abapgit_default_transport=>get_instance( )->get( )-ordernum.

************************************************************************
* Get the data from the input XML tree
************************************************************************
    io_xml->read( EXPORTING iv_name = 'HEADER'
                 CHANGING cg_data = ls_oltpsource ).
    io_xml->read( EXPORTING iv_name = 'TEXT'
                 CHANGING cg_data = lt_oltpsource_text[] ).
    io_xml->read( EXPORTING iv_name = 'FIELDS'
                 CHANGING cg_data = lt_oltpsource_field[] ).

*************************************************************************
** Fill the identifiers back, which has been cleaned by the serializer
*************************************************************************
    LOOP AT lt_oltpsource_text[] ASSIGNING <ls_oltpsource_text>.
      <ls_oltpsource_text>-oltpsource = ls_new_oltpsource-oltpsource.
      <ls_oltpsource_text>-objvers    = me->mv_objvers.
    ENDLOOP.
    UNASSIGN <ls_oltpsource_text>.

    LOOP AT lt_oltpsource_field[] ASSIGNING <ls_oltpsource_field>.
      <ls_oltpsource_field>-oltpsource = ls_new_oltpsource-oltpsource.
      <ls_oltpsource_field>-objvers    = me->mv_objvers.
    ENDLOOP.
    UNASSIGN <ls_oltpsource_field>.


************************************************************************
* Prepare the values for the API call
************************************************************************
    MOVE-CORRESPONDING ls_oltpsource TO ls_new_oltpsource.
    ls_oltpsource-objvers = me->mv_objvers.
    ls_new_oltpsource-structure = ls_oltpsource-exstruct.

    LOOP AT lt_oltpsource_field[] ASSIGNING <ls_oltpsource_field>.
      CLEAR ls_new_oltpsource_field.
      MOVE-CORRESPONDING <ls_oltpsource_field> TO ls_new_oltpsource_field.
      APPEND ls_new_oltpsource_field TO lt_new_oltpsource_field[].
    ENDLOOP.
    UNASSIGN <ls_oltpsource_field>.

    LOOP AT lt_oltpsource_text[] ASSIGNING <ls_oltpsource_text>.
      CLEAR ls_new_oltpsource_text.
      MOVE-CORRESPONDING <ls_oltpsource_text> TO ls_new_oltpsource_text.
      ls_new_oltpsource_text-oltpsource = ls_new_oltpsource-oltpsource.
      APPEND ls_new_oltpsource_text TO lt_new_oltpsource_text[].
    ENDLOOP.
    UNASSIGN <ls_oltpsource_text>.

************************************************************************
* Validate:
* - the object name
* - the package
* Either create it in some namespace or as Z or Y object
************************************************************************
    IF ls_new_oltpsource-oltpsource+0(1) = 'Z' OR
      ls_new_oltpsource-oltpsource+0(1) = 'Y' OR
      ls_new_oltpsource-oltpsource+0(1) = '/'.
    ELSE.
      zcx_abapgit_exception=>raise( |Invalid OLT Source Name { ls_new_oltpsource-oltpsource } | ) ##NO_TEXT.
    ENDIF.

    IF iv_package+0(1) = 'Z' OR
      iv_package+0(1) = 'Y' OR
      iv_package+0(1) = '/'.
    ELSE.
      zcx_abapgit_exception=>raise( |Invalid Package { iv_package } | ) ##NO_TEXT.
    ENDIF.

************************************************************************
* Read the current setting of RSADMIN-CONTENT_SYSTEM
* Set the value of RSADMIN-CONTENT_SYSTEM:
* - value ' ', if we are to create the active version, in that case OSOA transport is to be created
* - value 'X', if we are to create the delivered version, in that case OSOD transport is to be created
************************************************************************
    SELECT SINGLE
        *
      FROM rsadmin
      INTO ls_rsadmin_content_system_org
      WHERE
        object = 'CONTENT_SYSTEM'.
    IF sy-subrc <> 0.
      ASSERT 1 = 2.
    ENDIF.
    ls_rsadmin_content_system_tmp = ls_rsadmin_content_system_org.
    IF me->mv_objvers = me->mc_objvers_d.
      ls_rsadmin_content_system_tmp-value = abap_true.
    ELSE.
      ls_rsadmin_content_system_tmp-value = abap_false.
    ENDIF.
    UPDATE rsadmin FROM ls_rsadmin_content_system_tmp.

************************************************************************
* Create the OLTP Source
************************************************************************
    CALL FUNCTION 'RSA1_OLTPSOURCE_GENERATE'
      EXPORTING
        i_save_despite_warning = abap_true
        i_s_oltpsource         = ls_new_oltpsource
        i_t_oltpsourcefields   = lt_new_oltpsource_field[]
        i_t_ostext             = lt_new_oltpsource_text[]
        i_with_dynpro          = rsaot_c_flag-off
        i_request              = lv_trnum
        i_devclass             = iv_package
        i_objvers              = me->mv_objvers
      IMPORTING
        e_t_messages           = lt_messages[]
        e_subrc                = lv_subrc_rsa1
      EXCEPTIONS
        internal_error         = 1
        transport_error        = 2
        locked                 = 3
        inconsistent           = 4
        cancelled              = 5
        OTHERS                 = 6.
    lv_subrc = sy-subrc.
    LOOP AT lt_messages[] ASSIGNING <ls_message>.
      MESSAGE ID <ls_message>-msgid TYPE <ls_message>-msgty NUMBER <ls_message>-msgno WITH
      <ls_message>-msgv1 <ls_message>-msgv2 <ls_message>-msgv3 <ls_message>-msgv4
      INTO lv_message_text.
      ii_log->add( iv_msg = lv_message_text iv_type = <ls_message>-msgty is_item = me->ms_item ).
    ENDLOOP.
    UNASSIGN <ls_message>.
************************************************************************
* Restore the original value of RSADMIN-CONTENT_SYSTEM
************************************************************************
    UPDATE rsadmin FROM ls_rsadmin_content_system_org.

************************************************************************
* Do the error handling
************************************************************************
    IF lv_subrc <> 0 OR lv_subrc_rsa1 <> 0.
      zcx_abapgit_exception=>raise( 'Failed to create the object' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA:
      ls_roosource           TYPE roosource.

    SELECT SINGLE oltpsource FROM roosource INTO ls_roosource-oltpsource
      WHERE
        oltpsource = me->ms_item-obj_name AND
        objvers    = me->mv_objvers.

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
    SET PARAMETER ID 'RS1' FIELD space.
    SET PARAMETER ID 'RS2' FIELD space.
    SET PARAMETER ID 'RS3' FIELD space.
    SET PARAMETER ID 'RS1' FIELD ms_item-obj_name.
    CALL TRANSACTION 'RSO2'.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA:
      ls_oltpsource       TYPE rsaot_s_osource,
      lt_oltpsource_field TYPE rsaot_t_osfield,
      lt_oltpsource_text  TYPE rsaot_t_ostext.

    FIELD-SYMBOLS:
      <ls_oltpsource_text>  LIKE LINE OF lt_oltpsource_text[],
      <ls_oltpsource_field> LIKE LINE OF lt_oltpsource_field[].

************************************************************************
* Read the data
************************************************************************
    ls_oltpsource-oltpsource = me->ms_item-obj_name.
    CALL FUNCTION 'RSA1_SINGLE_OLTPSOURCE_GET'
      EXPORTING
        i_oltpsource   = ls_oltpsource-oltpsource
        i_objvers      = me->mv_objvers
      IMPORTING
        e_s_oltpsource = ls_oltpsource
        e_t_fields     = lt_oltpsource_field[]
        e_t_texts      = lt_oltpsource_text[]
      EXCEPTIONS
        no_authority   = 1
        not_exist      = 2
        inconsistent   = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Not found in ROOSOURCE' ) ##NO_TEXT.
      ASSERT 1 = 2.
    ENDIF.

************************************************************************
* Remove the identifiers which are not needed (do not repeat them,
* make the file smaller and cleaner
************************************************************************
    CLEAR ls_oltpsource-tstpnm.
    CLEAR ls_oltpsource-tstpdat.
    CLEAR ls_oltpsource-tstptim.
    CLEAR ls_oltpsource-objvers.
    CLEAR ls_oltpsource-txtsh.
    CLEAR ls_oltpsource-txtmd.
    CLEAR ls_oltpsource-txtlg.

    LOOP AT lt_oltpsource_text[] ASSIGNING <ls_oltpsource_text>.
      CLEAR <ls_oltpsource_text>-oltpsource.
      CLEAR <ls_oltpsource_text>-objvers.
    ENDLOOP.
    UNASSIGN <ls_oltpsource_text>.

    LOOP AT lt_oltpsource_field[] ASSIGNING <ls_oltpsource_field>.
      CLEAR <ls_oltpsource_field>-oltpsource.
      CLEAR <ls_oltpsource_field>-objvers.
      IF <ls_oltpsource_field>-selection IS INITIAL.
        "In case of customer or partner case we cannot leave the value blank, because it will be then set by the API to 3,
        "which will result in the field to be hidden
        <ls_oltpsource_field>-selection = 'P'.
      ENDIF.
    ENDLOOP.
    UNASSIGN <ls_oltpsource_field>.

************************************************************************
* Pass the data to the output XML tree
************************************************************************
    io_xml->add( iv_name = 'HEADER'
                 ig_data = ls_oltpsource ).
    io_xml->add( iv_name = 'TEXT'
                 ig_data = lt_oltpsource_text[] ).
    io_xml->add( iv_name = 'FIELDS'
                 ig_data = lt_oltpsource_field[] ).
  ENDMETHOD.
ENDCLASS.
