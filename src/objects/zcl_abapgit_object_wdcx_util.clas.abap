class ZCL_ABAPGIT_OBJECT_WDCX_UTIL definition
  public
  create public .

public section.

  class-methods XMLNODE_2_XSTRING
    importing
      !IO_XML type ref to IF_IXML_DOCUMENT
      !IV_NODE_NAME type CLIKE
    returning
      value(RV_RESULT) type XSTRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods XMLUTIL_GET_CHILD_NODE
    importing
      value(IO_NODE) type ref to IF_IXML_NODE
      value(IV_NODE_NAME) type CLIKE
    returning
      value(RO_RESULT) type ref to IF_IXML_NODE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods XSTRING_2_XML
    importing
      !IV_DATA type XSTRING
    returning
      value(RO_RESULT) type ref to IF_IXML_ELEMENT
    raising
      ZCX_ABAPGIT_EXCEPTION .
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


  METHOD xmlnode_2_xstring.
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
      lo_renderer                 TYPE REF TO if_ixml_renderer,
      lo_stream_factory           TYPE REF TO if_ixml_stream_factory,
      lo_ostream                  TYPE REF TO if_ixml_ostream,
      lo_xml                      TYPE REF TO if_ixml,
      lo_output_xml_doc           TYPE REF TO if_ixml_document,
      lo_input_node               TYPE REF TO if_ixml_node.

************************************************************************
* Initialize
************************************************************************
    CLEAR rv_result.

************************************************************************
* Get the requested node from the input XML document
************************************************************************
    lo_input_node = zcl_abapgit_object_wdcx_util=>xmlutil_get_child_node(
      io_node      = io_xml->get_root_element( )
      iv_node_name = 'values' ).
    lo_input_node = zcl_abapgit_object_wdcx_util=>xmlutil_get_child_node(
      io_node      = lo_input_node
      iv_node_name = iv_node_name ).
    lo_input_node = zcl_abapgit_object_wdcx_util=>xmlutil_get_child_node(
      io_node      = lo_input_node
      iv_node_name = space ).

************************************************************************
* Build a new XML document with the content retrieved above
************************************************************************
    lo_xml = cl_ixml=>create( ).
    lo_output_xml_doc  = lo_xml->create_document( ).
    lo_output_xml_doc->get_root( )->append_child( lo_input_node ).

************************************************************************
* Render the output XML to the output xstring
************************************************************************
    lo_stream_factory = lo_xml->create_stream_factory( ).
    lo_ostream = lo_stream_factory->create_ostream_xstring( rv_result ).
    lo_renderer = lo_xml->create_renderer( ostream  = lo_ostream document = lo_output_xml_doc ).
    lo_renderer->set_normalizing( abap_true ).
    lo_renderer->render( ).

  ENDMETHOD.


  METHOD xmlutil_get_child_node.
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
      lo_children      TYPE REF TO if_ixml_node_list,
      lo_children_iter TYPE REF TO if_ixml_node_iterator,
      lo_child_node    TYPE REF TO if_ixml_node.

************************************************************************
* Initialize
************************************************************************
    CLEAR ro_result.

************************************************************************
* Get the requested node from the input XML document
************************************************************************
    lo_children = io_node->get_children( ).
    lo_children_iter = lo_children->create_iterator( ).
    DO.
      lo_child_node = lo_children_iter->get_next( ).
      IF lo_child_node IS INITIAL.
        EXIT.
      ENDIF.
      DATA l TYPE text1024.
      l = lo_child_node->get_name( ).
      IF iv_node_name IS INITIAL OR lo_child_node->get_name( ) = iv_node_name.
        ro_result = lo_child_node.
        RETURN.
      ENDIF.
    ENDDO.

    zcx_abapgit_exception=>raise( |Node { iv_node_name } not found| ). "#EC NOTEXT


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


  METHOD XSTRING_2_XML.
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
      lo_xml            TYPE REF TO if_ixml,
      lo_xml_doc        TYPE REF TO if_ixml_document,
      lo_stream_factory TYPE REF TO if_ixml_stream_factory,
      lo_istream        TYPE REF TO if_ixml_istream,
      lo_element        TYPE REF TO if_ixml_element,
      lo_version        TYPE REF TO if_ixml_node,
      lo_parser         TYPE REF TO if_ixml_parser.

************************************************************************
* Initialize
************************************************************************
    CLEAR ro_result.
    lo_xml = cl_ixml=>create( ).
    lo_xml_doc  = lo_xml->create_document( ).

************************************************************************
* Check the input
************************************************************************
    IF iv_data IS INITIAL.
      RETURN.
    ENDIF.

************************************************************************
* Parse the XML
************************************************************************
    lo_stream_factory = lo_xml->create_stream_factory( ).
    lo_istream = lo_stream_factory->create_istream_xstring( iv_data ).
    lo_parser = lo_xml->create_parser( stream_factory = lo_stream_factory
      istream        = lo_istream
      document       = lo_xml_doc ).
    lo_parser->add_strip_space_element( ).
    IF lo_parser->parse( ) <> 0.
      zcx_abapgit_exception=>raise( |Error while parsing XML| ). "#EC NOTEXT
    ENDIF.
    lo_istream->close( ).

************************************************************************
* Return the XML element to the caller
************************************************************************
    ro_result = lo_xml_doc->get_root_element( ).
  ENDMETHOD.
ENDCLASS.
