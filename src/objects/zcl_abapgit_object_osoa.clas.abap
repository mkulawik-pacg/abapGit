class ZCL_ABAPGIT_OBJECT_OSOA definition
  public
  inheriting from ZCL_ABAPGIT_OBJECT_OSOD
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
      !IV_LANGUAGE type SPRAS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_OSOA IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor(
        is_item     = is_item
        iv_language = iv_language ).
    me->mv_objvers = me->mc_objvers_a.
  ENDMETHOD.
ENDCLASS.
