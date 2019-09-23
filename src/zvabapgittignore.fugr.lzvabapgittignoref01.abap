*----------------------------------------------------------------------*
***INCLUDE LZVABAPGITTIGNOREF01.
*----------------------------------------------------------------------*
FORM zvabapgit_form01.

  LOOP AT total[] ASSIGNING FIELD-SYMBOL(<ls_total>).
    CHECK <ls_total>+64(1) EQ 'N'.

    <ls_total>+44(12) = sy-uname.
    <ls_total>+56(8) = sy-datum.
  ENDLOOP.

ENDFORM.
