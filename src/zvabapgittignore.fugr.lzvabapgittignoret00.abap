*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 15.09.2019 at 20:59:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVABAPGIT_IGNORE................................*
TABLES: ZVABAPGIT_IGNORE, *ZVABAPGIT_IGNORE. "view work areas
CONTROLS: TCTRL_ZVABAPGIT_IGNORE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVABAPGIT_IGNORE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVABAPGIT_IGNORE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVABAPGIT_IGNORE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVABAPGIT_IGNORE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVABAPGIT_IGNORE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVABAPGIT_IGNORE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVABAPGIT_IGNORE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVABAPGIT_IGNORE_TOTAL.

*.........table declarations:.................................*
TABLES: ZABAPGIT_IGNORE                .
