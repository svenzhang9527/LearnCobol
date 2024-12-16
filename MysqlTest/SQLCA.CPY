       01  SQLCA.
       05  SQLCAID         PIC X(8).
       05  SQLCABC         PIC S9(9) COMP.
       05  SQLCODE         PIC S9(9) COMP.
       05  SQLERRML        PIC S9(9) COMP.
       05  SQLERRMC        PIC X(70).
       05  SQLERRP         PIC X(8).
       05  SQLERRD         OCCURS 6 TIMES PIC S9(15) COMP.
       05  SQLWARN         OCCURS 11 TIMES PIC X.
       05  SQLEXT          PIC X(8).