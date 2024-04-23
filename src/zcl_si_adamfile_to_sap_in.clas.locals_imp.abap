
class lcl_fill_data implementation.

  method ZMT_ADAM_FILE_TO_SAP_IN.
    call method ZDT_TABLE025
      importing
        out = out-MT_ADAM_FILE_TO_SAP_IN.
  endmethod.

  method ZDT_TABLE025.
    call method ZDT_TABLE025_ADAM_TAB
      importing
        out = out-ADAM.
  endmethod.

  method ZDT_TABLE025_ADAM_TAB.
    data: ls_out like line of out.
    call method ZDT_TABLE025_ADAM
      importing
        out = ls_out.
    do 3 times.
      append ls_out to out.
    enddo.
  endmethod.

  method ZDT_TABLE025_ADAM.
    out-IP = `String 1`. "#EC NOTEXT
    out-CH = `String 2`. "#EC NOTEXT
    out-COUNTER = `3 `. "#EC NOTEXT
    out-STATUS = `String 4`. "#EC NOTEXT
  endmethod.

endclass.

