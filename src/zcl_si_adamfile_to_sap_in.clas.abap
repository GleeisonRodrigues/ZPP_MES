CLASS zcl_si_adamfile_to_sap_in DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zpp_ii_si_adamfile_to_sap_in .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_si_adamfile_to_sap_in IMPLEMENTATION.


  METHOD zpp_ii_si_adamfile_to_sap_in~si_adamfile_to_sap_in.
    TYPES: BEGIN OF ty_adam,
             ip      TYPE ztpp017-ip_adam,
             ch      TYPE ztpp017-ch,
             counter TYPE ztpp017-counter,
             status  TYPE char10,
           END OF ty_adam,
           BEGIN OF ty_conf_adam,
             icon           TYPE icon-id,
             counterold(20) TYPE n,
             message        TYPE text100.
            INCLUDE TYPE ztpp017.
    TYPES: END OF ty_conf_adam .

    DATA:
      ls_saida  TYPE ty_conf_adam,
      ls_save   TYPE ztpp015,
      lo_mes    TYPE REF TO zcl_pp_mes_rodas,
      lo_mes_a  TYPE REF TO zcl_pp_mes_acos,
      l_counter TYPE i.

    CREATE OBJECT: lo_mes_a, lo_mes.
    SELECT * FROM ztpp017 INTO TABLE @DATA(lt_adams).
    LOOP AT lt_adams INTO DATA(ls_adam).
      CLEAR: ls_saida.
      MOVE-CORRESPONDING ls_adam TO ls_saida.
      ls_saida-counterold = ls_adam-counter.
      CLEAR: ls_saida-counter.
      TRY .
          DATA(ls_txt)   = input-mt_adam_file_to_sap_in-adam[ ip = ls_adam-ip_adam ch = ls_adam-ch ].
          ls_saida-counter    = ls_txt-counter.
          ls_saida-counterold = ls_adam-counter.
          IF ls_txt-status = 'ok'.
            ls_saida-icon = icon_green_light.
          ELSE.
            ls_saida-icon = icon_red_light.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          ls_saida-icon = icon_red_light.
          ls_saida-counter = ls_saida-counterold.
      ENDTRY.
      CLEAR: ls_save.

      l_counter = ls_saida-counter - ls_saida-counterold.
      CHECK l_counter >= 0.

      MOVE-CORRESPONDING ls_saida TO ls_save.
      ls_save-id_di = ls_saida-ch.
      ls_saida-data  = sy-datum.
      ls_saida-hora  = sy-uzeit.
      IF NOT ls_save IS INITIAL.
        ls_save-data  = sy-datum.
        ls_save-hora  = sy-uzeit.
        INSERT ztpp015 FROM ls_save.
        COMMIT WORK.
      ENDIF.
      IF ls_saida-icon = icon_green_light.
        UPDATE ztpp016 SET status = abap_true WHERE ip_adam = ls_save-ip_adam.
        COMMIT WORK.
        UPDATE ztpp017 SET counter = ls_save-counter "data = sy-datum hora = sy-uzeit
        WHERE ip_adam = ls_save-ip_adam
        AND ch      = ls_save-id_di.
        COMMIT WORK.
      ELSE.
        UPDATE ztpp016 SET status = space WHERE ip_adam = ls_save-ip_adam.
        COMMIT WORK.
      ENDIF.
*      break-point.



*      IF ls_saida-icon = icon_green_light.
      l_counter = ls_saida-counter - ls_saida-counterold.
      IF NOT l_counter IS INITIAL.
        UPDATE ztpp017 SET data = sy-datum hora = sy-uzeit
              WHERE ip_adam = ls_save-ip_adam
              AND ch      = ls_save-id_di.
        COMMIT WORK.
      ENDIF.
      SELECT SINGLE acos FROM ztpp016 INTO @DATA(l_acos) WHERE ip_adam = @ls_save-ip_adam.
      IF l_acos IS INITIAL.
        lo_mes->conf_apont_adam(
        EXPORTING
          is_conf_adam = ls_saida    " Apontamento MES Rodas
          i_count   = l_counter
          ).
      ELSE.
        lo_mes_a->conf_apont_adam(
        EXPORTING
          is_conf_adam = ls_saida    " Apontamento MES AÇOS
          i_count   = l_counter
          i_acos    = abap_true
          ).
      ENDIF.
      CLEAR: l_acos.
*      ENDIF.
      CLEAR: l_counter.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
