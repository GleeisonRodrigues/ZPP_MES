**---------------------------------------------------------------------------
* Programa: ZRPP073
* Módulo  : PP
* Transação: ZPPXX
* Descrição: Coleta de etiquetas IMF
* Autor : Gleison R. Silva      Data/Hora:25.08.2023 4:53:13 PM
*---------------------------------------------------------------------------
* Histórico de Alterações:
*---------------------------------------------------------------------------
* Data                Request    Autor                   |Alteração
*---------------------------------------------------------------------------
* DD/MM/AAAA |                  |                |Desenvolvimento Inicial
*---------------------------------------------------------------------------
REPORT zrpp073.
TABLES: ztpp046.
DATA: ok_code   TYPE sy-ucomm,
      gv_cursor TYPE char100.



START-OF-SELECTION.
  gv_cursor = 'ZTPP046-MATERIAL'.
  CALL SCREEN 0001.
*&---------------------------------------------------------------------*
*&      Module  ZM_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_pbo OUTPUT.
  SET PF-STATUS '0001'.
  SET TITLEBAR '0001'.
  SET CURSOR FIELD gv_cursor.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_pai INPUT.

  DATA(l_code) = ok_code.
  CLEAR: ok_code.

  CASE l_code.
    WHEN 'CANCE'.
      LEAVE PROGRAM.
    WHEN 'CLEAR'.
      LEAVE TO TRANSACTION sy-tcode.
    WHEN 'ENTE'.
      CLEAR: gv_cursor.


      IF ztpp046-material IS INITIAL AND gv_cursor IS INITIAL.
        gv_cursor = 'ZTPP046-MATERIAL'.
        EXIT.
      ENDIF.
      DATA(l_m) = strlen( ztpp046-material ).
      IF l_m <> 7.
        MESSAGE s000(zpp) WITH 'Material' ztpp046-material 'inválido' DISPLAY LIKE 'E'.
        CLEAR: ztpp046-material.
        gv_cursor = 'ZTPP046-MATERIAL'.
        EXIT.
      ENDIF.

      IF ztpp046-lote IS INITIAL AND gv_cursor IS INITIAL.
        gv_cursor = 'ZTPP046-LOTE'.
        EXIT.
      ENDIF.
      l_m = strlen( ztpp046-lote ).
      IF l_m <> 9.
        MESSAGE s000(zpp) WITH 'Lote' ztpp046-lote 'inválido' DISPLAY LIKE 'E'.
        CLEAR: ztpp046-lote.
        gv_cursor = 'ZTPP046-LOTE'.
        EXIT.
      ENDIF.
      IF ztpp046-etiqueta IS INITIAL.
        gv_cursor = 'ZTPP046-ETIQUETA'.
        EXIT.
      ENDIF.
      "l_m = strlen(  ).
      IF ztpp046-etiqueta+8(1) <> '/'.
        MESSAGE s000(zpp) WITH 'Etiqueta' ztpp046-etiqueta 'inválido' DISPLAY LIKE 'E'.
        CLEAR: ztpp046-etiqueta.
        gv_cursor = 'ZTPP046-ETIQUETA'.
        EXIT.
      ENDIF.

      ztpp046-quantidade = 25.
      IF NOT ztpp046-etiqueta IS INITIAL.
        SELECT SINGLE etiqueta FROM ztpp046 INTO @DATA(l_check)
            WHERE etiqueta = @ztpp046-etiqueta.
        IF sy-subrc = 0.
          MESSAGE s000(zpp) WITH 'Etiqueta' ztpp046-etiqueta 'já coletada' DISPLAY LIKE 'E'.
          CLEAR: ztpp046.
          gv_cursor = 'ZTPP046-MATERIAL'.
          EXIT.
        ENDIF.
      ENDIF.

*      IF ztpp046-quantidade IS INITIAL AND gv_cursor IS INITIAL.
*        gv_cursor = 'ZTPP046-QUANTIDADE'.
*      ENDIF.
      IF NOT ztpp046-etiqueta IS INITIAL AND
         NOT ztpp046-material IS INITIAL AND
         NOT ztpp046-lote IS INITIAL AND
         NOT ztpp046-quantidade IS INITIAL.

        SELECT SINGLE etiqueta FROM ztpp046 INTO l_check
          WHERE etiqueta = ztpp046-etiqueta.
        IF sy-subrc = 0.
          MESSAGE s000(zpp) WITH 'Etiqueta' ztpp046-etiqueta 'já coletada' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
        ztpp046-data = sy-datum.
        ztpp046-hora = sy-uzeit.
        MODIFY ztpp046 FROM ztpp046.
        COMMIT WORK.

        MESSAGE s000(zpp) WITH 'Etiqueta' ztpp046-etiqueta 'coletada'.

        CLEAR: ztpp046.
        gv_cursor = 'ZTPP046-MATERIAL'.

      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
