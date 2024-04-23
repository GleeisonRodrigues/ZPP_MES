class ZCL_PP_MES_RODAS definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_status,
        selected   TYPE zel_selecionar,
        id_status  TYPE ztpp021-id_status,
        rec_status TYPE ztpp021-rec_status,
        id_grupo   TYPE ztpp021-id_grupo,
        grupo_st   TYPE ztpp021-grupo_st,
        auto       TYPE ztpp021-auto,
        setmat     TYPE ztpp021-setmat,
        pulso      TYPE ztpp021-pulso,
      END OF ty_status .
  types:
    BEGIN OF ty_grp_status,
        selected TYPE zel_selecionar,
        id_grupo TYPE ztpp020-id_grupo,
        grupo_st TYPE ztpp020-grupo_st,
      END OF ty_grp_status .
  types:
    BEGIN OF ty_orders,
        aufnr TYPE aufnr,
      END OF  ty_orders .
  types:
    tt_status TYPE TABLE OF ty_status .
  types:
    tt_grp_status TYPE TABLE OF ty_grp_status .
  types:
    tt_orders TYPE TABLE OF ty_orders .
  types:
    tyr_matnr TYPE RANGE OF matnr .
  types:
    tyr_lgort TYPE RANGE OF lgort_d .
  types:
    tyr_werks TYPE RANGE OF werks_d .
  types:
    BEGIN OF ty_rec,
        selected TYPE zel_selecionar,
        arbpl    TYPE ztpp019_1-recurso,
        ktext    TYPE ztpp019_1-descrec,
      END OF ty_rec .
  types:
    tt_rec TYPE TABLE OF ty_rec .
  types:
    range_datum          TYPE RANGE OF datum .
  types:
    tt_ztpp013 TYPE TABLE OF ztpp013 .
  types:
    BEGIN OF ty_conf_adam,
        icon           TYPE icon-id,
        counterold(20) TYPE n,
        message        TYPE text100.
            INCLUDE TYPE ztpp017.
    TYPES: END OF ty_conf_adam .
  types TY_MENGE type ZES_SAIDA_ALV_PINT_OP-QUANT .

  data:
    ct_components TYPE TABLE OF bapi_order_component .
  data:
    ct_header TYPE TABLE OF bapi_order_header1 .
  data CT_BAPIRET type BAPIRET2_T .
  data GS_ETIQUETA type ZTPP029 .
  data:
    gt_itens_ref TYPE TABLE OF zes_itens_ref_etq .
  data:
    gt_modify013 TYPE TABLE OF ztpp013 .

  methods GET_DATA_ORDER
    importing
      value(IT_ORDERS) type TT_ORDERS .
  methods ESTORNO_RETRABALHO
    importing
      value(IS_ZTPP013) type ZTPP013 .
  methods CREATE_ANDOM_QUALIDADE .
  methods CREATE_ORDER
    importing
      value(MATNR) type MATNR
      value(MENGE) type TY_MENGE
      value(DATA) type DATUM
    returning
      value(AUFNR) type AUFNR .
  methods CONF_APONT_ADAM
    importing
      value(IS_CONF_ADAM) type TY_CONF_ADAM
      value(I_COUNT) type I .
  methods CONF_ETQ
    importing
      value(I_ETQ) type ZELMM_ETIQUETA .
  methods EXPORT_MEM_REC
    importing
      value(ARBPL) type ZEL_RECURSO .
  methods REIMPRIMIR_ETQ
    importing
      value(I_ETQ) type ZELMM_ETIQUETA
    returning
      value(OK) type CHAR01 .
  methods GET_ETIQUETA_REF
    importing
      value(I_ETQ) type ZELMM_ETIQUETA
    returning
      value(NOK) type CHAR01 .
  methods SAVE_ETQ_PRINT .
  methods SAVE_ZTPP018
    importing
      value(IS_ZTPP018) type ZTPP018
    exporting
      value(MESSAGE) type TEXT100
    returning
      value(OK) type CHAR01 .
  methods GET_ORDENS_CP
    importing
      value(I_ARBPL) type ARBPL
      value(I_MATNR) type MATNR optional
    exporting
      value(ET_ORDENS) type ZTTPP_OP_CT .
  methods GET_ORDENS
    importing
      value(I_ARBPL) type ARBPL
      value(I_MATNR) type MATNR optional
    exporting
      value(ET_ORDENS) type ZTTPP_OP_CT .
  methods GET_STATUS
    importing
      value(ID_GRUPO) type ZTPP020-ID_GRUPO optional
    exporting
      value(ET_STATUS) type TT_STATUS .
  methods GET_GRP_STATUS
    exporting
      value(ET_GRP_STATUS) type TT_GRP_STATUS .
  methods MODIFY_RECURSO
    importing
      value(IS_ZTPP018) type ZTPP018 .
  methods GET_MARD
    importing
      value(IT_MATNR) type TYR_MATNR optional
      value(IT_LGORT) type TYR_LGORT optional
      value(IT_WERKS) type TYR_WERKS optional
    exporting
      value(ET_MARD) type MARD_TAB .
  methods CONF_APONT
    importing
      value(IS_DATA) type ZTPP013 .
  methods CONF_APP_1_INSP
    importing
      value(IT_PARAMETER) type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  methods CONF_APP_MAT
    importing
      value(IT_PARAMETER) type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  methods GET_RECURSO
    changing
      !T_RECURSOS type TT_REC .
  methods GET_NUMBER_OPERATION
    importing
      value(I_AUFNR) type AUFNR
      value(I_ARBPLI) type RU_ARBPLI
      value(I_CHECKAP) type CHAR01 optional
    exporting
      value(OPERATION_NUMBER) type VORNR
      value(BAIXA_MAT) type CHAR01
      value(APONT_OK) type CHAR01 .
  methods GET_TURNO
    importing
      value(I_ARBPL) type ARBPL
    exporting
      value(E_BUDAT) type BUDAT
      value(E_SCHPROG) type SCHPROG
      value(ES_TURNO) type ZTPP028_1 .
  methods APONT_TP_1
    importing
      !LT_DATA type TT_ZTPP013
      !R_DATA type RANGE_DATUM .
  methods APONT_TP_2
    importing
      !LT_DATA type TT_ZTPP013
      !R_DATA type RANGE_DATUM .
  methods APONT_TP_3
    importing
      !LT_DATA type TT_ZTPP013
      !R_DATA type RANGE_DATUM .
  methods APONT_TP_ESTOR
    importing
      !LT_DATA type TT_ZTPP013
      !R_DATA type RANGE_DATUM .
  methods GET_IMSEG
    returning
      value(T_IMSEG) type TY_T_IMSEG .
  methods SAVE_MEMORY_ETQ_REF
    importing
      value(IS_ZTPP013) type ZTPP013 .
  methods CLEAR_IMSEG .
  methods CREATE_EXCEL_ANDOM_DAY .
  methods CREATE_ANDOM_STATUSREC .
  methods SAVE_MOV
    importing
      value(IS_ZTPP013) type ZTPP013
    returning
      value(IDMOV) type ZIDMOVPCF .
  methods SAVE_MOV_MAN
    importing
      value(IS_ZTPP013) type ZTPP013
      value(NO_UPD_TIME) type CHAR01 optional
    returning
      value(IDMOV) type ZIDMOVPCF .
  methods ESTORNA_MOV
    importing
      value(IS_ZTPP013) type ZTPP013 .
  methods ESTORNA_MOV_MAN
    importing
      value(IS_ZTPP013) type ZTPP013 .
  methods SAVE_CONF_TPP013
    importing
      value(IS_ZTPP013) type ZTPP013
    returning
      value(IDMOV) type ZIDMOVPCF .
  methods PESAGEM_USINAGEM
    importing
      value(IS_ZTPP039) type ZTPP039 .
  PROTECTED SECTION.
private section.

  constants:
    BEGIN OF gc,
        firstday     TYPE char02           VALUE '01',
        bapi_lastday TYPE char100          VALUE 'LAST_DAY_OF_MONTHS',
      END OF gc .
  data DATAF type DATUM .
  data DATAI type DATUM .
  data ET_IMSEG type TY_T_IMSEG .
  data:
    gt_etqs_ref TYPE TABLE OF ztpp029 .
  data GV_RECURSOOLD type RU_ARBPLI .

  methods CONF_APONT_ADAM_INJ
    importing
      !IS_CONFCT type ZTPP018_1
      !IS_CONF_ADAM type TY_CONF_ADAM
      !I_COUNT type I
      !LS_CONFADAM type ZTPP017 .
  methods CONF_APONT_ADAM_USIN
    importing
      !IS_CONFCT type ZTPP018_1
      !IS_CONF_ADAM type TY_CONF_ADAM
      !I_COUNT type I
      !LS_CONFADAM type ZTPP017 .
  methods CONF_APONT_ADAM_ESTAN
    importing
      !IS_CONFCT type ZTPP018_1
      !IS_CONF_ADAM type TY_CONF_ADAM
      !I_COUNT type I
      !LS_CONFADAM type ZTPP017 .
  methods CHECK_OP
    importing
      value(ARBPL) type ARBPL
    changing
      value(AUFNR) type AUFNR .
  methods PRINT_ETQ .
  methods CALCULE_DATE
    importing
      value(I_ARBPL) type ARBPL .
  methods CHECK_MOTIVO
    importing
      value(I_MOTIVO) type ZEL_ID_STATUS
    returning
      value(OK) type CHAR01 .
  methods GERAR_NUMBER_ETQ_REF
    returning
      value(NUMBER) type ZELMM_ETIQUETA .
  methods UPD_ZTPP013
    importing
      value(ZIDMOVPCF) type ZTPP013-ZIDMOVPCF
      value(GJAHR) type ZTPP013-GJAHR
      value(DATUM) type DATUM optional
      value(NDOC_ESTORN) type ZIDMOVPCF .
  methods UPD_ZTPP013_MOV
    changing
      value(CS_UPD_013) type ZTPP013 .
ENDCLASS.



CLASS ZCL_PP_MES_RODAS IMPLEMENTATION.


  METHOD apont_tp_1.
    DATA:  "ls_order_objects TYPE bapi_pp_order_objects,
*           lt_header        TYPE TABLE OF bapi_order_header1,
*           lt_operations    TYPE TABLE OF bapi_order_operation1,
      lt_components TYPE TABLE OF bapi_order_component,
      ls_imseg      TYPE imseg,
      ls_error      TYPE ztpp013_3,
      l_mov         TYPE char01.

*    BREAK-POINT.
    DATA(lt_data_aux) = lt_data.

    DELETE lt_data_aux WHERE data_m <> r_data[ 1 ]-low .
    DELETE lt_data_aux WHERE tpmov <> 1.
    DELETE lt_data_aux WHERE estorno <> abap_false.

    LOOP AT lt_data_aux INTO DATA(ls_data). "FROM l_index."WHERE tpmov = 1 AND estorno = abap_false."AND data_m IN r_data . " Apontamento de Entrada
*      IF ls_data-data_m <> r_data[ 1 ]-low OR ls_data-tpmov <> 1 OR ls_data-estorno <> abap_false.
*        EXIT.
*      ENDIF.
      DATA(ls_header) = ct_header[ order_number = ls_data-aufnr ].
      CLEAR: lt_components.
      lt_components = ct_components.
      SORT lt_components BY order_number.
      DELETE lt_components WHERE order_number <> ls_data-aufnr.
      SORT lt_components BY reservation_number reservation_item.
      CLEAR l_mov.
*      ls_order_objects-header     = abap_true.
*      ls_order_objects-operations = abap_true.
*      ls_order_objects-components = abap_true.
*
*      "   APPEND ls_data TO lt_upt_data.
*
*      CLEAR: lt_header, lt_operations, lt_components.
*
*      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
*        EXPORTING
*          number        = ls_data-aufnr
*          order_objects = ls_order_objects
*        TABLES
*          header        = lt_header
*          operation     = lt_operations
*          component     = lt_components.
      IF ls_data-mov_mat = abap_true.
        LOOP AT lt_components INTO DATA(ls_comp) WHERE operation = ls_data-vornr.
*** Item de movimento
          CLEAR: ls_imseg.
          SELECT SINGLE * FROM resb INTO @DATA(ls_resb)
            WHERE rsnum = @ls_comp-reservation_number
              AND rspos = @ls_comp-reservation_item.
          MOVE-CORRESPONDING ls_resb TO ls_imseg.
*          ls_imseg-erfmg        = ( ls_comp-entry_quantity / lt_header[ 1 ]-target_quantity ) * ls_data-lmnga.
          ls_imseg-erfmg        = ( ls_comp-entry_quantity / ls_header-target_quantity ) * ls_data-lmnga.
          SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = ls_data-aufnr.
          ls_imseg-sgtxt = ls_data-zidmovpcf.
          APPEND ls_imseg TO et_imseg.
          CLEAR: ls_resb.
          l_mov = abap_true.
        ENDLOOP.
*        IF line_exists( lt_operations[ operation_number = ls_data-vornr opr_cntrl_key = 'YBP5' ] ).
        SELECT SINGLE aufpl AS routing_no, aplzl AS counter
        INTO @DATA(ls_operation)
        FROM afvc
        WHERE aufpl EQ @ls_header-routing_no
          AND vornr EQ @ls_data-vornr
          AND steus = 'YBP5'.
        IF sy-subrc = 0.
          CLEAR: ls_imseg.
          ls_imseg-matnr        = ls_data-matnr.
          ls_imseg-werks        = 1015.
          ls_imseg-lgort        = ls_data-lgort_dest.
          ls_imseg-bwart        = '101'.
          ls_imseg-erfmg        = ls_data-lmnga.
          ls_imseg-erfme        = ls_header-unit.
          ls_imseg-aufnr        = ls_data-aufnr.
          ls_imseg-kzbew        = 'F'.
          SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = ls_data-aufnr.
*            COLLECT ls_imseg INTO ct_imseg.
          ls_imseg-sgtxt = ls_data-zidmovpcf.
          APPEND ls_imseg TO et_imseg.
          l_mov = abap_true.
        ENDIF.
*        DATA(ls_operation) = lt_operations[ operation_number = ls_data-vornr ].
        SELECT SINGLE * FROM afvv INTO @DATA(ls_afvv)
          WHERE aufpl = @ls_operation-routing_no
            AND aplzl = @ls_operation-counter.
        IF sy-subrc = 0 AND NOT ls_afvv-vgw04 IS INITIAL. " Apontamento de Retrabalho
          LOOP AT lt_components INTO ls_comp.
            CLEAR: ls_resb.
            SELECT SINGLE * FROM resb INTO ls_resb
              WHERE rsnum = ls_comp-reservation_number
                AND rspos = ls_comp-reservation_item
                AND sortf = abap_true.
            CHECK sy-subrc = 0.
            MOVE-CORRESPONDING ls_resb TO ls_imseg.
            ls_imseg-erfmg        = ( ls_comp-entry_quantity / ls_header-target_quantity ) * ls_data-lmnga.
            SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = ls_data-aufnr.
*              COLLECT ls_imseg INTO ct_imseg.
            ls_imseg-sgtxt = ls_data-zidmovpcf.
            APPEND ls_imseg TO et_imseg.
            l_mov = abap_true.
          ENDLOOP.
        ENDIF.
        IF ls_data-lgort <> ls_data-lgort_dest.
          CLEAR: ls_comp.
          ls_comp = lt_components[ 1 ].
          CLEAR: ls_resb.
          SELECT SINGLE * FROM resb INTO ls_resb
            WHERE rsnum = ls_comp-reservation_number
              AND sortf = 'Y'.
          IF sy-subrc = 0.
            CLEAR: ls_imseg.
            ls_comp = lt_components[ reservation_number = ls_resb-rsnum reservation_item = ls_resb-rspos ].
            ls_imseg-matnr        = ls_comp-material.
            ls_imseg-werks        = 1015.
            ls_imseg-lgort        = ls_data-lgort.
            ls_imseg-umwrk        = 1015.
            ls_imseg-umlgo        = ls_data-lgort_dest.
            ls_imseg-bwart        = '311'.
            ls_imseg-erfmg        = ls_data-lmnga.
            ls_imseg-erfme        = ls_comp-entry_uom.
*              COLLECT ls_imseg INTO ct_imseg.
            ls_imseg-sgtxt = ls_data-zidmovpcf.
            APPEND ls_imseg TO et_imseg.
            l_mov = abap_true.
          ENDIF.
        ENDIF.
        IF  l_mov = abap_false.
          TRY.
              upd_ztpp013_mov(
                CHANGING
                  cs_upd_013 = ls_data    " Apontamento MES Rodas
              ).
*              UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_data-zidmovpcf AND gjahr = ls_data-gjahr.
            CATCH cx_sy_open_sql_db INTO DATA(lf_err).
              DATA(lf_message) = lf_err->get_text( ).
              CLEAR: ls_error.
              ls_error-datum   = sy-datum.
              ls_error-uzeit   = sy-uzeit.
              ls_error-ip_adam = ls_data-zidmovpcf.
              ls_error-matnr   = lf_message.
              INSERT ztpp013_3 FROM ls_error.
              COMMIT WORK.
          ENDTRY.
        ENDIF.
      ELSE.
        IF ls_data-lgort <> ls_data-lgort_dest.
          CLEAR: ls_comp.
          ls_comp = lt_components[ 1 ].
          CLEAR: ls_resb.
          SELECT SINGLE * FROM resb INTO ls_resb
            WHERE rsnum = ls_comp-reservation_number
              AND sortf = 'Y'.
          IF sy-subrc = 0.
            CLEAR: ls_imseg.
            ls_comp = lt_components[ reservation_number = ls_resb-rsnum reservation_item = ls_resb-rspos ].
            ls_imseg-matnr        = ls_comp-material.
            ls_imseg-werks        = 1015.
            ls_imseg-lgort        = ls_data-lgort.
            ls_imseg-umwrk        = 1015.
            ls_imseg-umlgo        = ls_data-lgort_dest.
            ls_imseg-bwart        = '311'.
            ls_imseg-erfmg        = ls_data-lmnga.
            ls_imseg-erfme        = ls_comp-entry_uom.
*              COLLECT ls_imseg INTO ct_imseg.
            ls_imseg-sgtxt = ls_data-zidmovpcf.
            APPEND ls_imseg TO et_imseg.
          ENDIF.
        ELSE.
          upd_ztpp013_mov(
                CHANGING
                  cs_upd_013 = ls_data    " Apontamento MES Rodas
              ).
*          UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_data-zidmovpcf AND gjahr = ls_data-gjahr.
        ENDIF.
      ENDIF.
    ENDLOOP.
*
  ENDMETHOD.


  METHOD apont_tp_2.
    DATA:  "ls_order_objects TYPE bapi_pp_order_objects,
*           lt_header        TYPE TABLE OF bapi_order_header1,
*           lt_operations    TYPE TABLE OF bapi_order_operation1,
      lt_components TYPE TABLE OF bapi_order_component,
      ls_imseg      TYPE imseg,
      l_notupdt     TYPE char01.

    DATA(lt_data_aux) = lt_data.

    DELETE lt_data_aux WHERE data_m <> r_data[ 1 ]-low .
    DELETE lt_data_aux WHERE tpmov <> 2.
    DELETE lt_data_aux WHERE estorno <> abap_false.
    LOOP AT lt_data_aux INTO DATA(ls_data)." WHERE tpmov = 2 AND data_m IN r_data AND estorno = abap_false. " Apontamento de Retrabalho
      CLEAR: l_notupdt.

      DATA(ls_header) = ct_header[ order_number = ls_data-aufnr ].
      CLEAR: lt_components.
      lt_components = ct_components.
      SORT lt_components BY order_number.
      DELETE lt_components WHERE order_number <> ls_data-aufnr.
      SORT lt_components BY reservation_number reservation_item.
*      ls_order_objects-header     = abap_true.
*      ls_order_objects-operations = abap_true.
*      ls_order_objects-components = abap_true.
*
*      CLEAR: lt_header, lt_operations, lt_components.
*
**      APPEND ls_data TO lt_upt_data.
*
*      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
*        EXPORTING
*          number        = ls_data-aufnr
*          order_objects = ls_order_objects
*        TABLES
*          header        = lt_header
*          operation     = lt_operations
*          component     = lt_components.

*        LOOP AT lt_components INTO ls_comp WHERE operation = ls_data-vornr.
      IF ls_data-lgort <> ls_data-lgort_dest.
        DATA(ls_comp) = lt_components[ 1 ].
        SELECT SINGLE * FROM resb INTO @DATA(ls_resb)
          WHERE rsnum = @ls_comp-reservation_number
*              AND rspos = ls_comp-reservation_item
            AND sortf = 'Y'.
        IF sy-subrc = 0.
          CLEAR: ls_imseg.
          ls_comp = lt_components[ reservation_number = ls_resb-rsnum reservation_item = ls_resb-rspos ].
          ls_imseg-matnr        = ls_comp-material.
          ls_imseg-werks        = 1015.
          ls_imseg-lgort        = ls_data-lgort.
          ls_imseg-umwrk        = 1015.
          ls_imseg-umlgo        = ls_data-lgort_dest.
          ls_imseg-bwart        = '311'.
          ls_imseg-erfmg        = ls_data-rmnga.
          ls_imseg-erfme        = ls_comp-entry_uom.
*              COLLECT ls_imseg INTO ct_imseg.
          ls_imseg-sgtxt = ls_data-zidmovpcf.
          APPEND ls_imseg TO et_imseg.
          l_notupdt = abap_true.
        ENDIF.
      ENDIF.
*        ENDLOOP.
*      DATA(ls_operation) = lt_operations[ operation_number = ls_data-vornr ].
      SELECT SINGLE aufpl AS routing_no, aplzl AS counter
      INTO @DATA(ls_operation)
      FROM afvc
      WHERE aufpl EQ @ls_header-routing_no
        AND vornr EQ @ls_data-vornr.
      SELECT SINGLE * FROM afvv INTO @DATA(ls_afvv)
        WHERE aufpl = @ls_operation-routing_no
          AND aplzl = @ls_operation-counter.

      IF sy-subrc = 0 AND NOT ls_afvv-vgw04 IS INITIAL. " Apontamento de Retrabalho
        LOOP AT lt_components INTO ls_comp.
          SELECT SINGLE * FROM resb INTO ls_resb
            WHERE rsnum = ls_comp-reservation_number
              AND rspos = ls_comp-reservation_item
              AND sortf = abap_true.
          CHECK sy-subrc = 0.
          CLEAR: ls_imseg.
          MOVE-CORRESPONDING ls_resb TO ls_imseg.
          ls_imseg-erfmg        = ( ls_comp-entry_quantity / ls_header-target_quantity ) * ls_data-rmnga.

          SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = ls_data-aufnr.
          ls_imseg-sgtxt = ls_data-zidmovpcf.
          APPEND ls_imseg TO et_imseg.
          CLEAR: ls_resb.
          l_notupdt = abap_true.
        ENDLOOP.
      ENDIF.
      IF l_notupdt IS INITIAL.
        upd_ztpp013_mov(
          CHANGING
            cs_upd_013 = ls_data   " Apontamento MES Rodas
        ).
*        UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_data-zidmovpcf AND gjahr = ls_data-gjahr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD apont_tp_3.
    DATA: "ls_order_objects TYPE bapi_pp_order_objects,
*          lt_header        TYPE TABLE OF bapi_order_header1,
*          lt_operations    TYPE TABLE OF bapi_order_operation1,
      lt_components TYPE TABLE OF bapi_order_component,
      ls_imseg      TYPE imseg,
      datai         TYPE datum,
      dataf         TYPE datum.

    DATA(lt_data_aux) = lt_data.
    DELETE lt_data_aux WHERE data_m <> r_data[ 1 ]-low .
    DELETE lt_data_aux WHERE tpmov <> 3.
    DELETE lt_data_aux WHERE estorno <> abap_false.

    LOOP AT lt_data_aux INTO DATA(ls_data)." WHERE tpmov = 3 AND data_m IN r_data AND estorno = abap_false. " Apontamento de Refugo
      datai = ls_data-data_m(6) && '01'.
      CALL FUNCTION 'LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = datai
        IMPORTING
          last_day_of_month = dataf
        EXCEPTIONS
          day_in_no_date    = 1.

      IF NOT ls_data-idetiqueta IS INITIAL.
        SELECT SINGLE * FROM ztpp029 INTO @DATA(ls_etiqueta)
          WHERE idetiqueta = @ls_data-idetiqueta
            AND lido       = @abap_true.
        CHECK sy-subrc = 0.
      ENDIF.
      SELECT SINGLE * FROM mara INTO @DATA(ls_mara) WHERE matnr = @ls_data-matnr.
      IF NOT ls_data-aufnr IS INITIAL.
        DATA(ls_header) = ct_header[ order_number = ls_data-aufnr ].
        CLEAR: lt_components.
        lt_components = ct_components.
        SORT lt_components BY order_number.
        DELETE lt_components WHERE order_number <> ls_data-aufnr.
        SORT lt_components BY reservation_number reservation_item.
*        ls_order_objects-header     = abap_true.
*        ls_order_objects-operations = abap_true.
*        ls_order_objects-components = abap_true.
*
*        CLEAR: lt_header, lt_operations, lt_components.
*
**      APPEND ls_data TO lt_upt_data.
*
*        CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
*          EXPORTING
*            number        = ls_data-aufnr
*            order_objects = ls_order_objects
*          TABLES
*            header        = lt_header
*            operation     = lt_operations
*            component     = lt_components.
        LOOP AT lt_components INTO DATA(ls_comp) WHERE operation = ls_data-vornr.
          CLEAR: ls_imseg.
          SELECT SINGLE * FROM resb INTO @DATA(ls_resb)
            WHERE rsnum = @ls_comp-reservation_number
              AND rspos = @ls_comp-reservation_item
              AND sortf <> 'Y'.
          CHECK sy-subrc = 0.
          CLEAR: ls_imseg.
          MOVE-CORRESPONDING ls_resb TO ls_imseg.
          ls_imseg-erfmg        = ( ls_comp-entry_quantity / ls_header-target_quantity ) * ls_data-xmnga.
*          ls_imseg-erfmg        = ls_data-xmnga.
          SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = ls_data-aufnr.
          ls_imseg-sgtxt = ls_data-zidmovpcf.
          APPEND ls_imseg TO et_imseg.
          CLEAR: ls_resb.
        ENDLOOP.
        CLEAR: ls_imseg,ls_resb,ls_comp.
        IF NOT lt_components IS INITIAL.
          ls_comp = lt_components[ 1 ].
        ENDIF.
        SELECT SINGLE * FROM resb INTO ls_resb
            WHERE rsnum = ls_comp-reservation_number
              AND sortf = 'Y'.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_resb TO ls_imseg.
          ls_imseg-erfmg        = ls_data-xmnga.
          SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = ls_data-aufnr.
          ls_imseg-sgtxt = ls_data-zidmovpcf.
          APPEND ls_imseg TO et_imseg.
        ENDIF.
        CLEAR: ls_imseg.
        CASE ls_mara-matkl(4).
          WHEN 'RD07'.
            SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc)
              WHERE name = 'Z_REFUGO_07'.
          WHEN 'RD11'.
            SELECT SINGLE * FROM tvarvc INTO ls_tvarvc
                     WHERE name = 'Z_REFUGO_11'.
        ENDCASE.
        SELECT SINGLE *
            INTO @DATA(ls_mara_ref)
            FROM mara
           WHERE matnr EQ @ls_tvarvc-low.
        SELECT SINGLE *
            INTO @DATA(ls_marc)
            FROM marc
           WHERE matnr = @ls_mara_ref-matnr
             AND werks = 1015.

        ls_imseg-matnr      = ls_mara_ref-matnr.
        ls_imseg-lgort      = ls_marc-lgpro.
        ls_imseg-bwart      = 531.
        ls_imseg-werks      = 1015.
        ls_imseg-erfmg      = ls_mara-ntgew * ls_data-xmnga.
        ls_imseg-erfme      = ls_mara_ref-meins.
        ls_imseg-aufnr      = ls_data-aufnr.
*        ls_imseg-kzbew      = 'F'.
        SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = ls_data-aufnr.
*        ls_imseg-sgtxt = ls_data-zidmovpcf.
        ls_imseg-sgtxt = ls_data-zidmovpcf.
        APPEND ls_imseg TO et_imseg.
*        COLLECT ls_imseg INTO ct_imseg.
        CLEAR: ls_resb.
      ELSE.
        SELECT SINGLE *
            INTO ls_mara_ref
            FROM mara
           WHERE matnr EQ ls_data-ref_estq.
        SELECT SINGLE *
            INTO ls_marc
            FROM marc
           WHERE matnr = ls_data-ref_estq
             AND werks = 1015.
        ls_imseg-matnr      = ls_data-ref_estq.
        ls_imseg-lgort      = ls_marc-lgpro.
        ls_imseg-bwart      = 101.
        ls_imseg-werks      = 1015.
        ls_imseg-erfmg      = ls_mara-ntgew * ls_data-xmnga.
        ls_imseg-erfme      = ls_mara_ref-meins.
        SELECT SINGLE aufnr FROM zvs_afpo_afkko INTO @DATA(l_aufnr)
        WHERE matnr = @ls_data-ref_estq
          AND gstrp >= @datai
          AND gstrp <= @dataf.
        CHECK NOT l_aufnr IS INITIAL.
        ls_imseg-aufnr      = l_aufnr.
        SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = l_aufnr.
        ls_imseg-sgtxt = ls_data-zidmovpcf.
        ls_imseg-kzbew      = 'F'.
        APPEND ls_imseg TO et_imseg.
        CLEAR: ls_imseg.
        ls_imseg-matnr      = ls_data-matnr.
        SELECT SINGLE lgpro FROM marc INTO ls_imseg-lgort WHERE matnr = ls_data-matnr AND werks = 1015.
        ls_imseg-bwart      = 261.
        ls_imseg-werks      = 1015.
        ls_imseg-erfmg      = ls_data-xmnga.
        SELECT SINGLE meins FROM mara INTO ls_imseg-erfme WHERE matnr = ls_data-matnr.
        ls_imseg-aufnr      = l_aufnr.
        SELECT SINGLE objnr FROM caufv INTO ls_imseg-objnr WHERE aufnr = l_aufnr.
        ls_imseg-sgtxt = ls_data-zidmovpcf.
        APPEND ls_imseg TO et_imseg.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD apont_tp_estor.
    DATA: ls_imseg         TYPE imseg.

    DATA(lt_data_aux) = lt_data.
    DELETE lt_data_aux WHERE data_m <> r_data[ 1 ]-low .
    DELETE lt_data_aux WHERE estorno <> abap_true.

    LOOP AT lt_data_aux INTO DATA(ls_data)."WHERE data_m IN r_data AND estorno = abap_true. " Apontamento de Estorno

      SELECT SINGLE * FROM ztpp013 INTO @DATA(ls_013)
        WHERE ndoc_estorn = @ls_data-zidmovpcf
          AND gjahr = @ls_data-gjahr.
      IF sy-subrc <> 0.
        upd_ztpp013_mov( CHANGING cs_upd_013 = ls_data ).
        CONTINUE.
      ENDIF.
      SELECT SINGLE * FROM ztpp013_1 INTO @DATA(ls_check)
        WHERE zidmovpcf = @ls_013-zidmovpcf AND budat = @ls_013-data_m.
      IF sy-subrc = 0.
        DELETE FROM ztpp013_1 WHERE zidmovpcf = ls_013-zidmovpcf
        AND budat = ls_013-data_m.
        COMMIT WORK.
      ENDIF.
      SELECT  * FROM mseg INTO TABLE @DATA(lt_imseg_aux)
        WHERE sgtxt = @ls_013-zidmovpcf.
      IF sy-subrc = 0.
        LOOP AT lt_imseg_aux INTO DATA(ls_mseg).
          CLEAR: ls_imseg.
*                MOVE-CORRESPONDING ls_mseg TO  ls_imseg.
          CASE ls_mseg-bwart.
            WHEN 101.
              ls_imseg-bwart = 102.
              ls_imseg-kzbew = 'F'.
              ls_imseg-lgort = ls_mseg-lgort.
            WHEN 261.
              ls_imseg-bwart = 262.
              ls_imseg-lgort = ls_mseg-lgort.
            WHEN 531.
              ls_imseg-bwart = 532.
              ls_imseg-lgort = ls_mseg-lgort.
            WHEN 311.
              ls_imseg-bwart = 311.
              DATA(l_lgort)  = ls_mseg-lgort.
              ls_imseg-lgort = ls_mseg-umlgo.
              ls_imseg-umlgo = l_lgort .
          ENDCASE.
          ls_imseg-sgtxt = ls_data-zidmovpcf.
          ls_imseg-matnr = ls_mseg-matnr.
          ls_imseg-werks = ls_mseg-werks.
          ls_imseg-erfme = ls_mseg-erfme.
          ls_imseg-erfmg = ls_mseg-erfmg.
          ls_imseg-aufnr = ls_mseg-aufnr.
          APPEND ls_imseg TO et_imseg.
        ENDLOOP.
      ELSE.
        SORT et_imseg BY sgtxt.
        IF line_exists( et_imseg[ sgtxt = ls_013-zidmovpcf ] ) .
          upd_ztpp013_mov( CHANGING cs_upd_013 = ls_013 ).
          upd_ztpp013_mov( CHANGING cs_upd_013 = ls_data ).
*            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_013-zidmovpcf  AND gjahr = ls_013-gjahr.
*            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_data-zidmovpcf AND gjahr = ls_013-gjahr.
          DELETE et_imseg WHERE sgtxt = ls_013-zidmovpcf.
        ELSE.
          IF ls_013-mov = abap_false.
            upd_ztpp013_mov( CHANGING cs_upd_013 = ls_013 ).
          ENDIF.
          upd_ztpp013_mov( CHANGING cs_upd_013 = ls_data ).
*            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_013-zidmovpcf  AND gjahr = ls_013-gjahr.
*            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_data-zidmovpcf AND gjahr = ls_data-gjahr.
        ENDIF.
      ENDIF.
*      SELECT SINGLE * FROM ztpp013 INTO @DATA(ls_013)
*        WHERE ndoc_estorn = @ls_data-zidmovpcf
*          AND gjahr = @ls_data-gjahr.
*      IF sy-subrc = 0.
*        IF ls_013-mblnr IS INITIAL.
*          SORT et_imseg BY sgtxt.
*          IF line_exists( et_imseg[ sgtxt = ls_013-zidmovpcf ] ) .
*            upd_ztpp013_mov( CHANGING cs_upd_013 = ls_013 ).
*            upd_ztpp013_mov( CHANGING cs_upd_013 = ls_data ).
**            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_013-zidmovpcf  AND gjahr = ls_013-gjahr.
**            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_data-zidmovpcf AND gjahr = ls_013-gjahr.
*            DELETE et_imseg WHERE sgtxt = ls_013-zidmovpcf.
*          ELSE.
*            SELECT SINGLE * FROM ztpp013_1 INTO @DATA(ls_check)
*              WHERE zidmovpcf = @ls_013-zidmovpcf AND budat = @ls_013-data_m.
*            IF sy-subrc = 0.
**              UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_013-zidmovpcf  AND gjahr = ls_013-gjahr.
*              DELETE FROM ztpp013_1 WHERE zidmovpcf = ls_013-zidmovpcf
*              AND budat = ls_013-data_m.
*              COMMIT WORK.
*            ENDIF.
*            IF ls_013-mov = abap_false.
*              upd_ztpp013_mov( CHANGING cs_upd_013 = ls_013 ).
*            ENDIF.
*            upd_ztpp013_mov( CHANGING cs_upd_013 = ls_data ).
**            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_013-zidmovpcf  AND gjahr = ls_013-gjahr.
**            UPDATE ztpp013 SET mov = abap_true WHERE zidmovpcf = ls_data-zidmovpcf AND gjahr = ls_data-gjahr.
*          ENDIF.
*        ELSE.
*          SELECT SINGLE * FROM ztpp013_1 INTO ls_check
*                WHERE zidmovpcf = ls_013-zidmovpcf AND budat = ls_013-data_m.
*          IF sy-subrc = 0.
*            DELETE FROM ztpp013_1 WHERE zidmovpcf = ls_013-zidmovpcf
*            AND budat = ls_013-data_m.
*            COMMIT WORK.
*          ENDIF.
*          SELECT  * FROM mseg INTO TABLE @DATA(lt_imseg_aux)
*            WHERE mblnr = @ls_013-mblnr
*              AND mjahr = @ls_013-mjahr
*              AND xauto = @space
*              AND sgtxt = @ls_013-zidmovpcf.
*          IF sy-subrc = 0.
*            LOOP AT lt_imseg_aux INTO DATA(ls_mseg).
*              CLEAR: ls_imseg.
**                MOVE-CORRESPONDING ls_mseg TO  ls_imseg.
*              CASE ls_mseg-bwart.
*                WHEN 101.
*                  ls_imseg-bwart = 102.
*                  ls_imseg-kzbew = 'F'.
*                  ls_imseg-lgort = ls_mseg-lgort.
*                WHEN 261.
*                  ls_imseg-bwart = 262.
*                  ls_imseg-lgort = ls_mseg-lgort.
*                WHEN 531.
*                  ls_imseg-bwart = 532.
*                  ls_imseg-lgort = ls_mseg-lgort.
*                WHEN 311.
*                  ls_imseg-bwart = 311.
*                  DATA(l_lgort)  = ls_mseg-lgort.
*                  ls_imseg-lgort = ls_mseg-umlgo.
*                  ls_imseg-umlgo = l_lgort .
*              ENDCASE.
*              ls_imseg-sgtxt = ls_data-zidmovpcf.
*              ls_imseg-matnr = ls_mseg-matnr.
*              ls_imseg-werks = ls_mseg-werks.
*              ls_imseg-erfme = ls_mseg-erfme.
*              ls_imseg-erfmg = ls_mseg-erfmg.
*              ls_imseg-aufnr = ls_mseg-aufnr.
*              APPEND ls_imseg TO et_imseg.
*            ENDLOOP.
*          ELSE.
*            upd_ztpp013_mov( CHANGING cs_upd_013 = ls_data ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD calcule_date.
    get_turno(
      EXPORTING
        i_arbpl   = i_arbpl    " Centro de trabalho
      IMPORTING
        e_budat   = DATA(e_budat)    " Data de lançamento
    ).
    datai = e_budat(6) && gc-firstday.
    CALL FUNCTION gc-bapi_lastday
      EXPORTING
        day_in            = datai
      IMPORTING
        last_day_of_month = dataf
      EXCEPTIONS
        day_in_no_date    = 1.
  ENDMETHOD.


  METHOD check_motivo.
    SELECT SINGLE * FROM ztpp025
      INTO @DATA(ls_check)
      WHERE id_grupo  = 1
        AND id_status = @i_motivo.
    IF sy-subrc <> 0.
      ct_bapiret = VALUE #(
      ( type = 'E' id = 'ZPP' number = '000' message_v1 = 'Motivo' message_v2 = i_motivo message_v3 = 'não cadastrado' )
       ).
      EXIT.
    ENDIF.

    ok = abap_true.
  ENDMETHOD.


  METHOD check_op.
    DATA: ls_ztpp018   TYPE ztpp018.
    calcule_date( i_arbpl = arbpl ).
    "Busca ultima OP
    SELECT SINGLE aufnr FROM zvs_afpo_afkko INTO @DATA(l_aufnr)
      WHERE aufnr =  @aufnr
        AND gstrp  >= @datai
        AND gstrp  <= @dataf.
    IF sy-subrc <> 0.
      SELECT SINGLE matnr FROM afpo INTO @DATA(l_matnr)
      WHERE aufnr =  @aufnr.

      SELECT SINGLE aufnr FROM zvs_afpo_afkko INTO l_aufnr
         WHERE matnr   = l_matnr
           AND gstrp  >= datai
           AND gstrp  <= dataf.
      IF sy-subrc <> 0.
        CLEAR: aufnr.
      ELSE.
        aufnr = l_aufnr.
        SELECT SINGLE *
               FROM ztpp018_1
               INTO CORRESPONDING FIELDS OF @ls_ztpp018
                 WHERE arbpl = @arbpl.
        ls_ztpp018-aufnr   = l_aufnr.
        save_ztpp018(
          EXPORTING
            is_ztpp018 = ls_ztpp018 ).
        modify_recurso( is_ztpp018 = ls_ztpp018 ).
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD clear_imseg.
    CLEAR: et_imseg.
  ENDMETHOD.


  METHOD conf_apont.
    DATA: l_motivo TYPE zel_id_status.
    get_ordens(
      EXPORTING
        i_arbpl   = is_data-ru_arbpli    " Centro de trabalho
      IMPORTING
        et_ordens = DATA(lt_ordens)    " Tabela Para ZESPP_OP_CT
    ).

    IF lt_ordens IS INITIAL.
      ct_bapiret = VALUE #(
      ( type = 'E' id = 'ZPP' number = '013' message_v1 = is_data-ru_arbpli )
      ).
      EXIT.
    ENDIF.

    IF NOT line_exists( lt_ordens[ matnr = is_data-matnr ] ).
      ct_bapiret = VALUE #(
     ( type = 'E' id = 'ZPP' number = '000' message_v1 = 'Não existe ordem para o material' message_v2 = is_data-matnr )
     ).
      EXIT.
    ENDIF.

    l_motivo = is_data-co_agrnd.
    DATA(ok) = check_motivo( i_motivo = l_motivo ).
    IF ok IS INITIAL.
      EXIT.
    ENDIF.

    save_mov( is_ztpp013 = is_data ).

  ENDMETHOD.


  METHOD conf_apont_adam.
    DATA: ls_ztpp013   TYPE ztpp013,
          l_timediff   TYPE t,
          ls_ztpp018   TYPE ztpp018,
          ls_ztpp013_3 TYPE ztpp013_3.

    CONSTANTS: l_8_minut(02) TYPE n VALUE 8,
               l_4_minut(02) TYPE n VALUE 4.

*    BREAK-POINT.
    SELECT SINGLE * FROM ztpp018_1 INTO @DATA(ls_confct)
      WHERE arbpl = @is_conf_adam-arbpl.
    " busca se a configuração do canal
    SELECT SINGLE * FROM ztpp017 INTO @DATA(ls_confadam)
      WHERE arbpl = @is_conf_adam-arbpl
        AND ch    = @is_conf_adam-ch.

*      Apontamento fundição
    IF ls_confct-arbpl CP 'INJ*'.
      conf_apont_adam_inj(
      EXPORTING
      is_confct    = ls_confct    " Status de Recurso Produção
      is_conf_adam = is_conf_adam
      i_count      = i_count
      ls_confadam  = ls_confadam    " Configurações dos ADAMs
        ).
    ENDIF.
*      Apontamento fundição
    IF ls_confct-arbpl CP 'USIN*'.
      conf_apont_adam_usin(
        EXPORTING
          is_confct    = ls_confct    " Status de Recurso Produção
          is_conf_adam = is_conf_adam
          i_count      = i_count
          ls_confadam  = ls_confadam    " Configurações dos ADAMs
          ).
    ENDIF.

*      Apontamento fundição
    IF ls_confct-arbpl CP 'ESTAN*'.
      conf_apont_adam_estan(
        EXPORTING
          is_confct    = ls_confct    " Status de Recurso Produção
          is_conf_adam = is_conf_adam
          i_count      = i_count
          ls_confadam  = ls_confadam    " Configurações dos ADAMs
          ).
    ENDIF.
*    IF NOT i_count IS INITIAL. " Se existe algum apontamento de contagem
*      IF ls_confct-aufnr IS INITIAL.
*        SELECT SINGLE auto FROM ztpp021 INTO @DATA(l_auto)
*          WHERE id_status = @ls_confct-id_status_01.
*        IF l_auto = abap_true.
*          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
*          SELECT SINGLE * FROM ztpp021 INTO @DATA(ls_status)
*            WHERE id_status = 1.
*          SELECT SINGLE justificativa1, justificativa2, justificativa3
*                FROM ztpp018
*                INTO CORRESPONDING FIELDS OF @ls_ztpp018
*                  WHERE arbpl = @ls_ztpp018-arbpl
*                   AND  ativo = @abap_true.
*          ls_ztpp018-id_status  = ls_ztpp018-id_status_01  = ls_status-id_status.
*          ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
*          ls_confct-op1usin = ls_ztpp018-op1usin =  icon_green_light.
*          ls_ztpp018-id_grupo   = ls_status-id_grupo.
*          CLEAR: ls_ztpp018-justificativa1.
*          IF ls_confct-id_status_01 = ls_confct-id_status_02.
*            ls_ztpp018-id_status_02  = ls_status-id_status.
*            ls_ztpp018-rec_status_02 = ls_status-rec_status.
*            ls_confct-op2usin = ls_ztpp018-op2usin =  icon_green_light.
*            CLEAR: ls_ztpp018-justificativa2.
*          ENDIF.
*          IF ls_confct-id_status_03 = ls_confct-id_status_01.
*            ls_ztpp018-id_status_03  = ls_status-id_status.
*            ls_ztpp018-rec_status_03 = ls_status-rec_status.
*            ls_confct-op3usin = ls_ztpp018-op3usin =  icon_green_light.
*            CLEAR: ls_ztpp018-justificativa3.
*          ENDIF.
*          ls_ztpp018-grupo_st   = ls_status-grupo_st.
**          ls_confct-op1usin = ls_ztpp018-op1usin = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_green_light.
*          calcule_date( i_arbpl = is_conf_adam-arbpl ).
*          "Busca ultima OP
*          SELECT * FROM ztpp018 INTO TABLE @DATA(lt_ordens)
*            WHERE arbpl =  @is_conf_adam-arbpl
*              AND data  >= @datai
*              AND data  <= @dataf
*              AND aufnr <>  @space ORDER BY data DESCENDING, hora DESCENDING.
*          TRY.
*              DATA(ls_ordem)     = lt_ordens[ 1 ].
*              ls_confct-aufnr    = ls_ztpp018-aufnr   = ls_ordem-aufnr.
*              ls_ztpp018-matnr   = ls_ordem-matnr.
*              ls_ztpp018-maktx   = ls_ordem-maktx.
*              ls_ztpp018-corrida = ls_ordem-corrida.
*              ls_confct-equnr    = ls_ztpp018-equnr   = ls_ordem-equnr.
*              ls_ztpp018-eqktx   = ls_ordem-eqktx.
*              save_ztpp018(
*                EXPORTING
*                  is_ztpp018 = ls_ztpp018 ).
*              modify_recurso( is_ztpp018 = ls_ztpp018 ).
*            CATCH cx_sy_itab_line_not_found.
*              ls_ztpp013_3-mandt      = sy-mandt.
*              ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
*              ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
*              ls_ztpp013_3-ch         = is_conf_adam-ch.
*              IF ls_confadam-refugo = abap_true.
*                ls_ztpp013_3-quantidade = i_count * -1.
*              ELSE.
*                ls_ztpp013_3-quantidade = i_count.
*              ENDIF.
*              ls_ztpp013_3-datum      = sy-datum.
*              ls_ztpp013_3-uzeit      = sy-uzeit.
*              ls_ztpp013_3-matnr      = ls_confct-matnr.
*              ls_ztpp013_3-aufnr      = ls_confct-aufnr.
*              IF ls_ztpp013_3-matnr IS INITIAL.
*                ls_ztpp013_3-matnr = ls_confct-op1usin.
*              ENDIF.
*              IF ls_ztpp013_3-ru_arbpli(05) = 'ESTAN'.
*                save_ztpp018(
*                  EXPORTING
*                    is_ztpp018 = ls_ztpp018 ).
*                modify_recurso( is_ztpp018 = ls_ztpp018 ).
*              ENDIF.
*              INSERT ztpp013_3 FROM ls_ztpp013_3.
*              COMMIT WORK AND WAIT.
*              RETURN.
*          ENDTRY.
*        ELSE.
*          ls_ztpp013_3-mandt      = sy-mandt.
*          ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
*          IF ls_confadam-refugo = abap_true.
*            ls_ztpp013_3-quantidade = i_count * -1.
*          ELSE.
*            ls_ztpp013_3-quantidade = i_count.
*          ENDIF.
*          ls_ztpp013_3-datum      = sy-datum.
*          ls_ztpp013_3-uzeit      = sy-uzeit.
*          ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
*          ls_ztpp013_3-ch         = is_conf_adam-ch.
*          ls_ztpp013_3-matnr      = ls_confct-matnr.
*          ls_ztpp013_3-aufnr      = ls_confct-aufnr.
*          IF ls_ztpp013_3-matnr IS INITIAL.
*            ls_ztpp013_3-matnr = ls_confct-op1usin.
*          ENDIF.
*          INSERT ztpp013_3 FROM ls_ztpp013_3.
*          COMMIT WORK AND WAIT.
*          IF ls_confct-op1usin = icon_green_light AND ls_confct-arbpl CP 'ESTAN*'.
*            l_timediff  =   sy-uzeit - ls_confadam-hora.
*            IF l_timediff+2(02) > l_4_minut.
*              SELECT SINGLE * FROM ztpp021 INTO ls_status
*                WHERE id_status = 2.
*              SELECT SINGLE justificativa1, justificativa2, justificativa3
*                FROM ztpp018
*                INTO CORRESPONDING FIELDS OF @ls_ztpp018
*                  WHERE arbpl = @ls_ztpp018-arbpl
*                   AND  ativo = @abap_true.
*              ls_ztpp018-id_status = ls_ztpp018-id_status_01  = ls_status-id_status.
*              ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
*              ls_ztpp018-op1usin       = icon_yellow_light.
*              CLEAR: ls_ztpp018-justificativa1.
*              ls_ztpp018-id_grupo   = ls_status-id_grupo.
*              ls_ztpp018-grupo_st   = ls_status-grupo_st.
**          = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_yellow_light.
*              CLEAR:
*              ls_ztpp018-aufnr,
*              ls_ztpp018-matnr,
*              ls_ztpp018-maktx,
*              ls_ztpp018-corrida,
*              ls_ztpp018-equnr,
*              ls_ztpp018-eqktx.
*              save_ztpp018(
*                EXPORTING
*                  is_ztpp018 = ls_ztpp018 ).
*              modify_recurso( is_ztpp018 = ls_ztpp018 ).
*            ENDIF.
*          ENDIF.
*          RETURN.
*        ENDIF.
*      ENDIF.
*      conf_apont_adam_inj( is_confct = ls_confct is_conf_adam = is_conf_adam i_count = i_count ls_confadam = ls_confadam ).
**      SELECT SINGLE setmat, pulso FROM ztpp021 INTO @DATA(ls_ztpp021)
**          WHERE id_status = @ls_confct-id_status_01.
*      IF ls_confct-op1usin = icon_red_light AND ls_confct-arbpl CP 'INJ*' OR ls_confct-op3usin = icon_red_light AND ls_confct-arbpl CP 'USIN*'.
*        ls_ztpp013_3-mandt      = sy-mandt.
*        ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
*        IF ls_confadam-refugo = abap_true.
*          ls_ztpp013_3-quantidade = i_count * -1.
*        ELSE.
*          ls_ztpp013_3-quantidade = i_count.
*        ENDIF.
*        ls_ztpp013_3-datum      = sy-datum.
*        ls_ztpp013_3-uzeit      = sy-uzeit.
*        ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
*        ls_ztpp013_3-ch         = is_conf_adam-ch.
*        ls_ztpp013_3-matnr      = ls_confct-matnr.
*        ls_ztpp013_3-aufnr      = ls_confct-aufnr.
*        IF ls_ztpp013_3-matnr IS INITIAL.
*          ls_ztpp013_3-matnr = ls_confct-op1usin.
*        ENDIF.
*        INSERT ztpp013_3 FROM ls_ztpp013_3.
*        COMMIT WORK AND WAIT.
*        RETURN.
*      ENDIF.
**      IF ls_confct-op1usin <> icon_green_light.
**        IF ls_ztpp021-setmat = abap_true AND ls_ztpp021-pulso = abap_true.
**          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
**          save_ztpp018(
**                  EXPORTING
**                    is_ztpp018 = ls_ztpp018 ).
**          modify_recurso( is_ztpp018 = ls_ztpp018 ).
**        ELSE.
**          ls_ztpp013_3-mandt      = sy-mandt.
**          ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
**          ls_ztpp013_3-quantidade = i_count.
**          ls_ztpp013_3-datum      = sy-datum.
**          ls_ztpp013_3-uzeit      = sy-uzeit.
**          INSERT ztpp013_3 FROM ls_ztpp013_3.
**          COMMIT WORK AND WAIT.
**          RETURN.
**        ENDIF.
**      ENDIF.
*
*      check_op( EXPORTING arbpl = is_conf_adam-arbpl CHANGING aufnr = ls_confct-aufnr ).
*
*      IF ls_confct-aufnr IS INITIAL.
*        ls_ztpp013_3-mandt      = sy-mandt.
*        ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
*        IF ls_confadam-refugo = abap_true.
*          ls_ztpp013_3-quantidade = i_count * -1.
*        ELSE.
*          ls_ztpp013_3-quantidade = i_count.
*        ENDIF.
*        ls_ztpp013_3-datum      = sy-datum.
*        ls_ztpp013_3-uzeit      = sy-uzeit.
*        ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
*        ls_ztpp013_3-ch         = is_conf_adam-ch.
*        ls_ztpp013_3-matnr      = ls_confct-matnr.
*        ls_ztpp013_3-aufnr      = ls_confct-aufnr.
*        IF ls_ztpp013_3-matnr IS INITIAL.
*          ls_ztpp013_3-matnr = ls_confct-op1usin.
*        ENDIF.
*        INSERT ztpp013_3 FROM ls_ztpp013_3.
*        COMMIT WORK AND WAIT.
*        RETURN.
*      ENDIF.
*      IF ls_confadam-refugo = abap_false.
*        ls_ztpp013-tpmov = 1. " Roda Boa
*        ls_ztpp013-lmnga = i_count.
*      ELSE.
*        ls_ztpp013-tpmov = 3. " Refugo
*        ls_ztpp013-xmnga = i_count.
*        IF ls_confct-op1usin = icon_led_green AND ls_confct-arbpl NP 'USIN*' OR ls_confct-op3usin = icon_led_green AND ls_confct-arbpl CP 'USIN*'..
*          SELECT SINGLE id_status rec_status FROM ztpp023 INTO ( ls_ztpp013-co_agrnd, ls_ztpp013-co_agrnd_desc )
*            WHERE id_status = 061.
*        ELSE.
*          SELECT SINGLE id_status rec_status FROM ztpp023 INTO ( ls_ztpp013-co_agrnd, ls_ztpp013-co_agrnd_desc )
*            WHERE id_status = 136.
*        ENDIF.
*      ENDIF.
*
**      IF ls_confct-aufnr IS INITIAL.
**        IF ls_confct-arbpl IS INITIAL.
**          ls_ztpp013-ru_arbpli = is_conf_adam-arbpl.
**        ELSE.
**          ls_ztpp013-ru_arbpli = ls_confct-arbpl.
**        ENDIF.
**      ELSE.
*      IF ls_confct-op1usin = icon_led_green AND ls_confct-arbpl NP 'USIN*' OR ls_confct-op3usin = icon_led_green AND ls_confct-arbpl CP 'USIN*'.
*        ls_ztpp013-aux02 = 'TRY-OUT'.
*      ENDIF.
*      ls_ztpp013-aufnr = ls_confct-aufnr.
*      ls_ztpp013-ru_arbpli = ls_confct-arbpl.
*      ls_ztpp013-equnr     = ls_confct-equnr.
*      SELECT SINGLE matnr FROM afpo INTO ls_ztpp013-matnr
*        WHERE aufnr = ls_ztpp013-aufnr.
*      SELECT SINGLE lgort FROM afpo INTO ls_ztpp013-lgort
*        WHERE aufnr = ls_confct-aufnr.
*      ls_ztpp013-lgort_dest = ls_ztpp013-lgort.
*      get_number_operation(
*        EXPORTING
*          i_aufnr          = ls_confct-aufnr    " Nº ordem
*          i_arbpli         = ls_confct-arbpl    " Centro de trabalho
*        IMPORTING
*          operation_number = ls_ztpp013-vornr    " Nº operação
*          baixa_mat        = ls_ztpp013-mov_mat
*      ).
**      ENDIF.
**    ENDIF.
*
*
*      save_mov( is_ztpp013 = ls_ztpp013 ).
**      IF ls_confadam-refugo = abap_true.
**        estorna_mov( is_ztpp013 = ls_ztpp013 ).
**      ENDIF.
*    ELSE.
**      DATA: t1 TYPE t, t2 TYPE t.
*      IF ls_confct-op1usin = icon_green_light AND ls_confadam-refugo = abap_false AND ls_confct-arbpl NP 'USIN*' OR ls_confct-op3usin = icon_led_green AND ls_confct-arbpl CP 'USIN*' AND ls_confadam-refugo = abap_false.
*        l_timediff  =   sy-uzeit - ls_confadam-hora.
*        IF ( ls_confadam-arbpl CP 'INJ*' AND l_timediff+2(02) > l_8_minut ) OR ( ( ls_confadam-arbpl CP 'USIN*' OR ls_confadam-arbpl CP 'ESTAN*' ) AND l_timediff+2(02) > l_4_minut ).
*          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
*          SELECT SINGLE * FROM ztpp021 INTO ls_status
*            WHERE id_status = 2.
*          SELECT SINGLE justificativa1, justificativa2, justificativa3
*            FROM ztpp018
*            INTO CORRESPONDING FIELDS OF @ls_ztpp018
*              WHERE arbpl = @ls_ztpp018-arbpl
*               AND  ativo = @abap_true.
*          ls_ztpp018-id_status = ls_ztpp018-id_status_01  = ls_status-id_status.
*          ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
*          ls_ztpp018-op1usin       = icon_yellow_light.
*          CLEAR: ls_ztpp018-justificativa1.
*          IF ls_confct-id_status_02 = 1.
*            ls_ztpp018-id_status_02  = ls_status-id_status.
*            ls_ztpp018-rec_status_02 = ls_status-rec_status.
*            ls_ztpp018-op2usin       = icon_yellow_light.
*            CLEAR: ls_ztpp018-justificativa2.
*          ENDIF.
*          IF ls_confct-id_status_03 = 1.
*            ls_ztpp018-id_status_03  = ls_status-id_status.
*            ls_ztpp018-rec_status_03 = ls_status-rec_status.
*            ls_ztpp018-op3usin       = icon_yellow_light.
*            CLEAR: ls_ztpp018-justificativa3.
*          ENDIF.
*          ls_ztpp018-id_grupo   = ls_status-id_grupo.
*          ls_ztpp018-grupo_st   = ls_status-grupo_st.
**          = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_yellow_light.
*          CLEAR:
*          ls_ztpp018-aufnr,
*          ls_ztpp018-matnr,
*          ls_ztpp018-maktx,
*          ls_ztpp018-corrida,
*          ls_ztpp018-equnr,
*          ls_ztpp018-eqktx.
*          save_ztpp018(
*            EXPORTING
*              is_ztpp018 = ls_ztpp018 ).
*          modify_recurso( is_ztpp018 = ls_ztpp018 ).
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD conf_apont_adam_estan.
    DATA: ls_ztpp013   TYPE ztpp013,
          l_timediff   TYPE t,
          ls_ztpp018   TYPE ztpp018,
          ls_ztpp013_3 TYPE ztpp013_3.

    CONSTANTS: l_4_minut(02) TYPE n VALUE 4.

    DATA(ls_confct) = is_confct.

    IF NOT i_count IS INITIAL. " Se existe algum apontamento de contagem
      IF ls_confct-aufnr IS INITIAL.
        SELECT SINGLE auto FROM ztpp021 INTO @DATA(l_auto)
              WHERE id_status = @ls_confct-id_status_01.
        IF l_auto = abap_true.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021 INTO @DATA(ls_status)
                WHERE id_status = 1.
          SELECT SINGLE justificativa1, justificativa2, justificativa3
          FROM ztpp018
          INTO CORRESPONDING FIELDS OF @ls_ztpp018
          WHERE arbpl = @ls_ztpp018-arbpl
          AND  ativo = @abap_true.
          ls_ztpp018-id_status  = ls_ztpp018-id_status_01  = ls_status-id_status.
          ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
          ls_confct-op1usin = ls_ztpp018-op1usin =  icon_green_light.
          ls_ztpp018-id_grupo   = ls_status-id_grupo.
          CLEAR: ls_ztpp018-justificativa1.
          ls_ztpp018-grupo_st   = ls_status-grupo_st.
*          ls_confct-op1usin = ls_ztpp018-op1usin = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_green_light.
          calcule_date( i_arbpl = is_conf_adam-arbpl ).
          "Busca ultima OP
          SELECT * FROM ztpp018 INTO TABLE @DATA(lt_ordens)
                WHERE arbpl =  @is_conf_adam-arbpl
          AND data  >= @datai
          AND data  <= @dataf
          AND aufnr <>  @space ORDER BY data DESCENDING, hora DESCENDING.
          TRY.
              DATA(ls_ordem)     = lt_ordens[ 1 ].
              ls_confct-aufnr    = ls_ztpp018-aufnr   = ls_ordem-aufnr.
              ls_ztpp018-matnr   = ls_ordem-matnr.
              ls_ztpp018-maktx   = ls_ordem-maktx.
              ls_ztpp018-corrida = ls_ordem-corrida.
              ls_confct-equnr    = ls_ztpp018-equnr   = ls_ordem-equnr.
              ls_ztpp018-eqktx   = ls_ordem-eqktx.
              save_ztpp018(
              EXPORTING
                is_ztpp018 = ls_ztpp018 ).
              modify_recurso( is_ztpp018 = ls_ztpp018 ).
            CATCH cx_sy_itab_line_not_found.
              ls_ztpp013_3-mandt      = sy-mandt.
              ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
              ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
              ls_ztpp013_3-ch         = is_conf_adam-ch.
              IF ls_confadam-refugo = abap_true.
                ls_ztpp013_3-quantidade = i_count * -1.
              ELSE.
                ls_ztpp013_3-quantidade = i_count.
              ENDIF.
              ls_ztpp013_3-datum      = sy-datum.
              ls_ztpp013_3-uzeit      = sy-uzeit.
              ls_ztpp013_3-matnr      = ls_confct-matnr.
              ls_ztpp013_3-aufnr      = ls_confct-aufnr.
              IF ls_ztpp013_3-matnr IS INITIAL.
                ls_ztpp013_3-matnr = ls_confct-op1usin.
              ENDIF.
              save_ztpp018(
              EXPORTING
                is_ztpp018 = ls_ztpp018 ).
              modify_recurso( is_ztpp018 = ls_ztpp018 ).
              INSERT ztpp013_3 FROM ls_ztpp013_3.
              COMMIT WORK AND WAIT.
              RETURN.
          ENDTRY.
        ELSE.
          ls_ztpp013_3-mandt      = sy-mandt.
          ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
          IF ls_confadam-refugo = abap_true.
            ls_ztpp013_3-quantidade = i_count * -1.
          ELSE.
            ls_ztpp013_3-quantidade = i_count.
          ENDIF.
          ls_ztpp013_3-datum      = sy-datum.
          ls_ztpp013_3-uzeit      = sy-uzeit.
          ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
          ls_ztpp013_3-ch         = is_conf_adam-ch.
          ls_ztpp013_3-matnr      = ls_confct-matnr.
          ls_ztpp013_3-aufnr      = ls_confct-aufnr.
          IF ls_ztpp013_3-matnr IS INITIAL.
            ls_ztpp013_3-matnr = ls_confct-op1usin.
          ENDIF.
          INSERT ztpp013_3 FROM ls_ztpp013_3.
          COMMIT WORK AND WAIT.
          IF ls_confct-op1usin = icon_green_light.
            l_timediff  =   sy-uzeit - ls_confadam-hora.
            IF l_timediff+2(02) > l_4_minut.
              SELECT SINGLE * FROM ztpp021 INTO ls_status
              WHERE id_status = 2.
              SELECT SINGLE justificativa1, justificativa2, justificativa3
              FROM ztpp018
              INTO CORRESPONDING FIELDS OF @ls_ztpp018
              WHERE arbpl = @ls_ztpp018-arbpl
              AND  ativo = @abap_true.
              ls_ztpp018-id_status = ls_ztpp018-id_status_01  = ls_status-id_status.
              ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
              ls_ztpp018-op1usin       = icon_yellow_light.
              CLEAR: ls_ztpp018-justificativa1.
              ls_ztpp018-id_grupo   = ls_status-id_grupo.
              ls_ztpp018-grupo_st   = ls_status-grupo_st.
*          = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_yellow_light.
              CLEAR:
              ls_ztpp018-aufnr,
              ls_ztpp018-matnr,
              ls_ztpp018-maktx,
              ls_ztpp018-corrida,
              ls_ztpp018-equnr,
              ls_ztpp018-eqktx.
              save_ztpp018(
              EXPORTING
                is_ztpp018 = ls_ztpp018 ).
              modify_recurso( is_ztpp018 = ls_ztpp018 ).
            ENDIF.
          ENDIF.
          RETURN.
        ENDIF.
      ENDIF.
    ELSE.
*      DATA: t1 TYPE t, t2 TYPE t.
      IF ls_confct-op1usin = icon_green_light AND ls_confadam-refugo = abap_false.
        l_timediff  =   sy-uzeit - ls_confadam-hora.
        IF l_timediff+2(02) > l_4_minut.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021 INTO ls_status
          WHERE id_status = 2.
          SELECT SINGLE justificativa1
          FROM ztpp018
          INTO CORRESPONDING FIELDS OF @ls_ztpp018
          WHERE arbpl = @ls_ztpp018-arbpl
          AND  ativo = @abap_true.
          ls_ztpp018-id_status = ls_ztpp018-id_status_01  = ls_status-id_status.
          ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
          ls_ztpp018-op1usin       = icon_yellow_light.
          CLEAR: ls_ztpp018-justificativa1.
          ls_ztpp018-id_grupo   = ls_status-id_grupo.
          ls_ztpp018-grupo_st   = ls_status-grupo_st.
*          = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_yellow_light.
          CLEAR:
          ls_ztpp018-aufnr,
          ls_ztpp018-matnr,
          ls_ztpp018-maktx,
          ls_ztpp018-corrida,
          ls_ztpp018-equnr,
          ls_ztpp018-eqktx.
          save_ztpp018(
          EXPORTING
            is_ztpp018 = ls_ztpp018 ).
          modify_recurso( is_ztpp018 = ls_ztpp018 ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD conf_apont_adam_inj.
    DATA: ls_ztpp013   TYPE ztpp013,
          l_timediff   TYPE t,
          ls_ztpp018   TYPE ztpp018,
          ls_ztpp013_3 TYPE ztpp013_3,
          ls_confct    TYPE ztpp018_1.
    CONSTANTS: l_8_minut(02) TYPE n VALUE 8.

    ls_confct = is_confct.
    IF NOT i_count IS INITIAL.
      IF ls_confct-aufnr IS INITIAL.
        SELECT SINGLE auto FROM ztpp021 INTO @DATA(l_auto)
              WHERE id_status = @ls_confct-id_status_01.
        IF l_auto = abap_true.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021 INTO @DATA(ls_status)
                WHERE id_status = 1.
          SELECT SINGLE justificativa1
          FROM ztpp018
          INTO CORRESPONDING FIELDS OF @ls_ztpp018
          WHERE arbpl = @ls_ztpp018-arbpl
          AND  ativo = @abap_true.
          ls_ztpp018-id_status  = ls_ztpp018-id_status_01  = ls_status-id_status.
          ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
          ls_confct-op1usin = ls_ztpp018-op1usin =  icon_green_light.
          ls_ztpp018-id_grupo   = ls_status-id_grupo.
          CLEAR: ls_ztpp018-justificativa1.
          ls_ztpp018-grupo_st   = ls_status-grupo_st.
          calcule_date( i_arbpl = is_conf_adam-arbpl ).
          "Busca ultima OP
          SELECT * FROM ztpp018 INTO TABLE @DATA(lt_ordens)
                WHERE arbpl =  @is_conf_adam-arbpl
          AND data  >= @datai
          AND data  <= @dataf
          AND aufnr <>  @space ORDER BY data DESCENDING, hora DESCENDING.
          TRY.
              DATA(ls_ordem)     = lt_ordens[ 1 ].
              ls_confct-aufnr    = ls_ztpp018-aufnr   = ls_ordem-aufnr.
              ls_ztpp018-matnr   = ls_ordem-matnr.
              ls_ztpp018-maktx   = ls_ordem-maktx.
              ls_ztpp018-corrida = ls_ordem-corrida.
              ls_confct-equnr    = ls_ztpp018-equnr   = ls_ordem-equnr.
              ls_ztpp018-eqktx   = ls_ordem-eqktx.
              save_ztpp018(
              EXPORTING
                is_ztpp018 = ls_ztpp018 ).
              modify_recurso( is_ztpp018 = ls_ztpp018 ).
            CATCH cx_sy_itab_line_not_found.
              ls_ztpp013_3-mandt      = sy-mandt.
              ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
              ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
              ls_ztpp013_3-ch         = is_conf_adam-ch.
              IF ls_confadam-refugo = abap_true.
                ls_ztpp013_3-quantidade = i_count * -1.
              ELSE.
                ls_ztpp013_3-quantidade = i_count.
              ENDIF.
              ls_ztpp013_3-datum      = sy-datum.
              ls_ztpp013_3-uzeit      = sy-uzeit.
              ls_ztpp013_3-matnr      = ls_confct-matnr.
              ls_ztpp013_3-aufnr      = ls_confct-aufnr.
              IF ls_ztpp013_3-matnr IS INITIAL.
                ls_ztpp013_3-matnr = ls_confct-op1usin.
              ENDIF.
              INSERT ztpp013_3 FROM ls_ztpp013_3.
              COMMIT WORK AND WAIT.
              RETURN.
          ENDTRY.
        ELSE.
          ls_ztpp013_3-mandt      = sy-mandt.
          ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
          IF ls_confadam-refugo = abap_true.
            ls_ztpp013_3-quantidade = i_count * -1.
          ELSE.
            ls_ztpp013_3-quantidade = i_count.
          ENDIF.
          ls_ztpp013_3-datum      = sy-datum.
          ls_ztpp013_3-uzeit      = sy-uzeit.
          ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
          ls_ztpp013_3-ch         = is_conf_adam-ch.
          ls_ztpp013_3-matnr      = ls_confct-matnr.
          ls_ztpp013_3-aufnr      = ls_confct-aufnr.
          IF ls_ztpp013_3-matnr IS INITIAL.
            ls_ztpp013_3-matnr = ls_confct-op1usin.
          ENDIF.
          INSERT ztpp013_3 FROM ls_ztpp013_3.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
*      SELECT SINGLE setmat, pulso FROM ztpp021 INTO @DATA(ls_ztpp021)
*          WHERE id_status = @ls_confct-id_status_01.
      IF ls_confct-op1usin = icon_red_light.
        ls_ztpp013_3-mandt      = sy-mandt.
        ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
        IF ls_confadam-refugo = abap_true.
          ls_ztpp013_3-quantidade = i_count * -1.
        ELSE.
          ls_ztpp013_3-quantidade = i_count.
        ENDIF.
        ls_ztpp013_3-datum      = sy-datum.
        ls_ztpp013_3-uzeit      = sy-uzeit.
        ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
        ls_ztpp013_3-ch         = is_conf_adam-ch.
        ls_ztpp013_3-matnr      = ls_confct-matnr.
        ls_ztpp013_3-aufnr      = ls_confct-aufnr.
        IF ls_ztpp013_3-matnr IS INITIAL.
          ls_ztpp013_3-matnr = ls_confct-op1usin.
        ENDIF.
        INSERT ztpp013_3 FROM ls_ztpp013_3.
        COMMIT WORK AND WAIT.
        RETURN.
      ENDIF.
*      IF ls_confct-op1usin <> icon_green_light.
*        IF ls_ztpp021-setmat = abap_true AND ls_ztpp021-pulso = abap_true.
*          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
*          save_ztpp018(
*                  EXPORTING
*                    is_ztpp018 = ls_ztpp018 ).
*          modify_recurso( is_ztpp018 = ls_ztpp018 ).
*        ELSE.
*          ls_ztpp013_3-mandt      = sy-mandt.
*          ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
*          ls_ztpp013_3-quantidade = i_count.
*          ls_ztpp013_3-datum      = sy-datum.
*          ls_ztpp013_3-uzeit      = sy-uzeit.
*          INSERT ztpp013_3 FROM ls_ztpp013_3.
*          COMMIT WORK AND WAIT.
*          RETURN.
*        ENDIF.
*      ENDIF.

      check_op( EXPORTING arbpl = is_conf_adam-arbpl CHANGING aufnr = ls_confct-aufnr ).

      IF ls_confct-aufnr IS INITIAL.
        ls_ztpp013_3-mandt      = sy-mandt.
        ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
        IF ls_confadam-refugo = abap_true.
          ls_ztpp013_3-quantidade = i_count * -1.
        ELSE.
          ls_ztpp013_3-quantidade = i_count.
        ENDIF.
        ls_ztpp013_3-datum      = sy-datum.
        ls_ztpp013_3-uzeit      = sy-uzeit.
        ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
        ls_ztpp013_3-ch         = is_conf_adam-ch.
        ls_ztpp013_3-matnr      = ls_confct-matnr.
        ls_ztpp013_3-aufnr      = ls_confct-aufnr.
        IF ls_ztpp013_3-matnr IS INITIAL.
          ls_ztpp013_3-matnr = ls_confct-op1usin.
        ENDIF.
        INSERT ztpp013_3 FROM ls_ztpp013_3.
        COMMIT WORK AND WAIT.
        RETURN.
      ENDIF.
      IF ls_confadam-refugo = abap_false.
        ls_ztpp013-tpmov = 1. " Roda Boa
        ls_ztpp013-lmnga = i_count.
      ELSE.
        ls_ztpp013-tpmov = 3. " Refugo
        ls_ztpp013-xmnga = i_count.
        IF ls_confct-op1usin = icon_led_green.
          SELECT SINGLE id_status rec_status FROM ztpp023 INTO ( ls_ztpp013-co_agrnd, ls_ztpp013-co_agrnd_desc )
          WHERE id_status = 061.
        ELSE.
          SELECT SINGLE id_status rec_status FROM ztpp023 INTO ( ls_ztpp013-co_agrnd, ls_ztpp013-co_agrnd_desc )
          WHERE id_status = 136.
        ENDIF.
      ENDIF.

*      IF ls_confct-aufnr IS INITIAL.
*        IF ls_confct-arbpl IS INITIAL.
*          ls_ztpp013-ru_arbpli = is_conf_adam-arbpl.
*        ELSE.
*          ls_ztpp013-ru_arbpli = ls_confct-arbpl.
*        ENDIF.
*      ELSE.
      IF ls_confct-op1usin = icon_led_green.
        ls_ztpp013-aux02 = 'TRY-OUT'.
      ENDIF.
      ls_ztpp013-aufnr = ls_confct-aufnr.
      ls_ztpp013-ru_arbpli = ls_confct-arbpl.
      ls_ztpp013-equnr     = ls_confct-equnr.
      SELECT SINGLE matnr FROM afpo INTO ls_ztpp013-matnr
      WHERE aufnr = ls_ztpp013-aufnr.
      SELECT SINGLE lgort FROM afpo INTO ls_ztpp013-lgort
      WHERE aufnr = ls_confct-aufnr.
      ls_ztpp013-lgort_dest = ls_ztpp013-lgort.
      get_number_operation(
      EXPORTING
        i_aufnr          = ls_confct-aufnr    " Nº ordem
        i_arbpli         = ls_confct-arbpl    " Centro de trabalho
      IMPORTING
        operation_number = ls_ztpp013-vornr    " Nº operação
        baixa_mat        = ls_ztpp013-mov_mat
        ).
*      ENDIF.
*    ENDIF.


      save_mov( is_ztpp013 = ls_ztpp013 ).
*      IF ls_confadam-refugo = abap_true.
*        estorna_mov( is_ztpp013 = ls_ztpp013 ).
*      ENDIF.
    ELSE.
*      DATA: t1 TYPE t, t2 TYPE t.
      IF ls_confct-op1usin = icon_green_light AND ls_confadam-refugo = abap_false.
        l_timediff  =   sy-uzeit - ls_confadam-hora.
        IF l_timediff+2(02) > l_8_minut.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021 INTO ls_status
          WHERE id_status = 2.
          SELECT SINGLE justificativa1, justificativa2, justificativa3
          FROM ztpp018
          INTO CORRESPONDING FIELDS OF @ls_ztpp018
          WHERE arbpl = @ls_ztpp018-arbpl
          AND  ativo = @abap_true.
          ls_ztpp018-id_status = ls_ztpp018-id_status_01  = ls_status-id_status.
          ls_ztpp018-rec_status = ls_ztpp018-rec_status_01 = ls_status-rec_status.
          ls_ztpp018-op1usin       = icon_yellow_light.
          CLEAR: ls_ztpp018-justificativa1.
          IF ls_confct-id_status_02 = 1.
            ls_ztpp018-id_status_02  = ls_status-id_status.
            ls_ztpp018-rec_status_02 = ls_status-rec_status.
            ls_ztpp018-op2usin       = icon_yellow_light.
            CLEAR: ls_ztpp018-justificativa2.
          ENDIF.
          IF ls_confct-id_status_03 = 1.
            ls_ztpp018-id_status_03  = ls_status-id_status.
            ls_ztpp018-rec_status_03 = ls_status-rec_status.
            ls_ztpp018-op3usin       = icon_yellow_light.
            CLEAR: ls_ztpp018-justificativa3.
          ENDIF.
          ls_ztpp018-id_grupo   = ls_status-id_grupo.
          ls_ztpp018-grupo_st   = ls_status-grupo_st.
*          = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_yellow_light.
          CLEAR:
          ls_ztpp018-aufnr,
          ls_ztpp018-matnr,
          ls_ztpp018-maktx,
          ls_ztpp018-corrida,
          ls_ztpp018-equnr,
          ls_ztpp018-eqktx.
          save_ztpp018(
          EXPORTING
            is_ztpp018 = ls_ztpp018 ).
          modify_recurso( is_ztpp018 = ls_ztpp018 ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD conf_apont_adam_usin.
    DATA: ls_ztpp013      TYPE ztpp013,
          l_timediff      TYPE t,
          ls_ztpp018      TYPE ztpp018,
          ls_ztpp013_3    TYPE ztpp013_3,
          l_time_stop(02) TYPE n.

    CONSTANTS: l_4_minut(02) TYPE n VALUE 3.

    DATA(ls_confct) = is_confct.
    IF NOT i_count IS INITIAL. " Se existe algum apontamento de contagem
      IF ls_confct-aufnr IS INITIAL.
        SELECT SINGLE auto FROM ztpp021 INTO @DATA(l_auto)
              WHERE id_status = @ls_confct-id_status_03.
        IF l_auto = abap_true.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021 INTO @DATA(ls_status)
                WHERE id_status = 1.
          SELECT SINGLE justificativa1, justificativa2, justificativa3
          FROM ztpp018
          INTO CORRESPONDING FIELDS OF @ls_ztpp018
          WHERE arbpl = @ls_ztpp018-arbpl
          AND  ativo = @abap_true.
          ls_ztpp018-id_status  = ls_ztpp018-id_status_03  = ls_status-id_status.
          ls_ztpp018-rec_status = ls_ztpp018-rec_status_03 = ls_status-rec_status.
          ls_confct-op3usin = ls_ztpp018-op3usin =  icon_green_light.
          ls_ztpp018-id_grupo   = ls_status-id_grupo.
          CLEAR: ls_ztpp018-justificativa3.
          IF ls_confct-id_status_03 = ls_confct-id_status_02.
            ls_ztpp018-id_status_02  = ls_status-id_status.
            ls_ztpp018-rec_status_02 = ls_status-rec_status.
            ls_confct-op2usin = ls_ztpp018-op2usin =  icon_green_light.
            CLEAR: ls_ztpp018-justificativa2.
          ENDIF.
          IF ls_confct-id_status_03 = ls_confct-id_status_01.
            ls_ztpp018-id_status_01  = ls_status-id_status.
            ls_ztpp018-rec_status_01 = ls_status-rec_status.
            ls_confct-op1usin = ls_ztpp018-op1usin =  icon_green_light.
            CLEAR: ls_ztpp018-justificativa1.
          ENDIF.
          ls_ztpp018-grupo_st   = ls_status-grupo_st.
*          ls_confct-op1usin = ls_ztpp018-op1usin = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_green_light.
          calcule_date( i_arbpl = is_conf_adam-arbpl ).
          "Busca ultima OP
          SELECT * FROM ztpp018 INTO TABLE @DATA(lt_ordens)
                WHERE arbpl =  @is_conf_adam-arbpl
          AND data  >= @datai
          AND data  <= @dataf
          AND aufnr <>  @space ORDER BY data DESCENDING, hora DESCENDING.
          TRY.
              DATA(ls_ordem)     = lt_ordens[ 1 ].
              ls_confct-aufnr    = ls_ztpp018-aufnr   = ls_ordem-aufnr.
              ls_ztpp018-matnr   = ls_ordem-matnr.
              ls_ztpp018-maktx   = ls_ordem-maktx.
              ls_ztpp018-corrida = ls_ordem-corrida.
              ls_confct-equnr    = ls_ztpp018-equnr   = ls_ordem-equnr.
              ls_ztpp018-eqktx   = ls_ordem-eqktx.
              save_ztpp018(
              EXPORTING
                is_ztpp018 = ls_ztpp018 ).
              modify_recurso( is_ztpp018 = ls_ztpp018 ).
            CATCH cx_sy_itab_line_not_found.
              ls_ztpp013_3-mandt      = sy-mandt.
              ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
              ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
              ls_ztpp013_3-ch         = is_conf_adam-ch.
              IF ls_confadam-refugo = abap_true.
                ls_ztpp013_3-quantidade = i_count * -1.
              ELSE.
                ls_ztpp013_3-quantidade = i_count.
              ENDIF.
              ls_ztpp013_3-datum      = sy-datum.
              ls_ztpp013_3-uzeit      = sy-uzeit.
              ls_ztpp013_3-matnr      = ls_confct-matnr.
              ls_ztpp013_3-aufnr      = ls_confct-aufnr.
              IF ls_ztpp013_3-matnr IS INITIAL.
                ls_ztpp013_3-matnr = ls_confct-op1usin.
              ENDIF.
              INSERT ztpp013_3 FROM ls_ztpp013_3.
              COMMIT WORK AND WAIT.
              RETURN.
          ENDTRY.
        ELSE.
          ls_ztpp013_3-mandt      = sy-mandt.
          ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
          IF ls_confadam-refugo = abap_true.
            ls_ztpp013_3-quantidade = i_count * -1.
          ELSE.
            ls_ztpp013_3-quantidade = i_count.
          ENDIF.
          ls_ztpp013_3-datum      = sy-datum.
          ls_ztpp013_3-uzeit      = sy-uzeit.
          ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
          ls_ztpp013_3-ch         = is_conf_adam-ch.
          ls_ztpp013_3-matnr      = ls_confct-matnr.
          ls_ztpp013_3-aufnr      = ls_confct-aufnr.
          IF ls_ztpp013_3-matnr IS INITIAL.
            ls_ztpp013_3-matnr = ls_confct-op1usin.
          ENDIF.
          INSERT ztpp013_3 FROM ls_ztpp013_3.
          COMMIT WORK AND WAIT.
          RETURN.
        ENDIF.
      ENDIF.
*      SELECT SINGLE setmat, pulso FROM ztpp021 INTO @DATA(ls_ztpp021)
*          WHERE id_status = @ls_confct-id_status_01.
      IF ls_confct-op3usin = icon_red_light.
        ls_ztpp013_3-mandt      = sy-mandt.
        ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
        IF ls_confadam-refugo = abap_true.
          ls_ztpp013_3-quantidade = i_count * -1.
        ELSE.
          ls_ztpp013_3-quantidade = i_count.
        ENDIF.
        ls_ztpp013_3-datum      = sy-datum.
        ls_ztpp013_3-uzeit      = sy-uzeit.
        ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
        ls_ztpp013_3-ch         = is_conf_adam-ch.
        ls_ztpp013_3-matnr      = ls_confct-matnr.
        ls_ztpp013_3-aufnr      = ls_confct-aufnr.
        IF ls_ztpp013_3-matnr IS INITIAL.
          ls_ztpp013_3-matnr = ls_confct-op1usin.
        ENDIF.
        INSERT ztpp013_3 FROM ls_ztpp013_3.
        COMMIT WORK AND WAIT.
        RETURN.
      ENDIF.
*      IF ls_confct-op1usin <> icon_green_light.
*        IF ls_ztpp021-setmat = abap_true AND ls_ztpp021-pulso = abap_true.
*          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
*          save_ztpp018(
*                  EXPORTING
*                    is_ztpp018 = ls_ztpp018 ).
*          modify_recurso( is_ztpp018 = ls_ztpp018 ).
*        ELSE.
*          ls_ztpp013_3-mandt      = sy-mandt.
*          ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
*          ls_ztpp013_3-quantidade = i_count.
*          ls_ztpp013_3-datum      = sy-datum.
*          ls_ztpp013_3-uzeit      = sy-uzeit.
*          INSERT ztpp013_3 FROM ls_ztpp013_3.
*          COMMIT WORK AND WAIT.
*          RETURN.
*        ENDIF.
*      ENDIF.

      check_op( EXPORTING arbpl = is_conf_adam-arbpl CHANGING aufnr = ls_confct-aufnr ).

      IF ls_confct-aufnr IS INITIAL.
        ls_ztpp013_3-mandt      = sy-mandt.
        ls_ztpp013_3-ru_arbpli  = is_conf_adam-arbpl.
        IF ls_confadam-refugo = abap_true.
          ls_ztpp013_3-quantidade = i_count * -1.
        ELSE.
          ls_ztpp013_3-quantidade = i_count.
        ENDIF.
        ls_ztpp013_3-datum      = sy-datum.
        ls_ztpp013_3-uzeit      = sy-uzeit.
        ls_ztpp013_3-ip_adam    = is_conf_adam-ip_adam.
        ls_ztpp013_3-ch         = is_conf_adam-ch.
        ls_ztpp013_3-matnr      = ls_confct-matnr.
        ls_ztpp013_3-aufnr      = ls_confct-aufnr.
        IF ls_ztpp013_3-matnr IS INITIAL.
          ls_ztpp013_3-matnr = ls_confct-op1usin.
        ENDIF.
        INSERT ztpp013_3 FROM ls_ztpp013_3.
        COMMIT WORK AND WAIT.
        RETURN.
      ENDIF.
      IF ls_confadam-refugo = abap_false.
        ls_ztpp013-tpmov = 1. " Roda Boa
        ls_ztpp013-lmnga = i_count.
      ELSE.
        ls_ztpp013-tpmov = 3. " Refugo
        ls_ztpp013-xmnga = i_count.
        IF ls_confct-op3usin = icon_led_green.
          SELECT SINGLE id_status rec_status FROM ztpp023 INTO ( ls_ztpp013-co_agrnd, ls_ztpp013-co_agrnd_desc )
          WHERE id_status = 061.
        ELSE.
          SELECT SINGLE id_status rec_status FROM ztpp023 INTO ( ls_ztpp013-co_agrnd, ls_ztpp013-co_agrnd_desc )
          WHERE id_status = 136.
        ENDIF.
      ENDIF.

      IF ls_confct-op3usin = icon_led_green.
        ls_ztpp013-aux02 = 'TRY-OUT'.
      ENDIF.
      ls_ztpp013-aufnr = ls_confct-aufnr.
      ls_ztpp013-ru_arbpli = ls_confct-arbpl.
      ls_ztpp013-equnr     = ls_confct-equnr.
      SELECT SINGLE matnr FROM afpo INTO ls_ztpp013-matnr
      WHERE aufnr = ls_ztpp013-aufnr.
      SELECT SINGLE lgort FROM afpo INTO ls_ztpp013-lgort
      WHERE aufnr = ls_confct-aufnr.
      ls_ztpp013-lgort_dest = ls_ztpp013-lgort.
      get_number_operation(
      EXPORTING
        i_aufnr          = ls_confct-aufnr    " Nº ordem
        i_arbpli         = ls_confct-arbpl    " Centro de trabalho
      IMPORTING
        operation_number = ls_ztpp013-vornr    " Nº operação
        baixa_mat        = ls_ztpp013-mov_mat
        ).
*      ENDIF.
*    ENDIF.


      save_mov( is_ztpp013 = ls_ztpp013 ).
*      IF ls_confadam-refugo = abap_true.
*        estorna_mov( is_ztpp013 = ls_ztpp013 ).
*      ENDIF.
    ELSE.
*      DATA: t1 TYPE t, t2 TYPE t.
      IF ls_confct-op3usin = icon_green_light AND ls_confadam-refugo = abap_false.
        l_timediff  =   sy-uzeit - ls_confadam-hora.
        SELECT SINGLE low FROM tvarvc INTO @DATA(l_time) WHERE name = 'ZPP_PARADAUSIN'.
        IF sy-subrc <> 0.
          l_time_stop = l_4_minut.
        ELSE.
          IF NOT l_time IS INITIAL.
            l_time_stop = l_time.
          ELSE.
            l_time_stop = l_4_minut.
          ENDIF.
        ENDIF.
        IF  l_timediff+2(02) > l_time_stop.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021 INTO ls_status
          WHERE id_status = 2.
          SELECT SINGLE justificativa1, justificativa2, justificativa3
          FROM ztpp018
          INTO CORRESPONDING FIELDS OF @ls_ztpp018
          WHERE arbpl = @ls_ztpp018-arbpl
          AND  ativo = @abap_true.
          ls_ztpp018-id_status = ls_ztpp018-id_status_03  = ls_status-id_status.
          ls_ztpp018-rec_status = ls_ztpp018-rec_status_03 = ls_status-rec_status.
          ls_ztpp018-op3usin       = icon_yellow_light.
          CLEAR: ls_ztpp018-justificativa3.
          IF ls_confct-id_status_02 = 1.
            ls_ztpp018-id_status_02  = ls_status-id_status.
            ls_ztpp018-rec_status_02 = ls_status-rec_status.
            ls_ztpp018-op2usin       = icon_yellow_light.
            CLEAR: ls_ztpp018-justificativa2.
          ENDIF.
          IF ls_confct-id_status_01 = 1.
            ls_ztpp018-id_status_01  = ls_status-id_status.
            ls_ztpp018-rec_status_01 = ls_status-rec_status.
            ls_ztpp018-op1usin       = icon_yellow_light.
            CLEAR: ls_ztpp018-justificativa1.
          ENDIF.
          ls_ztpp018-id_grupo   = ls_status-id_grupo.
          ls_ztpp018-grupo_st   = ls_status-grupo_st.
*          = ls_ztpp018-op2usin = ls_ztpp018-op3usin = icon_yellow_light.
          CLEAR:
          ls_ztpp018-aufnr,
          ls_ztpp018-matnr,
          ls_ztpp018-maktx,
          ls_ztpp018-corrida,
          ls_ztpp018-equnr,
          ls_ztpp018-eqktx.
          save_ztpp018(
          EXPORTING
            is_ztpp018 = ls_ztpp018 ).
          modify_recurso( is_ztpp018 = ls_ztpp018 ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD conf_app_1_insp.
    DATA: ls_data     TYPE ztpp013,
          l_message   TYPE text255,
          l_num03(03) TYPE n.

    ls_data-ru_arbpli = it_parameter[ name = 'Recurso' ]-value.
    ls_data-matnr     = it_parameter[ name = 'Matnr' ]-value.

    CASE ls_data-ru_arbpli.
      WHEN 'PINT_001'.
        get_ordens(
                  EXPORTING
                    i_arbpl   = 'PREP_001'   " Centro de trabalho
                  IMPORTING
                    et_ordens = DATA(lt_ordens)     " Tabela Para ZESPP_OP_CT
                ).
      WHEN 'PINT_003'.
        get_ordens(
                  EXPORTING
                    i_arbpl   = 'DIAM_001'   " Centro de trabalho
                  IMPORTING
                    et_ordens = lt_ordens     " Tabela Para ZESPP_OP_CT
                ).
      WHEN OTHERS.
        get_ordens(
          EXPORTING
            i_arbpl   = ls_data-ru_arbpli   " Centro de trabalho
          IMPORTING
            et_ordens = lt_ordens     " Tabela Para ZESPP_OP_CT
        ).
    ENDCASE.
    IF lt_ordens IS  INITIAL.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = 'ZPP'
          lang      = sy-langu
          no        = '013'
          v1        = ls_data-ru_arbpli
        IMPORTING
          msg       = l_message
        EXCEPTIONS
          not_found = 1.
      ct_bapiret = VALUE #(
      ( message = l_message )
      ).
      EXIT.
    ENDIF.

    IF NOT line_exists( lt_ordens[ matnr = ls_data-matnr ] ).
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = 'ZPP'
          lang      = sy-langu
          no        = '015'
          v1        = ls_data-matnr
        IMPORTING
          msg       = l_message
        EXCEPTIONS
          not_found = 1.
      ct_bapiret = VALUE #(
      ( message = l_message )
      ).
      EXIT.
    ENDIF.

    ls_data-aufnr = lt_ordens[ matnr = ls_data-matnr ]-aufnr.

    ls_data-lgort = '1415'.

    CASE it_parameter[ name = 'Decisao' ]-value.
      WHEN 'RETRABALHO'.
        ls_data-lgort_dest = '1417'.
      WHEN 'DESPLACAMENTO'.
        ls_data-lgort_dest = '1419'.
    ENDCASE.


    l_num03 = it_parameter[ name = 'IdStatus' ]-value.
    ls_data-co_agrnd   = l_num03."it_parameter[ name = 'IdStatus' ]-value.
    SELECT SINGLE rec_status FROM ztpp023 INTO ls_data-co_agrnd_desc
       WHERE id_status = ls_data-co_agrnd.
    IF sy-subrc <> 0.
      ct_bapiret = VALUE #(
    ( message = 'Motivo' && | | && ls_data-co_agrnd && | | && 'não cadastrado'  )
    ).
      EXIT.
    ENDIF.

    ls_data-tpmov      = '2'.
    get_number_operation(
        EXPORTING
          i_aufnr   = ls_data-aufnr
          i_arbpli  = ls_data-ru_arbpli
*          i_checkap = abap_true
          IMPORTING
            operation_number = ls_data-vornr
            baixa_mat        = ls_data-mov_mat
            apont_ok         = DATA(ok) ).

*    IF ok IS INITIAL.
*      ct_bapiret = VALUE #(
*      ( message = 'Falta apontamento na etapa anterior'  )
*       ).
*      EXIT.
*    ENDIF.

    ls_data-mov_mat = abap_true.
    ls_data-rmnga   = 1.
    IF ls_data-ru_arbpli = 'PINT_001'.
      ls_data-matnr     = ls_data-matnr(8) && 'PREP'.
    ENDIF.
    get_turno(
    EXPORTING
      i_arbpl   = ls_data-ru_arbpli    " Centro de trabalho
    IMPORTING
      e_budat   =  ls_data-data_m    " Data de lançamento
      e_schprog =  ls_data-schprog    " Chave da seqüência de turnos
        ).
    estorno_retrabalho( is_ztpp013 = ls_data ).
    save_mov( is_ztpp013 = ls_data ).

  ENDMETHOD.


  METHOD conf_app_mat.
    DATA: l_matnrold TYPE matnr,
          l_matnrnew TYPE matnr,
          ls_save    TYPE ztpp026.

    l_matnrold = it_parameter[ name = 'MatOld' ]-value.
    l_matnrnew = it_parameter[ name = 'MatNew' ]-value.
    SELECT SINGLE * FROM ztpp026 INTO @DATA(ls_check) WHERE matnr = @l_matnrnew.
    IF sy-subrc = 0.
      ct_bapiret = VALUE #(
      ( message = 'Material' && | | && l_matnrnew && | | && ' já contido na lista' )
      ).
      RETURN.
    ENDIF.
    DELETE FROM ztpp026 WHERE matnr = @l_matnrold.
    IF sy-subrc <> 0.
      ct_bapiret = VALUE #(
      ( message = 'Material' && | | && l_matnrold && | | && ' já excluído ou não faz parte da Lista' )
      ).
      RETURN.
    ENDIF.
    ls_save-mandt = sy-mandt.
    ls_save-uname = sy-uname.
    ls_save-data  = sy-datum.
    ls_save-uzeit = sy-uzeit.
    ls_save-matnr = l_matnrnew.
    INSERT ztpp026 FROM ls_save.
    IF sy-subrc <> 0.
      ct_bapiret = VALUE #(
      ( message = 'Erro ao incluir o Material' && | | && l_matnrnew )
      ).
    ENDIF.
  ENDMETHOD.


  METHOD conf_etq.

    UPDATE ztpp029 SET data_l = sy-datum
                       hora_l = sy-uzeit
                       user_l = sy-uname
                       lido   = abap_true
                   WHERE idetiqueta = i_etq.
  ENDMETHOD.


  METHOD create_andom_qualidade.
    DATA: l_week    TYPE scal-week,
          l_segunda TYPE scal-date,
          l_terca   TYPE scal-date,
          l_quarta  TYPE scal-date,
          l_quinta  TYPE scal-date,
          l_sexta   TYPE scal-date,
          l_sabado  TYPE scal-date,
          l_add     TYPE i.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = sy-datum
      IMPORTING
        week         = l_week
      EXCEPTIONS
        date_invalid = 1.

    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
      EXPORTING
        week         = l_week
      IMPORTING
        date         = l_segunda
      EXCEPTIONS
        week_invalid = 1
        OTHERS       = 2.

    DO 5 TIMES.
      ADD 1 TO l_add.
      CASE l_add.
        WHEN 1.
          l_terca = l_segunda.
          ADD l_add TO l_terca.
        WHEN 2.
          l_quarta = l_segunda.
          ADD l_add TO l_quarta.
        WHEN 3.
          l_quinta = l_segunda.
          ADD l_add TO l_quinta.
        WHEN 4.
          l_sexta = l_segunda.
          ADD l_add TO l_sexta.
        WHEN 5.
          l_sabado = l_segunda.
          ADD l_add TO l_sabado.
      ENDCASE.
    ENDDO.

    SELECT * FROM ztpp018 INTO TABLE @DATA(lt_status)
      WHERE data <= @l_sabado
        AND data >= @l_segunda.


  ENDMETHOD.


  METHOD create_andom_statusrec.
    DATA: lt_tab              TYPE TABLE OF dd07v,
          lv_fullpath         TYPE string,
          ev_document_rawdata TYPE mime_data,
          lt_txt_conv         TYPE  truxs_t_text_data,
          lo_exception        TYPE REF TO cx_root,
          lo_mes_rodas        TYPE REF TO zcl_pp_mes_rodas.

    CONSTANTS: lc_domain TYPE dd07l-domname VALUE 'ZDTPMOV'.
    DATA: lt_data  TYPE TABLE OF zes_andom_usin_ct_status,
          lt_data1 TYPE TABLE OF zes_andom_fund_ct_status.
    SELECT * FROM ztpp018_1 INTO CORRESPONDING FIELDS OF TABLE lt_data WHERE arbpl LIKE 'USIN%'.
    IF sy-subrc <> 0.
      WRITE 'Não existem dados para seleção USINAGEM'.
      RETURN.
    ENDIF.


    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).

      get_turno(
       EXPORTING
         i_arbpl   =  <data>-arbpl    " Centro de trabalho
       IMPORTING
         e_budat   = DATA(e_budat)    " Data de lançamento
*           e_schprog = e_schprog    " Chave da seqüência de turnos
     ).
      SELECT schprog, SUM( lmnga ) AS lmnga,estorno, ndoc_estorn FROM ztpp013
        INTO TABLE @DATA(lt_prodturno)
        WHERE ru_arbpli   = @<data>-arbpl
*          AND estorno     = @abap_false
*          AND ndoc_estorn = @space
*         " AND aufnr     = ls_recurso-aufnr
          AND data_m     = @e_budat GROUP BY schprog,estorno,ndoc_estorn.

      SORT lt_prodturno BY estorno.
      DELETE lt_prodturno WHERE estorno <> space.
      SORT lt_prodturno BY ndoc_estorn.
      DELETE lt_prodturno WHERE ndoc_estorn <> space.
      SORT lt_prodturno BY schprog.
      TRY.
          <data>-t1 = lt_prodturno[ schprog = '1' ]-lmnga.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY.
          <data>-t2 = lt_prodturno[ schprog = '2' ]-lmnga.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      TRY.
          <data>-t3 = lt_prodturno[ schprog = '3' ]-lmnga.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      <data>-total_t = <data>-t1 + <data>-t2 + <data>-t3.

      CLEAR: lt_prodturno.
    ENDLOOP.
*    CALL FUNCTION 'GET_DOMAIN_VALUES'
*      EXPORTING
*        domname    = lc_domain    " Name of domain
*      TABLES
*        values_tab = lt_tab.    " Value table
*    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).
*      SELECT SINGLE maktx FROM makt INTO <data>-maktx WHERE matnr = <data>-matnr AND spras = sy-langu.
*      <data>-desc_tp = lt_tab[ domvalue_l = <data>-tpmov ]-ddtext.
*      IF <data>-matnr CP '*USIN'.
*        <data>-fase = 'USINAGEM'.
*      ELSE.
*        IF <data>-matnr+7(01) = 'D'.
*          <data>-fase = 'DIAMANTADA'.
*        ELSE.
*          <data>-fase = 'PINTADA'.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

    SELECT SINGLE low FROM tvarvc INTO lv_fullpath WHERE name = 'ZFILE_ANDOMROD'.
    IF sy-subrc <> 0.
      WRITE: 'Falta Caminho para salvar arquivo. Entre em contato com a T.I'.
      EXIT.
    ENDIF.

    lv_fullpath = lv_fullpath && 'STATUSREC/ANDOMUSIN.TXT'.
    TRANSLATE: lv_fullpath TO LOWER CASE.

    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
      TABLES
        i_tab_sap_data       = lt_data
      CHANGING
        i_tab_converted_data = lt_txt_conv.

    TRY.

        OPEN DATASET lv_fullpath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        LOOP AT lt_txt_conv ASSIGNING  FIELD-SYMBOL(<fs_dados>).

          TRANSFER <fs_dados> TO lv_fullpath.
        ENDLOOP.

        CLOSE DATASET lv_fullpath.

        WRITE 'Dados foram atualizados USINAGEM'.
      CATCH cx_root INTO lo_exception.
*
        DATA(lv_error) = lo_exception->get_text( ).
        WRITE lv_error.
*      MESSAGE lv_error TYPE 'I'.

    ENDTRY.


    " FUNDIÇÃO

    CLEAR: lt_data1,lt_prodturno.
    SELECT * FROM ztpp018_1 INTO CORRESPONDING FIELDS OF TABLE lt_data1 WHERE arbpl LIKE 'INJ%'.
    IF sy-subrc <> 0.
      WRITE 'Não existem dados para seleção FUNDIÇÃO'.
      RETURN.
    ENDIF.
    LOOP AT lt_data1 ASSIGNING FIELD-SYMBOL(<data1>).

      get_turno(
       EXPORTING
         i_arbpl   =  <data1>-arbpl    " Centro de trabalho
       IMPORTING
         e_budat   = e_budat    " Data de lançamento
*           e_schprog = e_schprog    " Chave da seqüência de turnos
     ).
      SELECT schprog, SUM( lmnga ) AS lmnga, estorno, ndoc_estorn FROM ztpp013
        INTO CORRESPONDING FIELDS OF TABLE @lt_prodturno
        WHERE ru_arbpli   = @<data1>-arbpl
          "AND estorno     = @abap_false
          "AND ndoc_estorn = @space
         " AND aufnr     = ls_recurso-aufnr
          AND data_m     = @e_budat GROUP BY schprog,estorno,ndoc_estorn.

      SORT lt_prodturno BY estorno.
      DELETE lt_prodturno WHERE estorno <> space.
      SORT lt_prodturno BY ndoc_estorn.
      DELETE lt_prodturno WHERE ndoc_estorn <> space.
      SORT lt_prodturno BY schprog.
      TRY.
          <data1>-t1 = lt_prodturno[ schprog = '1' ]-lmnga.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      TRY.
          <data1>-t2 = lt_prodturno[ schprog = '2' ]-lmnga.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      TRY.
          <data1>-t3 = lt_prodturno[ schprog = '3' ]-lmnga.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      <data1>-total_t = <data>-t1 + <data>-t2 + <data>-t3.
      CLEAR: lt_prodturno.
    ENDLOOP.
*    CALL FUNCTION 'GET_DOMAIN_VALUES'
*      EXPORTING
*        domname    = lc_domain    " Name of domain
*      TABLES
*        values_tab = lt_tab.    " Value table
*    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).
*      SELECT SINGLE maktx FROM makt INTO <data>-maktx WHERE matnr = <data>-matnr AND spras = sy-langu.
*      <data>-desc_tp = lt_tab[ domvalue_l = <data>-tpmov ]-ddtext.
*      IF <data>-matnr CP '*USIN'.
*        <data>-fase = 'USINAGEM'.
*      ELSE.
*        IF <data>-matnr+7(01) = 'D'.
*          <data>-fase = 'DIAMANTADA'.
*        ELSE.
*          <data>-fase = 'PINTADA'.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

    SELECT SINGLE low FROM tvarvc INTO lv_fullpath WHERE name = 'ZFILE_ANDOMROD'.
    IF sy-subrc <> 0.
      WRITE: 'Falta Caminho para salvar arquivo. Entre em contato com a T.I'.
      EXIT.
    ENDIF.

    lv_fullpath = lv_fullpath && 'STATUSREC/ANDOMFUND.TXT'.
    TRANSLATE: lv_fullpath TO LOWER CASE.
    CLEAR: lt_txt_conv.
    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
      TABLES
        i_tab_sap_data       = lt_data1
      CHANGING
        i_tab_converted_data = lt_txt_conv.

    TRY.

        OPEN DATASET lv_fullpath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        LOOP AT lt_txt_conv ASSIGNING  <fs_dados>.

          TRANSFER <fs_dados> TO lv_fullpath.
        ENDLOOP.

        CLOSE DATASET lv_fullpath.

        WRITE 'Dados foram atualizados FUNDIÇÃO'.
      CATCH cx_root INTO lo_exception.
*
        lv_error = lo_exception->get_text( ).
        WRITE lv_error.
*      MESSAGE lv_error TYPE 'I'.

    ENDTRY.




  ENDMETHOD.


  METHOD create_excel_andom_day.
    DATA: lt_tab              TYPE TABLE OF dd07v,
          lv_fullpath         TYPE string,
          lt_data             TYPE TABLE OF zes_andom_day,
          ev_document_rawdata TYPE mime_data,
          lt_txt_conv         TYPE  truxs_t_text_data,
          lo_exception        TYPE REF TO cx_root.

    CONSTANTS: lc_domain TYPE dd07l-domname VALUE 'ZDTPMOV'.
    SELECT * FROM ztpp013 INTO CORRESPONDING FIELDS OF TABLE @lt_data
      WHERE data_m     = @sy-datum.
    " AND estorno    = @space
    " AND ndoc_estorn = @space.
    IF sy-subrc <> 0.
      WRITE 'Não existem dados para seleção'.
      RETURN.
    ENDIF.
    SORT lt_data BY estorno.
    DELETE lt_data WHERE estorno <> space.
    SORT lt_data BY ndoc_estorn.
    DELETE lt_data WHERE ndoc_estorn <> space.
    SORT lt_data BY datum hora.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = lc_domain    " Name of domain
      TABLES
        values_tab = lt_tab.    " Value table
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).
      SELECT SINGLE maktx FROM makt INTO <data>-maktx WHERE matnr = <data>-matnr AND spras = sy-langu.
      <data>-desc_tp = lt_tab[ domvalue_l = <data>-tpmov ]-ddtext.
      IF <data>-matnr CP '*USIN'.
        <data>-fase = 'USINAGEM'.
      ELSEIF <data>-matnr CP '*INJE'.
        <data>-fase = 'INJETADA'.
      ELSEIF <data>-matnr CP '*BRUT'.
        <data>-fase = 'BRUTA'.
      ELSEIF <data>-matnr CP '*TRAT'.
        <data>-fase = 'TRATADA'.
      ELSE.
        IF <data>-matnr+7(01) = 'D'.
          <data>-fase = 'DIAMANTADA'.
        ELSE.
          <data>-fase = 'PINTADA'.
        ENDIF.
      ENDIF.
      <data>-aro  = <data>-matnr+2(02).
      <data>-hora = <data>-uzeit(02).

      CASE <data>-aro.
        WHEN '14'.
          <data>-prod_max_hora = '104'.
          <data>-maxporhora    = '13'.
        WHEN '15'.
          <data>-prod_max_hora = '96'.
          <data>-maxporhora    = '12'.
        WHEN '16'.
          <data>-prod_max_hora = '88'.
          <data>-maxporhora    = '11'.
        WHEN '17'.
          <data>-prod_max_hora = '80'.
          <data>-maxporhora    = '10'.
        WHEN '18'.
          <data>-prod_max_hora = '72'.
          <data>-maxporhora    = '9'.
      ENDCASE.
    ENDLOOP.

    SELECT SINGLE low FROM tvarvc INTO lv_fullpath WHERE name = 'ZFILE_ANDOMROD'.
    IF sy-subrc <> 0.
      WRITE: 'Falta Caminho para salvar arquivo. Entre em contato com a T.I'.
      EXIT.
    ENDIF.

    lv_fullpath = lv_fullpath && 'ZPP050/ANDOM.TXT'.
    TRANSLATE: lv_fullpath TO LOWER CASE.

    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
      TABLES
        i_tab_sap_data       = lt_data
      CHANGING
        i_tab_converted_data = lt_txt_conv.

    TRY.

        OPEN DATASET lv_fullpath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        LOOP AT lt_txt_conv ASSIGNING  FIELD-SYMBOL(<fs_dados>).

          TRANSFER <fs_dados> TO lv_fullpath.
        ENDLOOP.

        CLOSE DATASET lv_fullpath.

        WRITE 'Dados foram atualizados'.
      CATCH cx_root INTO lo_exception.
*
        DATA(lv_error) = lo_exception->get_text( ).
        WRITE lv_error.
*      MESSAGE lv_error TYPE 'I'.

    ENDTRY.




  ENDMETHOD.


  METHOD create_order.
    DATA: ls_order  TYPE bapi_pp_order_create,
          ls_return TYPE bapiret2,
          lt_orders TYPE STANDARD TABLE OF bapi_order_key,
          lt_return TYPE TABLE OF bapi_order_return.

    FIELD-SYMBOLS: <t350> TYPE t350.

    ls_order-material = matnr.
    ls_order-basic_start_date = data.
    ls_order-plant = '1015'.
    ls_order-quantity = menge.
    ls_order-order_type ='YBM4'.

    CALL FUNCTION 'BAPI_PRODORD_CREATE'
      EXPORTING
        orderdata    = ls_order
      IMPORTING
        return       = ls_return
        order_number = aufnr
*       ORDER_TYPE   =
      .

    IF NOT aufnr IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      APPEND aufnr TO lt_orders.
      EXPORT no_avail_check_imp FROM abap_true TO MEMORY ID 'LIB_ORDER_PRD'.
      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        CALL FUNCTION 'BAPI_PRODORD_RELEASE'
          TABLES
            orders        = lt_orders
            detail_return = lt_return.

        IF line_exists( lt_return[ id = 'CO' number = '093' ] ).
          DATA(l_ok) = abap_true.
          EXIT.
        ENDIF.
      ENDDO.
      IF l_ok IS INITIAL.
        CLEAR: aufnr.
      ENDIF.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD estorna_mov.
    DATA: ls_ztpp013 TYPE ztpp013,
          ls_log     TYPE ztpp013_3,
          l_quant    TYPE ztpp013-lmnga,
          ls_error   TYPE ztpp013_3,
          datai      TYPE datum.
    l_quant = is_ztpp013-xmnga.
    datai = is_ztpp013-data_m(06) && '01'.
    SELECT * FROM ztpp013 INTO TABLE @DATA(lt_estorno) "UP TO 100 ROWS
      WHERE data_m >= @datai
        AND data_m <= @is_ztpp013-data_m
        AND matnr   = @is_ztpp013-matnr
   "     AND estorno = @space
        AND tpmov   = 1
        AND equnr   = @is_ztpp013-equnr.
    "AND ndoc_estorn = @space ORDER BY data_m DESCENDING, datum DESCENDING , uzeit DESCENDING.
*    SORT lt_estorno BY data_m DESCENDING datum DESCENDING uzeit DESCENDING.
    SORT lt_estorno BY estorno.
    DELETE lt_estorno WHERE estorno <> space.
    SORT lt_estorno BY ndoc_estorn.
    DELETE lt_estorno WHERE ndoc_estorn <> space.
    SORT lt_estorno BY data_m DESCENDING datum DESCENDING uzeit DESCENDING.

    IF sy-subrc <> 0.
      ls_log-datum = sy-datum.
      ls_log-uzeit = sy-uzeit.
      ls_log-ru_arbpli  = is_ztpp013-ru_arbpli.
      ls_log-aufnr      = is_ztpp013-aufnr.
      ls_log-matnr      = is_ztpp013-matnr.
      ls_log-quantidade = is_ztpp013-xmnga * -1.
      ls_log-ip_adam    = is_ztpp013-zidmovpcf.
      INSERT ztpp013_3 FROM ls_log.
      EXIT.
    ENDIF.
    LOOP AT lt_estorno INTO DATA(ls_estorno).
      CLEAR: ls_ztpp013.

      MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
      ls_ztpp013-estorno = abap_true.
      DATA(l_num) = save_mov_man( is_ztpp013 = ls_ztpp013 ).
      TRY.
*          UPDATE ztpp013 SET ndoc_estorn = l_num
*              WHERE zidmovpcf   = ls_estorno-zidmovpcf
*                AND gjahr       = ls_estorno-gjahr.
          upd_ztpp013( datum = ls_ztpp013-datum zidmovpcf = ls_ztpp013-zidmovpcf gjahr = ls_ztpp013-gjahr ndoc_estorn = l_num ).
        CATCH cx_sy_open_sql_db INTO DATA(lf_err).
          DATA(lf_message) = lf_err->get_text( ).
          CLEAR: ls_error.
          ls_error-datum   = sy-datum.
          ls_error-uzeit   = sy-uzeit.
          ls_error-ip_adam = ls_estorno-zidmovpcf.
          ls_error-matnr   = lf_message.
          INSERT ztpp013_3 FROM ls_error.
      ENDTRY.

      l_quant = l_quant - ls_estorno-lmnga.
      IF l_quant = 0.
        EXIT.
      ENDIF.
      IF l_quant < 0.
        MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
        ls_ztpp013-lmnga = l_quant * -1.
        save_mov_man( is_ztpp013 =  ls_ztpp013 ).
*        CLEAR: ls_ztpp013.
*        MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
*        ls_ztpp013-estorno = abap_true.
*        l_num = save_mov_man( is_ztpp013 = ls_ztpp013 ).
*        UPDATE ztpp013 SET ndoc_estorn = l_num
*            WHERE zidmovpcf   = ls_estorno-zidmovpcf
*              AND gjahr       = ls_estorno-gjahr.
        EXIT.
      ENDIF.
    ENDLOOP.

*    ENDSELECT.

  ENDMETHOD.


  METHOD estorna_mov_man.
    DATA: ls_ztpp013 TYPE ztpp013,
          ls_log     TYPE ztpp013_3,
          l_quant    TYPE ztpp013-lmnga,
          l_data     TYPE datum,
          datai      TYPE datum.
    l_quant = is_ztpp013-xmnga.
    datai = is_ztpp013-data_m(06) && '01'.
    SELECT * FROM ztpp013 INTO TABLE @DATA(lt_estorno) UP TO 100 ROWS
      WHERE data_m >= @datai
        AND data_m <= @is_ztpp013-data_m
        AND matnr   = @is_ztpp013-matnr
        AND estorno = @space
        AND tpmov   = 1
        AND schprog <= @is_ztpp013-schprog
        AND equnr   = @is_ztpp013-equnr
        AND ndoc_estorn = @space ORDER BY data_m DESCENDING, datum DESCENDING , uzeit DESCENDING.

*    SORT lt_estorno BY data_m DESCENDING datum DESCENDING uzeit DESCENDING.
*    DATA(lines) = lines( lt_estorno ).
*    IF lines <> 100.
*      lines = 100 - lines.
*      l_data = is_ztpp013-data_m - 1.
*      SELECT * FROM ztpp013 APPENDING CORRESPONDING FIELDS OF TABLE @lt_estorno UP TO @lines ROWS
*        WHERE data_m <= @l_data
*          AND matnr   = @is_ztpp013-matnr
*          AND estorno = @space
*          AND tpmov   = 1
*          AND equnr   = @is_ztpp013-equnr
*          AND ndoc_estorn = @space ORDER BY data_m DESCENDING, datum DESCENDING , uzeit DESCENDING.
*    ENDIF.
    IF lt_estorno IS INITIAL.
      ls_log-datum = sy-datum.
      ls_log-uzeit = sy-uzeit.
      ls_log-ru_arbpli  = is_ztpp013-ru_arbpli.
      ls_log-aufnr      = is_ztpp013-aufnr.
      ls_log-matnr      = is_ztpp013-matnr.
      ls_log-quantidade = is_ztpp013-xmnga * -1.
      ls_log-ip_adam    = is_ztpp013-zidmovpcf.
      INSERT ztpp013_3 FROM ls_log.
      EXIT.
    ENDIF.
    LOOP AT lt_estorno INTO DATA(ls_estorno).
      CLEAR: ls_ztpp013.
      l_quant =  l_quant - ls_estorno-lmnga.

      MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
      ls_ztpp013-estorno = abap_true.
      DATA(l_num) = save_mov_man( is_ztpp013 = ls_ztpp013 ).
*      UPDATE ztpp013 SET ndoc_estorn = l_num
*          WHERE zidmovpcf   = ls_estorno-zidmovpcf
*            AND gjahr       = ls_estorno-gjahr.
      upd_ztpp013( zidmovpcf = ls_ztpp013-zidmovpcf gjahr = ls_ztpp013-gjahr ndoc_estorn = l_num ).
      IF l_quant = 0.
        EXIT.
      ENDIF.
      IF l_quant < 0.
        MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
        ls_ztpp013-lmnga = l_quant * -1.
        save_mov_man( is_ztpp013 =  ls_ztpp013 ).
        EXIT.
      ENDIF.
    ENDLOOP.

*    ENDSELECT.

  ENDMETHOD.


  METHOD estorno_retrabalho.
    DATA: ldatai     TYPE datum,
          ls_log     TYPE ztpp013_3,
          ls_ztpp013 TYPE ztpp013,
          l_quant    TYPE ztpp013-lmnga,
          ls_error   TYPE ztpp013_3,
          r_arbpl    TYPE RANGE OF ztpp013-ru_arbpli,
          lr_aufnr   TYPE RANGE OF aufnr,
          lr_idmov   TYPE RANGE OF zidmovpcf,
          lr_schprog TYPE RANGE OF ztpp013-schprog,
          lr_data_m  TYPE RANGE OF datum.

    ldatai = is_ztpp013-data_m(06) && '01'.
    lr_data_m = VALUE #( ( sign = 'I' option = 'BT' low = ldatai high = is_ztpp013-data_m ) ).
    CASE is_ztpp013-ru_arbpli(04).

      WHEN 'PREP'.
        r_arbpl = VALUE #(
        ( sign = 'I' option = 'EQ' low = 'PINT_001' )
        ( sign = 'I' option = 'EQ' low = is_ztpp013-ru_arbpli )
         ).

*        SELECT * FROM ztpp013 INTO TABLE @DATA(lt_estorno)
*          WHERE data_m >= @ldatai
*            AND data_m <= @is_ztpp013-data_m
*            AND ru_arbpli IN @r_arbpl
*            AND matnr   = @is_ztpp013-matnr
*            AND tpmov   = 1
*            AND estorno = @space
*            AND ndoc_estorn = @space
*          ORDER BY ru_arbpli ASCENDING,datum DESCENDING ,uzeit DESCENDING.
        SELECT * FROM ztpp013 INTO TABLE @DATA(lt_estorno)
            WHERE matnr = @is_ztpp013-matnr
             " AND aufnr <> space"IN lr_aufnr
              AND data_m IN @lr_data_m">= ldatai
     "         AND data_m <= is_ztpp013-data_m
            "  AND zidmovpcf <> space "IN lr_idmov
              AND ru_arbpli IN @r_arbpl
              AND tpmov   = 1.
        "  AND schprog <> space " lr_schprog
        "  AND estorno = space
        "  AND ndoc_estorn = space.
        "ORDER BY ru_arbpli ASCENDING datum DESCENDING uzeit DESCENDING .
        SORT lt_estorno BY estorno.
        DELETE lt_estorno WHERE estorno <> space.
        SORT lt_estorno BY ndoc_estorn.
        DELETE lt_estorno WHERE ndoc_estorn <> space.
        SORT lt_estorno BY ru_arbpli ASCENDING datum DESCENDING uzeit DESCENDING .
        IF NOT lt_estorno IS INITIAL.
          l_quant = is_ztpp013-rmnga.
          LOOP AT lt_estorno INTO DATA(ls_estorno) WHERE ru_arbpli = 'PINT_001'.
            CLEAR: ls_ztpp013.
            MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
            ls_ztpp013-estorno = abap_true.
            DATA(l_num) = save_mov_man( is_ztpp013 = ls_ztpp013 no_upd_time = abap_true ).
            TRY.
                upd_ztpp013(
                 EXPORTING
                   zidmovpcf    = ls_estorno-zidmovpcf
                   gjahr        = ls_estorno-gjahr
                   ndoc_estorn  = l_num
                   datum        = ls_estorno-datum
               ).
*                UPDATE ztpp013 SET ndoc_estorn = l_num
*                    WHERE zidmovpcf   = ls_estorno-zidmovpcf
*                      AND gjahr       = ls_estorno-gjahr.
              CATCH cx_sy_open_sql_db INTO DATA(lf_err).
                DATA(lf_message) = lf_err->get_text( ).
                CLEAR: ls_error.
                ls_error-datum   = sy-datum.
                ls_error-uzeit   = sy-uzeit.
                ls_error-ip_adam = ls_estorno-zidmovpcf.
                ls_error-matnr   = lf_message.
                INSERT ztpp013_3 FROM ls_error.
            ENDTRY.

            l_quant = l_quant - ls_estorno-lmnga.
            IF l_quant = 0.
              EXIT.
            ENDIF.
            IF l_quant < 0.
              MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
              ls_ztpp013-lmnga = l_quant * -1.
              save_mov_man( is_ztpp013 =  ls_ztpp013 no_upd_time = abap_true ).
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_quant > 0.
            CLEAR: ls_log.
            ls_log-datum      = sy-datum.
            ls_log-uzeit      = sy-uzeit.
            ls_log-ru_arbpli  = is_ztpp013-ru_arbpli.
            ls_log-matnr      = is_ztpp013-matnr.
            ls_log-aufnr      = is_ztpp013-aufnr.
            ls_log-quantidade =  l_quant.
            TRY.
                INSERT ztpp013_3 FROM ls_log.
              CATCH  cx_sy_open_sql_db.
            ENDTRY.
          ENDIF.
          l_quant = is_ztpp013-rmnga.
          LOOP AT lt_estorno INTO ls_estorno WHERE ru_arbpli CP 'PREP*'.
            CLEAR: ls_ztpp013.
            MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
            ls_ztpp013-estorno = abap_true.
            l_num = save_mov_man( is_ztpp013 = ls_ztpp013 no_upd_time = abap_true ).
            TRY.
                upd_ztpp013(
                    EXPORTING
                      zidmovpcf    = ls_estorno-zidmovpcf
                      gjahr        = ls_estorno-gjahr
                      ndoc_estorn  = l_num
                      datum        = ls_estorno-datum
                  ).
*                UPDATE ztpp013 SET ndoc_estorn = l_num
*                    WHERE zidmovpcf   = ls_estorno-zidmovpcf
*                      AND gjahr       = ls_estorno-gjahr.
              CATCH cx_sy_open_sql_db INTO lf_err.
                lf_message = lf_err->get_text( ).
                CLEAR: ls_error.
                ls_error-datum   = sy-datum.
                ls_error-uzeit   = sy-uzeit.
                ls_error-ip_adam = ls_estorno-zidmovpcf.
                ls_error-matnr   = lf_message.
                INSERT ztpp013_3 FROM ls_error.
            ENDTRY.

            l_quant = l_quant - ls_estorno-lmnga.
            IF l_quant = 0.
              EXIT.
            ENDIF.
            IF l_quant < 0.
              MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
              ls_ztpp013-lmnga = l_quant * -1.
              save_mov_man( is_ztpp013 =  ls_ztpp013 no_upd_time = abap_true ).
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_quant > 0.
            CLEAR: ls_log.
            ls_log-datum      = sy-datum.
            ls_log-uzeit      = sy-uzeit.
            ls_log-ru_arbpli  = 'PREP_001'.
            ls_log-matnr      = is_ztpp013-matnr.
            ls_log-aufnr      = is_ztpp013-aufnr.
            ls_log-quantidade =  l_quant.
            TRY.
                INSERT ztpp013_3 FROM ls_log.
              CATCH  cx_sy_open_sql_db.
            ENDTRY.
          ENDIF.
        ELSE.
          ls_log-datum      = sy-datum.
          ls_log-uzeit      = sy-uzeit.
          ls_log-ru_arbpli  = is_ztpp013-ru_arbpli.
          ls_log-ip_adam    = 'PREP_001'.
          ls_log-matnr      = is_ztpp013-matnr.
          ls_log-aufnr      = is_ztpp013-aufnr.
          ls_log-quantidade = is_ztpp013-rmnga.
          TRY.
              INSERT ztpp013_3 FROM ls_log.
            CATCH  cx_sy_open_sql_db.
          ENDTRY.
        ENDIF.
      WHEN 'DIAM'.
        r_arbpl = VALUE #(
        ( sign = 'I' option = 'EQ' low = 'PINT_003' )
        ( sign = 'I' option = 'EQ' low = is_ztpp013-ru_arbpli )
         ).
        l_quant = is_ztpp013-rmnga.
        SELECT * FROM ztpp013 INTO TABLE lt_estorno
           WHERE matnr = is_ztpp013-matnr
            " AND aufnr <> space"IN lr_aufnr
             AND data_m IN lr_data_m">= ldatai
    "         AND data_m <= is_ztpp013-data_m
           "  AND zidmovpcf <> space "IN lr_idmov
             AND ru_arbpli IN r_arbpl
             AND tpmov   = 1.
        "  AND schprog <> space " lr_schprog
        "  AND estorno = space
        "  AND ndoc_estorn = space.
        "ORDER BY ru_arbpli ASCENDING datum DESCENDING uzeit DESCENDING .
        SORT lt_estorno BY estorno.
        DELETE lt_estorno WHERE estorno <> space.
        SORT lt_estorno BY ndoc_estorn.
        DELETE lt_estorno WHERE ndoc_estorn <> space.
        SORT lt_estorno BY ru_arbpli ASCENDING datum DESCENDING uzeit DESCENDING .

        IF NOT lt_estorno IS INITIAL.
          LOOP AT lt_estorno INTO ls_estorno WHERE ru_arbpli = 'PINT_003'.
            DATA(l_pint_ok) = abap_true.
            CLEAR: ls_ztpp013.
            MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
            ls_ztpp013-estorno = abap_true.
            l_num = save_mov_man( is_ztpp013 = ls_ztpp013 no_upd_time = abap_true ).
            TRY.
                upd_ztpp013(
                  EXPORTING
                    zidmovpcf    = ls_estorno-zidmovpcf
                    gjahr        = ls_estorno-gjahr
                    ndoc_estorn  = l_num
                    datum        = ls_estorno-datum
                ).
*                UPDATE ztpp013 SET ndoc_estorn = l_num
*                    WHERE zidmovpcf   = ls_estorno-zidmovpcf
*                      AND gjahr       = ls_estorno-gjahr.
              CATCH cx_sy_open_sql_db INTO lf_err.
                lf_message = lf_err->get_text( ).
                CLEAR: ls_error.
                ls_error-datum   = sy-datum.
                ls_error-uzeit   = sy-uzeit.
                ls_error-ip_adam = ls_estorno-zidmovpcf.
                ls_error-matnr   = lf_message.
                INSERT ztpp013_3 FROM ls_error.
                CONTINUE.
            ENDTRY.

            l_quant = l_quant - ls_estorno-lmnga.
            IF l_quant = 0.
              EXIT.
            ENDIF.
            IF l_quant < 0.
              MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
              ls_ztpp013-lmnga = l_quant * -1.
              save_mov_man( is_ztpp013 =  ls_ztpp013 no_upd_time = abap_true ).
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_pint_ok = abap_false.
            CLEAR: ls_error.
            ls_error-datum   = sy-datum.
            ls_error-uzeit   = sy-uzeit.
            ls_error-ip_adam = ls_estorno-zidmovpcf.
            ls_error-matnr   = 'SEM SALDO PINT'.
            INSERT ztpp013_3 FROM ls_error.
          ENDIF.
          CLEAR: l_pint_ok.
          IF l_quant > 0.
            CLEAR: ls_log.
            ls_log-datum      = sy-datum.
            ls_log-uzeit      = sy-uzeit.
            ls_log-ru_arbpli  = is_ztpp013-ru_arbpli.
            ls_log-matnr      = is_ztpp013-matnr.
            ls_log-aufnr      = is_ztpp013-aufnr.
            ls_log-quantidade =  l_quant.
            TRY.
                INSERT ztpp013_3 FROM ls_log.
              CATCH  cx_sy_open_sql_db.
            ENDTRY.
          ENDIF.
          l_quant = is_ztpp013-rmnga.
          LOOP AT lt_estorno INTO ls_estorno WHERE ru_arbpli CP 'DIAM*'.
            DATA(l_diam_ok) = abap_true.
            CLEAR: ls_ztpp013.
            " CHECK SE A ETIQUETA JÁ FOI LIDA.
            SELECT SINGLE idetiqueta,lido FROM ztpp029_1 INTO @DATA(ls_ztpp029)
            WHERE estornado = @abap_false
              AND delete_r  = @abap_false
              AND lido      = @abap_true
              AND zidmovpcf = @ls_estorno-zidmovpcf.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
              ls_ztpp013-estorno = abap_true.
              l_num = save_mov_man( is_ztpp013 = ls_ztpp013 no_upd_time = abap_true ).
              TRY.
                  upd_ztpp013(
                     EXPORTING
                       zidmovpcf    = ls_estorno-zidmovpcf
                       gjahr        = ls_estorno-gjahr
                       ndoc_estorn  = l_num
                       datum        = ls_estorno-datum
                   ).

                CATCH cx_sy_open_sql_db INTO lf_err.
                  lf_message = lf_err->get_text( ).
                  CLEAR: ls_error.
                  ls_error-datum   = sy-datum.
                  ls_error-uzeit   = sy-uzeit.
                  ls_error-ip_adam = ls_estorno-zidmovpcf.
                  ls_error-matnr   = lf_message.
                  INSERT ztpp013_3 FROM ls_error.
                  CONTINUE.
              ENDTRY.
            ELSE.
              CLEAR: ls_error.
              ls_error-datum   = sy-datum.
              ls_error-uzeit   = sy-uzeit.
              ls_error-ip_adam = ls_estorno-zidmovpcf.
              ls_error-matnr   = 'ETQ NÃO LIDA'.
              INSERT ztpp013_3 FROM ls_error.
              CONTINUE.
            ENDIF.

            l_quant = l_quant - ls_estorno-lmnga.
            IF l_quant = 0.
              EXIT.
            ENDIF.
            IF l_quant < 0.
              MOVE-CORRESPONDING ls_estorno TO ls_ztpp013.
              ls_ztpp013-lmnga = l_quant * -1.
              save_mov_man( is_ztpp013 =  ls_ztpp013 no_upd_time = abap_true ).
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_diam_ok = abap_false.
            CLEAR: ls_error.
            ls_error-datum   = sy-datum.
            ls_error-uzeit   = sy-uzeit.
            ls_error-ip_adam = ls_estorno-zidmovpcf.
            ls_error-matnr   = 'SEM SALDO DIAM'.
            INSERT ztpp013_3 FROM ls_error.
          ENDIF.
          CLEAR: l_diam_ok.
          IF l_quant > 0.
            CLEAR: ls_log.
            ls_log-datum      = sy-datum.
            ls_log-uzeit      = sy-uzeit.
            ls_log-ru_arbpli  = 'DIAM_001'.
            ls_log-matnr      = is_ztpp013-matnr.
            ls_log-aufnr      = is_ztpp013-aufnr.
            ls_log-quantidade =  l_quant.
            TRY.
                INSERT ztpp013_3 FROM ls_log.
              CATCH  cx_sy_open_sql_db.
            ENDTRY.
          ENDIF.
        ELSE.
          ls_log-datum      = sy-datum.
          ls_log-uzeit      = sy-uzeit.
          ls_log-ru_arbpli  = is_ztpp013-ru_arbpli.
          ls_log-ip_adam    = 'DIAM_001'.
          ls_log-matnr      = is_ztpp013-matnr.
          ls_log-aufnr      = is_ztpp013-aufnr.
          ls_log-quantidade = is_ztpp013-rmnga.
          TRY.
              INSERT ztpp013_3 FROM ls_log.
            CATCH  cx_sy_open_sql_db.
          ENDTRY.
        ENDIF.
      WHEN 'PINT_002'.

    ENDCASE.
  ENDMETHOD.


  METHOD export_mem_rec.
    SET PARAMETER ID sy-tcode FIELD arbpl.
  ENDMETHOD.


  METHOD gerar_number_etq_ref.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZPPRDETQRF'
        quantity    = '1'
      IMPORTING
        number      = number.

  ENDMETHOD.


  METHOD get_data_order.

    DATA: ls_order_objects TYPE bapi_pp_order_objects,
          lt_header        TYPE TABLE OF bapi_order_header1,
          lt_components    TYPE TABLE OF bapi_order_component,
          ls_imseg         TYPE imseg,
          ls_error         TYPE ztpp013_3.

    CLEAR: ct_header, ct_components.

    LOOP AT it_orders INTO DATA(ls_order). " Apontamento de Entrada
      ls_order_objects-header     = abap_true.
      ls_order_objects-components = abap_true.

      "   APPEND ls_data TO lt_upt_data.

      CLEAR: lt_header, lt_components.

      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
        EXPORTING
          number        = ls_order-aufnr
          order_objects = ls_order_objects
        TABLES
          header        = lt_header
          component     = lt_components.

      APPEND LINES OF lt_header TO ct_header.
      APPEND LINES OF lt_components TO ct_components.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_etiqueta_ref.
    calcule_date( i_arbpl = 'INJ_G001' ).
    CLEAR: gt_itens_ref.
    SELECT matnr SUM( xmnga ) AS quantidade FROM zvs_pp029_pp013
      INTO TABLE gt_itens_ref
      WHERE idetiqueta = i_etq
        AND data_m     >= datai
        AND data_m     <= dataf
        AND delete_r   = space
        AND estornado  = space
        AND lido       = space
      GROUP BY matnr.
    IF sy-subrc <> 0.
      nok = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_grp_status.
    SELECT '@00@' AS selected, ztpp020~* FROM ztpp020 INTO CORRESPONDING FIELDS OF TABLE @et_grp_status WHERE id_grupo >= 2.
  ENDMETHOD.


  METHOD get_imseg.
    t_imseg = et_imseg.
  ENDMETHOD.


  METHOD get_mard.
    SELECT * FROM mard INTO TABLE et_mard
      WHERE matnr IN it_matnr
        AND lgort IN it_lgort
        AND werks IN it_werks
        AND labst <> 0.
  ENDMETHOD.


  METHOD get_number_operation.
    DATA: ls_order_objects TYPE bapi_pp_order_objects,
          lt_operations    TYPE TABLE OF bapi_order_operation1,
          lt_components    TYPE TABLE OF bapi_order_component.

    ls_order_objects-operations = abap_true.
    ls_order_objects-components = abap_true.
    DO 10 TIMES.
      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
        EXPORTING
          number        = i_aufnr
          order_objects = ls_order_objects
        TABLES
          operation     = lt_operations
          component     = lt_components.
      IF NOT lt_operations IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    TRY.
        operation_number = lt_operations[ work_center = i_arbpli ]-operation_number.
      CATCH cx_sy_itab_line_not_found.
        DATA(l_arbpl) = i_arbpli(04) && '*'.
        IF i_arbpli(06) = 'TRAT_0'.
          l_arbpl = i_arbpli(06) && '*'.
        ENDIF.
        LOOP AT lt_operations INTO DATA(ls_op) WHERE work_center CP l_arbpl.
          operation_number = ls_op-operation_number.
          EXIT.
        ENDLOOP.
        IF operation_number IS INITIAL.
          TRY.
              operation_number = lt_operations[ work_center = i_arbpli ]-operation_number.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDIF.
    ENDTRY.
    IF line_exists( lt_components[ operation = operation_number  ] ) OR
       line_exists( lt_operations[ opr_cntrl_key = 'YBP5' operation_number = operation_number ] ).
      baixa_mat = abap_true.
    ENDIF.

    IF i_checkap = abap_true.
      DATA(li) = line_index( lt_operations[ work_center = i_arbpli ] ).
      li = li - 1.
      DATA(vorn_old) = lt_operations[ li ]-operation_number.
*      checar se a apontamento anterior para a ordem
*      SELECT SINGLE aufnr FROM ztpp013 INTO @DATA(l_check) WHERE aufnr = @i_aufnr AND vornr = @vorn_old.
*      IF sy-subrc = 0.
      apont_ok = abap_true.
*      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_ordens.
    calcule_date( i_arbpl = i_arbpl ).
    SELECT a~*, c~*  FROM crhd_v1 AS a
      INNER JOIN afvc AS b
      ON b~arbid = a~objid
      INNER JOIN zvs_afpo_afkko AS c
      ON c~aufpl = b~aufpl
      INTO CORRESPONDING FIELDS OF TABLE @et_ordens
      WHERE a~spras = @sy-langu
        AND a~arbpl = @i_arbpl
        AND c~gstrp >= @datai
        AND c~gstrp <= @dataf.
    IF NOT i_matnr IS INITIAL.
      SORT et_ordens BY matnr.
      DELETE et_ordens WHERE matnr <> i_matnr.
    ENDIF.
    SORT et_ordens BY matnr aufnr.
  ENDMETHOD.


  METHOD get_ordens_cp.
    calcule_date( i_arbpl = i_arbpl ).
    SELECT a~*, c~*  FROM crhd_v1 AS a
      INNER JOIN afvc AS b
      ON b~arbid = a~objid
      INNER JOIN zvs_afpo_afkko AS c
      ON c~aufpl = b~aufpl
      INTO CORRESPONDING FIELDS OF TABLE @et_ordens
      WHERE a~spras = @sy-langu
        AND a~arbpl LIKE @i_arbpl
        AND c~gstrp >= @datai
        AND c~gstrp <= @dataf.
    IF NOT i_matnr IS INITIAL.
      SORT et_ordens BY matnr.
      DELETE et_ordens WHERE matnr <> i_matnr.
    ENDIF.
    SORT et_ordens BY matnr aufnr.
  ENDMETHOD.


  METHOD get_recurso.
    DATA: ls_recursos_aux TYPE ty_rec,
          l_arbpl         TYPE zel_recurso.
    GET PARAMETER ID sy-tcode FIELD l_arbpl.
    DATA(lt_aux_rec) = t_recursos.
    SELECT '@00@' AS selected, ztpp019_1~recurso AS arbpl , ztpp019_1~descrec AS ktext FROM ztpp019
    INNER JOIN ztpp019_1 ON ztpp019_1~grupoprod = ztpp019~grupoprod
    INNER JOIN crhd_v1 ON crhd_v1~arbpl = ztpp019_1~recurso
    INTO CORRESPONDING FIELDS OF TABLE @t_recursos
    WHERE ztpp019~uname   = @sy-uname
      AND ztpp019_1~tcode = @sy-tcode.

    LOOP AT t_recursos INTO DATA(ls_recursos).
      READ TABLE lt_aux_rec ASSIGNING FIELD-SYMBOL(<rec>) WITH KEY arbpl = ls_recursos-arbpl.
      IF sy-subrc = 0.
        <rec>-arbpl = ls_recursos-arbpl.
        <rec>-ktext = ls_recursos-ktext.
      ENDIF.
    ENDLOOP.
*    IF lines( t_recursos ) = 1.
*      READ TABLE t_recursos ASSIGNING FIELD-SYMBOL(<recurso>) INDEX 1.
*      <recurso>-selected = icon_checked.
*    ENDIF.
    IF NOT l_arbpl IS INITIAL.
      READ TABLE t_recursos ASSIGNING FIELD-SYMBOL(<recursos>) WITH KEY arbpl = l_arbpl.
      IF <recursos> IS ASSIGNED.
        <recursos>-selected = icon_checked.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_status.
    IF id_grupo IS INITIAL.
      SELECT '@00@' AS selected, ztpp021~* FROM ztpp021 INTO CORRESPONDING FIELDS OF TABLE @et_status WHERE id_grupo > 2.
    ELSE.
      SELECT '@00@' AS selected, ztpp021~* FROM ztpp021 INTO CORRESPONDING FIELDS OF TABLE @et_status WHERE id_grupo = @id_grupo.

    ENDIF.
  ENDMETHOD.


  METHOD get_turno.
    SELECT SINGLE code FROM ztpp028_2
      INTO  @DATA(l_code)
      WHERE arbpl = @i_arbpl.
    IF sy-subrc <> 0.
      l_code = '01'.
    ENDIF.
    SELECT code,
           schprog,
           eshift_start,
           eshift_end,
           next_day
      FROM ztpp028_1 INTO TABLE @DATA(lt_turno)
      WHERE code = @l_code
        AND eshift_start <= @sy-uzeit.
    IF sy-subrc = 0.
      SORT lt_turno BY eshift_start DESCENDING.
      TRY.
          DATA(ls_turno) = lt_turno[ 1 ].
        CATCH cx_sy_itab_line_not_found.
          e_budat = sy-datum.
          RETURN.
      ENDTRY.
    ELSE.
      SELECT code,
           schprog,
           eshift_start,
           eshift_end,
           next_day
      FROM ztpp028_1 INTO CORRESPONDING FIELDS OF TABLE @lt_turno
      WHERE code = @l_code
        AND eshift_start >= @sy-uzeit.
      SORT lt_turno BY eshift_end ASCENDING.
      TRY.
          ls_turno = lt_turno[ 1 ].
        CATCH cx_sy_itab_line_not_found.
          e_budat = sy-datum.
          RETURN.
      ENDTRY.
    ENDIF.



    IF ls_turno-next_day = abap_true.
      CASE ls_turno-schprog.
        WHEN 1.
          e_budat = sy-datum.
          e_schprog = ls_turno-schprog.
          IF sy-uzeit >= ls_turno-eshift_start.
            e_budat = sy-datum + 1.
          ENDIF.

        WHEN 2 OR 3.
          e_budat = sy-datum.
          e_schprog = ls_turno-schprog.
          IF sy-uzeit <= ls_turno-eshift_end.
            e_budat = sy-datum - 1.
          ENDIF.
      ENDCASE.
    ELSE.
      e_budat = sy-datum.
      e_schprog = ls_turno-schprog.
    ENDIF.

    MOVE-CORRESPONDING ls_turno TO es_turno.
  ENDMETHOD.


  METHOD modify_recurso.
    DATA: ls_save TYPE ztpp018_1.

    MOVE-CORRESPONDING is_ztpp018 TO ls_save.

    MODIFY ztpp018_1 FROM ls_save.
  ENDMETHOD.


  METHOD pesagem_usinagem.

    is_ztpp039-datum = sy-datum.
    is_ztpp039-uzeit = sy-uzeit.

    INSERT ztpp039 FROM is_ztpp039.

  ENDMETHOD.


  METHOD print_etq.
    CALL FUNCTION 'ZFPP_PRINT_ETQ_REFUGO_RD'
      EXPORTING
        is_etiqueta = gs_etiqueta.
  ENDMETHOD.


  METHOD reimprimir_etq.
    DATA: ls_log TYPE ztpp038.
    SELECT SINGLE * FROM ztpp029 INTO gs_etiqueta
      WHERE idetiqueta = i_etq.
    IF sy-subrc = 0.
      print_etq( ).
      MESSAGE i000(zpp) WITH 1 'Etiqueta(s) enviada(s) para impressão'.
      ok = abap_true.
      gs_etiqueta-contagem = gs_etiqueta-contagem + 1.
      UPDATE ztpp029 SET contagem = gs_etiqueta-contagem WHERE idetiqueta = i_etq.
      ls_log-idetiqueta = i_etq.
      ls_log-mandt      = sy-mandt.
      ls_log-datum      = sy-datum.
      ls_log-uzeit      = sy-uzeit.
      ls_log-uname      = sy-uname.
      SELECT SINGLE name_text FROM v_usr_name INTO ls_log-name_text WHERE bname = sy-uname.
      INSERT ztpp038 FROM ls_log.
      COMMIT WORK.
    ELSE.
      MESSAGE i000(zpp) WITH 'Etiqueta' i_etq 'não existe' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD save_conf_tpp013.
    DATA:ls_conf4    TYPE ztpp013_4, "
         l_range(10) TYPE n.


    is_ztpp013-mandt = sy-mandt.
    is_ztpp013-datum = sy-datum.
    is_ztpp013-uzeit = sy-uzeit.
    is_ztpp013-gjahr = is_ztpp013-data_m(04).
    is_ztpp013-uname = sy-uname.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZIDMOVPCF'
        toyear                  = sy-datum(04)
      IMPORTING
        number                  = l_range "-- Newly generated Number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    idmov = is_ztpp013-zidmovpcf = l_range.
    MOVE-CORRESPONDING is_ztpp013 TO ls_conf4.

    INSERT ztpp013_4 FROM ls_conf4.
  ENDMETHOD.


  METHOD save_etq_print.
    DATA: lt_log TYPE TABLE OF ztpp038.
    LOOP AT gt_etqs_ref ASSIGNING FIELD-SYMBOL(<etq>).
      <etq>-contagem = 1.
      SELECT SINGLE name_text FROM v_usr_name INTO @DATA(l_name) WHERE bname = @sy-uname.
      APPEND VALUE ztpp038(
      mandt = sy-mandt
      idetiqueta = <etq>-idetiqueta
      datum = sy-datum
      uzeit = sy-uzeit
      uname = sy-uname
      name_text = l_name ) TO lt_log.
    ENDLOOP.
    INSERT ztpp029 FROM TABLE gt_etqs_ref.
    COMMIT WORK.
    INSERT ztpp038 FROM TABLE lt_log.
    COMMIT WORK.
    LOOP AT gt_etqs_ref INTO gs_etiqueta.
      print_etq( ).
    ENDLOOP.
    DATA(lines) = lines( gt_etqs_ref ).
    MESSAGE i000(zpp) WITH lines 'Etiqueta(s) enviada(s) para impressão'.
  ENDMETHOD.


  METHOD save_memory_etq_ref.
    CLEAR: gs_etiqueta.
    SELECT SINGLE descrec FROM ztpp019_1
      INTO @DATA(l_desc) WHERE recurso = @is_ztpp013-ru_arbpli.
    CASE is_ztpp013-ru_arbpli(04).
      WHEN 'USIN'.
        gs_etiqueta-local_ct = 'USINAGEM'.
      WHEN 'DIAM'.
        gs_etiqueta-local_ct = 'DIAMANTAMENTO'.
      WHEN OTHERS.
        gs_etiqueta-local_ct   = is_ztpp013-ru_arbpli && | | && '-' && | | && l_desc .
    ENDCASE.
    TRY.
        gs_etiqueta =  gt_etqs_ref[ local_ct = gs_etiqueta-local_ct ].
        gv_recursoold          = is_ztpp013-ru_arbpli.
      CATCH cx_sy_itab_line_not_found.
        gs_etiqueta-mandt      = sy-mandt.
        gs_etiqueta-data       = sy-datum.
        gs_etiqueta-hora       = sy-uzeit.
        gs_etiqueta-ernam      = sy-uname.
        gs_etiqueta-idetiqueta = gerar_number_etq_ref( ).
    ENDTRY.
    gs_etiqueta-quantidade = is_ztpp013-xmnga.
    COLLECT gs_etiqueta INTO gt_etqs_ref.

*    print_etq( ).
  ENDMETHOD.


  METHOD save_mov.
    DATA:  l_range(10) TYPE n.
    get_turno(
        EXPORTING
          i_arbpl   = is_ztpp013-ru_arbpli    " Centro de trabalho
        IMPORTING
          e_budat   =  is_ztpp013-data_m    " Data de lançamento
          e_schprog =  is_ztpp013-schprog    " Chave da seqüência de turnos
            ).
    is_ztpp013-mandt = sy-mandt.
    is_ztpp013-datum = sy-datum.
    is_ztpp013-uzeit = sy-uzeit.
    is_ztpp013-gjahr = is_ztpp013-data_m(04).
    is_ztpp013-uname = sy-uname.

    CLEAR: is_ztpp013-mov,
           is_ztpp013-mblnr,
           is_ztpp013-mjahr.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZIDMOVPCF'
        toyear                  = sy-datum(04)
      IMPORTING
        number                  = l_range "-- Newly generated Number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      ct_bapiret = VALUE #(
      ( type =  sy-msgty id = sy-msgid number =  sy-msgno
        message_v1 = sy-msgv1 message_v2 = sy-msgv2
        message_v3 = sy-msgv3 message_v4 = sy-msgv4 )
              ).
      EXIT.
    ENDIF.
    idmov = is_ztpp013-zidmovpcf = l_range.

    TRY.
        INSERT ztpp013 FROM is_ztpp013.
        COMMIT WORK AND WAIT.
      CATCH  cx_sy_open_sql_db.
    ENDTRY.

    "eSTORNAR REFUGO DE INJETORA
*    IF is_ztpp013-tpmov = 3 AND is_ztpp013-ru_arbpli(03) = 'INJ'.
*      estorna_mov( is_ztpp013 = is_ztpp013 ).
*    ENDIF.
  ENDMETHOD.


  METHOD save_mov_man.
    DATA:  l_range(10) TYPE n.
    is_ztpp013-mandt = sy-mandt.
    IF no_upd_time = abap_false.
      is_ztpp013-datum = sy-datum.
      is_ztpp013-uzeit = sy-uzeit.
      is_ztpp013-gjahr = is_ztpp013-data_m(04).
      is_ztpp013-uname = sy-uname.
    ENDIF.

    CLEAR: is_ztpp013-mov,
           is_ztpp013-mblnr,
           is_ztpp013-mjahr."

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZIDMOVPCF'
        toyear                  = sy-datum(04)
      IMPORTING
        number                  = l_range "-- Newly generated Number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      ct_bapiret = VALUE #(
      ( type =  sy-msgty id = sy-msgid number =  sy-msgno
        message_v1 = sy-msgv1 message_v2 = sy-msgv2
        message_v3 = sy-msgv3 message_v4 = sy-msgv4 )
              ).
      EXIT.
    ENDIF.
    idmov = is_ztpp013-zidmovpcf = l_range.
    TRY.
        INSERT ztpp013 FROM is_ztpp013.
        COMMIT WORK AND WAIT.
      CATCH  cx_sy_open_sql_db.
    ENDTRY.

    "eSTORNAR REFUGO DE INJETORA
*    IF is_ztpp013-tpmov = 3 AND is_ztpp013-ru_arbpli(03) = 'INJ'.
*      estorna_mov_man( is_ztpp013 = is_ztpp013 ).
*    ENDIF.
  ENDMETHOD.


  METHOD save_ztpp018.
    DATA: l_hora_f TYPE ztpp018-hora_f.
    IF NOT is_ztpp018-arbpl IS INITIAL.
      TRY.
          l_hora_f = ( sy-uzeit - 000001 ).
          UPDATE ztpp018 SET hora_f = l_hora_f data_f = sy-datum
             WHERE arbpl = is_ztpp018-arbpl AND ativo = abap_true.
          COMMIT WORK.
        CATCH  cx_sy_open_sql_db.
      ENDTRY.
      TRY.
          UPDATE ztpp018 SET ativo = space
           WHERE arbpl = is_ztpp018-arbpl.
          COMMIT WORK.
        CATCH  cx_sy_open_sql_db.
      ENDTRY.
*      IF is_ztpp018-data IS INITIAL.
      is_ztpp018-data  = sy-datum.
*      ENDIF.
*      IF is_ztpp018-hora  IS INITIAL.
      is_ztpp018-hora  = sy-uzeit.
*      ENDIF.
*      IF is_ztpp018-bname IS INITIAL.
      is_ztpp018-bname = sy-uname.
*      ENDIF.

      is_ztpp018-ativo = abap_true.
      get_turno(
            EXPORTING
              i_arbpl   = is_ztpp018-arbpl   " Centro de trabalho
          IMPORTING
            es_turno = DATA(ls_turno)
                    ).
      is_ztpp018-schprog = ls_turno-schprog.
      is_ztpp018-code    = ls_turno-code.
      is_ztpp018-turno_i = ls_turno-eshift_start.
      is_ztpp018-turno_f = ls_turno-eshift_end.
      TRY.
          INSERT ztpp018 FROM is_ztpp018.
          IF sy-subrc = 0.
            COMMIT WORK.
            ok = abap_true.
          ENDIF.
        CATCH  cx_sy_open_sql_db.
      ENDTRY.
    ELSE.
      message = text-001.
    ENDIF.
  ENDMETHOD.


  METHOD upd_ztpp013.
    DATA: lr_datum TYPE RANGE OF datum.
    IF NOT datum IS INITIAL.
      lr_datum = VALUE #( ( sign = 'I' option = 'EQ' low = datum ) ).
    ENDIF.
    SELECT SINGLE * FROM ztpp013 INTO @DATA(ls_upd_013)
                    WHERE datum      IN @lr_datum
                      AND zidmovpcf   = @zidmovpcf
                      AND gjahr       = @gjahr.
    IF sy-subrc = 0.
      ls_upd_013-ndoc_estorn = ndoc_estorn.
      MODIFY ztpp013 FROM ls_upd_013.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD upd_ztpp013_mov.

    cs_upd_013-mov = abap_true.
    cs_upd_013-message = space.

    APPEND cs_upd_013 TO gt_modify013.
  ENDMETHOD.
ENDCLASS.
