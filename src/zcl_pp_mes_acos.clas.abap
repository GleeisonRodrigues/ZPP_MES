CLASS zcl_pp_mes_acos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
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
    TYPES:
      BEGIN OF ty_grp_status,
        selected TYPE zel_selecionar,
        id_grupo TYPE ztpp020-id_grupo,
        grupo_st TYPE ztpp020-grupo_st,
      END OF ty_grp_status .
    TYPES:
      BEGIN OF ty_orders,
        aufnr TYPE aufnr,
      END OF  ty_orders .
    TYPES:
      tt_status TYPE TABLE OF ty_status .
    TYPES:
      tt_grp_status TYPE TABLE OF ty_grp_status .
    TYPES:
      tt_orders TYPE TABLE OF ty_orders .
    TYPES:
      tyr_matnr TYPE RANGE OF matnr .
    TYPES:
      tyr_lgort TYPE RANGE OF lgort_d .
    TYPES:
      tyr_werks TYPE RANGE OF werks_d .
    TYPES:
      BEGIN OF ty_rec,
        selected TYPE zel_selecionar,
        arbpl    TYPE ztpp019_1-recurso,
        ktext    TYPE ztpp019_1-descrec,
      END OF ty_rec .
    TYPES:
      tt_rec TYPE TABLE OF ty_rec .
    TYPES:
      range_datum          TYPE RANGE OF datum .
    TYPES:
      tt_ztpp013 TYPE TABLE OF ztpp013 .
    TYPES:
      BEGIN OF ty_conf_adam,
        icon           TYPE icon-id,
        counterold(20) TYPE n,
        message        TYPE text100.
            INCLUDE TYPE ztpp017.
    TYPES: END OF ty_conf_adam .
    TYPES ty_menge TYPE zes_saida_alv_pint_op-quant .

    DATA:
      ct_components TYPE TABLE OF bapi_order_component .
    DATA:
      ct_header TYPE TABLE OF bapi_order_header1 .
    DATA ct_bapiret TYPE bapiret2_t .
    DATA gs_etiqueta TYPE ztpp029 .
    DATA:
      gt_itens_ref TYPE TABLE OF zes_itens_ref_etq .
    DATA:
      gt_modify013 TYPE TABLE OF ztpp013 .

    METHODS get_data_order
      IMPORTING
        VALUE(it_orders) TYPE tt_orders .
    METHODS estorno_retrabalho
      IMPORTING
        VALUE(is_ztpp013) TYPE ztpp013 .
    METHODS create_andom_qualidade .
    METHODS create_order
      IMPORTING
        VALUE(matnr) TYPE matnr
        VALUE(menge) TYPE ty_menge
        VALUE(data)  TYPE datum
      RETURNING
        VALUE(aufnr) TYPE aufnr .
    METHODS conf_apont_adam
      IMPORTING
        VALUE(is_conf_adam) TYPE ty_conf_adam
        VALUE(i_count)      TYPE i
        VALUE(i_acos)       TYPE char01 OPTIONAL .
    METHODS conf_etq
      IMPORTING
        VALUE(i_etq) TYPE zelmm_etiqueta .
    METHODS export_mem_rec
      IMPORTING
        VALUE(arbpl) TYPE zel_recurso .
    METHODS reimprimir_etq
      IMPORTING
        VALUE(i_etq) TYPE zelmm_etiqueta
      RETURNING
        VALUE(ok)    TYPE char01 .
    METHODS get_etiqueta_ref
      IMPORTING
        VALUE(i_etq) TYPE zelmm_etiqueta
      RETURNING
        VALUE(nok)   TYPE char01 .
    METHODS save_etq_print .
    METHODS save_ztpp018
      IMPORTING
        VALUE(is_ztpp018) TYPE ztpp018
      EXPORTING
        VALUE(message)    TYPE text100
      RETURNING
        VALUE(ok)         TYPE char01 .
    METHODS get_ordens_cp
      IMPORTING
        VALUE(i_arbpl)   TYPE arbpl
        VALUE(i_matnr)   TYPE matnr OPTIONAL
      EXPORTING
        VALUE(et_ordens) TYPE zttpp_op_ct .
    METHODS get_ordens
      IMPORTING
        VALUE(i_arbpl)   TYPE arbpl
        VALUE(i_matnr)   TYPE matnr OPTIONAL
      EXPORTING
        VALUE(et_ordens) TYPE zttpp_op_ct .
    METHODS get_status
      IMPORTING
        VALUE(id_grupo)  TYPE ztpp020-id_grupo OPTIONAL
      EXPORTING
        VALUE(et_status) TYPE tt_status .
    METHODS get_grp_status
      EXPORTING
        VALUE(et_grp_status) TYPE tt_grp_status .
    METHODS modify_recurso
      IMPORTING
        VALUE(is_ztpp018) TYPE ztpp018 .
    METHODS get_mard
      IMPORTING
        VALUE(it_matnr) TYPE tyr_matnr OPTIONAL
        VALUE(it_lgort) TYPE tyr_lgort OPTIONAL
        VALUE(it_werks) TYPE tyr_werks OPTIONAL
      EXPORTING
        VALUE(et_mard)  TYPE mard_tab .
    METHODS conf_apont
      IMPORTING
        VALUE(is_data) TYPE ztpp013 .
    METHODS conf_app_1_insp
      IMPORTING
        VALUE(it_parameter) TYPE /iwbep/t_mgw_name_value_pair .
    METHODS conf_app_mat
      IMPORTING
        VALUE(it_parameter) TYPE /iwbep/t_mgw_name_value_pair .
    METHODS get_recurso
      CHANGING
        !t_recursos TYPE tt_rec .
    METHODS get_number_operation
      IMPORTING
        VALUE(i_aufnr)          TYPE aufnr
        VALUE(i_arbpli)         TYPE ru_arbpli
        VALUE(i_checkap)        TYPE char01 OPTIONAL
      EXPORTING
        VALUE(operation_number) TYPE vornr
        VALUE(baixa_mat)        TYPE char01
        VALUE(apont_ok)         TYPE char01 .
    METHODS get_turno
      IMPORTING
        VALUE(i_arbpl)   TYPE arbpl
      EXPORTING
        VALUE(e_budat)   TYPE budat
        VALUE(e_schprog) TYPE schprog .
    METHODS apont_tp_1
      IMPORTING
        !lt_data TYPE tt_ztpp013
        !r_data  TYPE range_datum .
    METHODS apont_tp_2
      IMPORTING
        !lt_data TYPE tt_ztpp013
        !r_data  TYPE range_datum .
    METHODS apont_tp_3
      IMPORTING
        !lt_data TYPE tt_ztpp013
        !r_data  TYPE range_datum .
    METHODS apont_tp_estor
      IMPORTING
        !lt_data TYPE tt_ztpp013
        !r_data  TYPE range_datum .
    METHODS get_imseg
      RETURNING
        VALUE(t_imseg) TYPE ty_t_imseg .
    METHODS save_memory_etq_ref
      IMPORTING
        VALUE(is_ztpp013) TYPE ztpp013 .
    METHODS clear_imseg .
    METHODS create_excel_andom_day .
    METHODS create_andom_statusrec .
    METHODS save_mov
      IMPORTING
        VALUE(is_ztpp013) TYPE ztpp013
      RETURNING
        VALUE(idmov)      TYPE zidmovpcf .
    METHODS save_mov_man
      IMPORTING
        VALUE(is_ztpp013)  TYPE ztpp013
        VALUE(no_upd_time) TYPE char01 OPTIONAL
      RETURNING
        VALUE(idmov)       TYPE zidmovpcf .
    METHODS estorna_mov
      IMPORTING
        VALUE(is_ztpp013) TYPE ztpp013 .
    METHODS estorna_mov_man
      IMPORTING
        VALUE(is_ztpp013) TYPE ztpp013 .
    METHODS save_conf_tpp013
      IMPORTING
        VALUE(is_ztpp013) TYPE ztpp013 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF gc,
        firstday     TYPE char02           VALUE '01',
        bapi_lastday TYPE char100          VALUE 'LAST_DAY_OF_MONTHS',
      END OF gc .
    DATA dataf TYPE datum .
    DATA datai TYPE datum .
    DATA et_imseg TYPE ty_t_imseg .
    DATA:
      gt_etqs_ref TYPE TABLE OF ztpp029 .
    DATA gv_recursoold TYPE ru_arbpli .

    METHODS check_op
      IMPORTING
        VALUE(arbpl) TYPE arbpl
      CHANGING
        VALUE(aufnr) TYPE aufnr .
    METHODS print_etq .
    METHODS calcule_date
      IMPORTING
        VALUE(i_arbpl) TYPE arbpl .
    METHODS check_motivo
      IMPORTING
        VALUE(i_motivo) TYPE zel_id_status
      RETURNING
        VALUE(ok)       TYPE char01 .
    METHODS gerar_number_etq_ref
      RETURNING
        VALUE(number) TYPE zelmm_etiqueta .
    METHODS upd_ztpp013
      IMPORTING
        VALUE(zidmovpcf)   TYPE ztpp013-zidmovpcf
        VALUE(gjahr)       TYPE ztpp013-gjahr
        VALUE(ndoc_estorn) TYPE zidmovpcf .
    METHODS upd_ztpp013_mov
      CHANGING
        VALUE(cs_upd_013) TYPE ztpp013 .
ENDCLASS.



CLASS ZCL_PP_MES_ACOS IMPLEMENTATION.


  METHOD apont_tp_1.
    DATA: lt_header_conf        TYPE TABLE OF bapi_pp_hdrlevel,
          lt_goodsmovements     TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lt_link_conf_goodsmov TYPE STANDARD TABLE OF bapi_link_conf_goodsmov,
          lt_header_bapi        TYPE TABLE OF bapi_pp_hdrlevel,
          lt_return_detail      TYPE cocf_t_bapi_return,
          lt_components         TYPE TABLE OF ioopcomp,
          return                TYPE bapiret1,
          ls_error              TYPE ztpp013_3,
          l_mov                 TYPE char01.


    CLEAR: gt_modify013.
    DATA(lt_data_aux) = lt_data.

    DELETE lt_data_aux WHERE data_m <> r_data[ 1 ]-low .
    DELETE lt_data_aux WHERE tpmov <> 1.
    DELETE lt_data_aux WHERE estorno = abap_true.

    LOOP AT lt_data_aux INTO DATA(ls_data). "FROM l_index."WHERE tpmov = 1 AND estorno = abap_false."AND data_m IN r_data . " Apontamento de Entrada

      COLLECT VALUE bapi_pp_hdrlevel(
                    orderid    = ls_data-aufnr
                    postg_date = ls_data-data_m
                    yield      = ls_data-lmnga
                    scrap      = ls_data-rmnga
                    rework     = ls_data-xmnga
                    conf_text  = 'Apontamento aut. ADAM' ) INTO lt_header_conf.
      APPEND ls_data TO gt_modify013.
    ENDLOOP.
    LOOP AT lt_header_conf INTO DATA(ls_header_conf).
      CLEAR: lt_components,lt_header_bapi,lt_link_conf_goodsmov,lt_return_detail.

      lt_header_bapi = VALUE #( ( orderid = ls_header_conf-orderid ) ).
      SELECT SINGLE psmng, lgort,matnr,amein FROM afpo INTO @DATA(ls_afpo) WHERE aufnr = @ls_header_conf-orderid.
      SELECT SINGLE rsnum FROM afko INTO @DATA(l_rsnum)
          WHERE aufnr = @ls_header_conf-orderid.
      SELECT * FROM resb INTO CORRESPONDING FIELDS OF TABLE lt_components
         WHERE rsnum = l_rsnum.


      APPEND VALUE ioopcomp(
            matnr   = ls_afpo-matnr
            werks   = '1015'
            lgort   = ls_afpo-lgort
            bwart   = '101'
            erfme   = ls_afpo-amein
            erfmg   = ls_header_conf-yield
            ) TO lt_components.

      LOOP AT lt_components INTO DATA(ls_comp).
        CASE ls_comp-bwart.
          WHEN '101'.
            APPEND VALUE bapi2017_gm_item_create(
              material      = ls_comp-matnr
              plant         = ls_comp-werks
              stge_loc      = ls_comp-lgort
              batch         = ls_comp-charg
              move_type     = ls_comp-bwart
              entry_uom     = ls_comp-erfme
              orderid       = ls_header_conf-orderid
              entry_qnt     = ls_comp-erfmg
              mvt_ind       = 'F'
              ) TO lt_goodsmovements.
          WHEN OTHERS.
            ls_comp-erfmg = ( ls_comp-erfmg / ls_afpo-psmng ) * ls_header_conf-yield.
            APPEND VALUE bapi2017_gm_item_create(
            material      = ls_comp-matnr
            plant         = ls_comp-werks
            stge_loc      = ls_comp-lgort
            batch         = ls_comp-charg
            move_type     = ls_comp-bwart
            entry_uom     = ls_comp-erfme
            orderid       = ls_header_conf-orderid
            entry_qnt     = ls_comp-erfmg
            ) TO lt_goodsmovements.
        ENDCASE.
      ENDLOOP.
      CLEAR: ls_afpo,l_rsnum.

      DO lines( lt_goodsmovements ) TIMES.
        APPEND VALUE bapi_link_conf_goodsmov( index_confirm = 1 index_goodsmov = sy-index ) TO lt_link_conf_goodsmov.
      ENDDO.
      CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_HDR'
        IMPORTING
          return             = return
        TABLES
          athdrlevels        = lt_header_conf
          goodsmovements     = lt_goodsmovements
          link_conf_goodsmov = lt_link_conf_goodsmov
          detail_return      = lt_return_detail.
      IF NOT line_exists( lt_return_detail[ type = 'E' ] ).
        LOOP AT gt_modify013 ASSIGNING FIELD-SYMBOL(<ztpp013>) WHERE aufnr = ls_header_conf-orderid.
          <ztpp013>-mov = abap_true.
          <ztpp013>-conf = abap_true.
          <ztpp013>-rmzhl = lt_return_detail[ id = 'RU' number = 100 ]-conf_cnt.
          <ztpp013>-rueck = lt_return_detail[ id = 'RU' number = 100 ]-conf_no.
        ENDLOOP.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        SORT gt_modify013 BY aufnr.
        DELETE gt_modify013 WHERE aufnr = ls_header_conf-orderid.
      ENDIF.
    ENDLOOP.
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
      SELECT SINGLE matnr FROM zvs_afpo_afkko INTO @DATA(l_matnr)
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
               l_1_minut(02) TYPE n VALUE 10.

*    BREAK-POINT.
    SELECT SINGLE * FROM ztpp018_1 INTO @DATA(ls_confct)
      WHERE arbpl = @is_conf_adam-arbpl.
    " busca se a configuração do canal
    SELECT SINGLE * FROM ztpp017 INTO @DATA(ls_confadam)
      WHERE arbpl = @is_conf_adam-arbpl
        AND ch    = @is_conf_adam-ch.

    IF NOT i_count IS INITIAL. " Se existe algum apontamento de contagem
      IF ls_confct-aufnr IS INITIAL.
        SELECT SINGLE auto FROM ztpp021_a INTO @DATA(l_auto)
          WHERE id_status = @ls_confct-id_status_01.
        IF l_auto = abap_true.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021_a INTO @DATA(ls_status)
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
              INSERT ztpp013_3 FROM ls_ztpp013_3.
              COMMIT WORK AND WAIT.
              RETURN.
          ENDTRY.
        ELSE.
          SELECT SINGLE qtde FROM ztpp037 INTO @DATA(l_qtde) WHERE matnr = @ls_confct-matnr.
          IF sy-subrc = 0 AND NOT l_qtde IS INITIAL .
            i_count = i_count * l_qtde.
          ENDIF.
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
          INSERT ztpp013_3 FROM ls_ztpp013_3.
          COMMIT WORK AND WAIT.
          RETURN.
        ENDIF.
      ENDIF.
*      SELECT SINGLE setmat, pulso FROM ztpp021 INTO @DATA(ls_ztpp021)
*          WHERE id_status = @ls_confct-id_status_01.
      IF ls_confct-op1usin = icon_red_light.
        SELECT SINGLE qtde FROM ztpp037 INTO l_qtde WHERE matnr = ls_confct-matnr.
        IF sy-subrc = 0 AND NOT l_qtde IS INITIAL .
          i_count = i_count * l_qtde.
        ENDIF.
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
        SELECT SINGLE qtde FROM ztpp037 INTO l_qtde WHERE matnr = ls_confct-matnr.
        IF sy-subrc = 0 AND NOT l_qtde IS INITIAL .
          i_count = i_count * l_qtde.
        ENDIF.
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
        INSERT ztpp013_3 FROM ls_ztpp013_3.
        COMMIT WORK AND WAIT.
        RETURN.
      ENDIF.
      ls_ztpp013-aufnr = ls_confct-aufnr.
      SELECT SINGLE matnr FROM afpo INTO ls_ztpp013-matnr
      WHERE aufnr = ls_confct-aufnr.
      SELECT SINGLE qtde FROM ztpp037 INTO l_qtde WHERE matnr = ls_ztpp013-matnr.
      IF sy-subrc = 0 AND NOT l_qtde IS INITIAL .
        i_count = i_count * l_qtde.
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
*      IF ls_confct-op1usin = icon_led_green.
*        ls_ztpp013-aux02 = 'TRY-OUT'.
*      ENDIF.
      ls_ztpp013-aufnr = ls_confct-aufnr.
      ls_ztpp013-ru_arbpli = ls_confct-arbpl.
      ls_ztpp013-equnr     = ls_confct-equnr.
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
        IF l_timediff+2(02) > l_1_minut.
          MOVE-CORRESPONDING ls_confct TO ls_ztpp018.
          SELECT SINGLE * FROM ztpp021_a INTO ls_status
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


  METHOD conf_app_1_insp.
    DATA: ls_data     TYPE ztpp013,
          l_message   TYPE text255,
          l_num03(03) TYPE n.

    ls_data-ru_arbpli = it_parameter[ name = 'Recurso' ]-value.
    ls_data-matnr     = it_parameter[ name = 'Matnr' ]-value.


    get_ordens(
      EXPORTING
        i_arbpl   = ls_data-ru_arbpli    " Centro de trabalho
      IMPORTING
        et_ordens = DATA(lt_ordens)    " Tabela Para ZESPP_OP_CT
    ).

    IF lt_ordens IS INITIAL.
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
      ls_data-matnr     = ls_data-matnr(8) && 'USIN'.
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
          lo_mes_rodas        TYPE REF TO zcl_pp_mes_rodas,
          lt_range_ct         TYPE RANGE OF ztpp019_1a-recurso.

    CONSTANTS: lc_domain TYPE dd07l-domname VALUE 'ZDTPMOV'.
    DATA: lt_data  TYPE TABLE OF zes_andom_usin_ct_status,
          lt_data1 TYPE TABLE OF zes_andom_fund_ct_status.
    SELECT 'I' AS sign, 'EQ' AS option, recurso AS low FROM ztpp019_1a INTO CORRESPONDING FIELDS OF TABLE @lt_range_ct.

    SORT lt_range_ct BY low.
    DELETE ADJACENT DUPLICATES FROM lt_range_ct COMPARING low.

    SELECT * FROM ztpp018_1 INTO CORRESPONDING FIELDS OF TABLE lt_data WHERE arbpl IN lt_range_ct.
    IF sy-subrc <> 0.
      WRITE 'Não existem dados para seleção CSA'.
      RETURN.
    ENDIF.
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).
      SELECT SINGLE maktx FROM makt INTO <data>-maktx WHERE spras = sy-langu AND matnr = <data>-matnr.
      get_turno(
       EXPORTING
         i_arbpl   =  <data>-arbpl    " Centro de trabalho
       IMPORTING
         e_budat   = DATA(e_budat)    " Data de lançamento
*           e_schprog = e_schprog    " Chave da seqüência de turnos
     ).
      SELECT estorno,ndoc_estorn,schprog, SUM( lmnga ) AS lmnga FROM ztpp013
        INTO TABLE @DATA(lt_prodturno)
        WHERE ru_arbpli   = @<data>-arbpl
         " AND estorno     = @abap_false
         " AND ndoc_estorn = @space
         " AND aufnr     = ls_recurso-aufnr
          AND data_m     = @e_budat GROUP BY estorno,ndoc_estorn,schprog.
      SORT lt_prodturno BY estorno.
      DELETE lt_prodturno WHERE estorno = abap_true.
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

    lv_fullpath = lv_fullpath && 'STATUSREC/ANDOMCSA.TXT'.
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

        WRITE 'Dados foram atualizados CSA'.
      CATCH cx_root INTO lo_exception.
*
        DATA(lv_error) = lo_exception->get_text( ).
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
      WHERE data_m     = @sy-datum
        AND estorno    = @space
        AND ndoc_estorn = @space.
    IF sy-subrc <> 0.
      WRITE 'Não existem dados para seleção'.
      RETURN.
    ENDIF.
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
      APPEND aufnr TO lt_orders.
      WAIT UP TO 2 SECONDS.
      EXPORT no_avail_check_imp FROM abap_true TO MEMORY ID 'LIB_ORDER_PRD'.
      CALL FUNCTION 'BAPI_PRODORD_RELEASE'
        TABLES
          orders        = lt_orders
          detail_return = lt_return.

      IF NOT line_exists( lt_return[ id = 'CO' number = '093' ] ).
        CLEAR: aufnr.
        EXIT.
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
    SELECT * FROM ztpp013 INTO TABLE @DATA(lt_estorno) UP TO 100 ROWS
      WHERE data_m >= @datai
        AND data_m <= @is_ztpp013-data_m
        AND matnr   = @is_ztpp013-matnr
        AND estorno = @space
        AND tpmov   = 1
        AND equnr   = @is_ztpp013-equnr
        AND ndoc_estorn = @space ORDER BY data_m DESCENDING, datum DESCENDING , uzeit DESCENDING.
*    SORT lt_estorno BY data_m DESCENDING datum DESCENDING uzeit DESCENDING.

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
          upd_ztpp013( zidmovpcf = ls_ztpp013-zidmovpcf gjahr = ls_ztpp013-gjahr ndoc_estorn = l_num ).
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
          r_arbpl    TYPE RANGE OF ztpp013-ru_arbpli.

    ldatai = is_ztpp013-data_m(06) && '01'.
    CHECK is_ztpp013-ru_arbpli CP 'PINT*'.
    CASE is_ztpp013-ru_arbpli.

      WHEN 'PINT_001'.
        r_arbpl = VALUE #(
        ( sign = 'I' option = 'EQ' low = 'PINT_001' )
        ( sign = 'I' option = 'CP' low = 'PREP*' )
         ).

        SELECT * FROM ztpp013 INTO TABLE @DATA(lt_estorno)
          WHERE data_m >= @ldatai
            AND data_m <= @is_ztpp013-data_m
            AND ru_arbpli IN @r_arbpl
            AND matnr   = @is_ztpp013-matnr
            AND tpmov   = 1
            AND estorno = @space
            AND ndoc_estorn = @space
          ORDER BY ru_arbpli ASCENDING,datum DESCENDING ,uzeit DESCENDING.
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
      WHEN 'PINT_003'.
        r_arbpl = VALUE #(
        ( sign = 'I' option = 'EQ' low = 'PINT_003' )
        ( sign = 'I' option = 'CP' low = 'DIAM*' )
         ).
        l_quant = is_ztpp013-rmnga.
        SELECT * FROM ztpp013 INTO TABLE lt_estorno
           WHERE data_m >= ldatai
             AND data_m <= is_ztpp013-data_m
             AND matnr = is_ztpp013-matnr
             AND tpmov   = 1
             AND estorno = space
             AND ndoc_estorn = space
             AND ru_arbpli IN r_arbpl
           ORDER BY ru_arbpli ASCENDING datum DESCENDING uzeit DESCENDING .

        IF NOT lt_estorno IS INITIAL.
          LOOP AT lt_estorno INTO ls_estorno WHERE ru_arbpli = 'PINT_003'.
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
                 ).
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
    SELECT '@00@' AS selected, ztpp020_a~* FROM ztpp020_a INTO CORRESPONDING FIELDS OF TABLE @et_grp_status WHERE id_grupo >= 2.
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

    CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
      EXPORTING
        number        = i_aufnr
        order_objects = ls_order_objects
      TABLES
        operation     = lt_operations
        component     = lt_components.
    TRY.
        operation_number = lt_operations[ work_center = i_arbpli ]-operation_number.
      CATCH cx_sy_itab_line_not_found.
        DATA(l_arbpl) = i_arbpli(04) && '*'.
        LOOP AT lt_operations INTO DATA(ls_op) WHERE work_center CP l_arbpl.
          operation_number = ls_op-operation_number.
          EXIT.
        ENDLOOP.
        IF operation_number IS INITIAL.
          operation_number = lt_operations[ work_center = i_arbpli ]-operation_number.
        ENDIF.
    ENDTRY.
    IF line_exists( lt_components[ operation = operation_number  ] ) OR
       line_exists( lt_operations[ opr_cntrl_key = 'YBP3' operation_number = operation_number ] ).
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
    SELECT '@00@' AS selected, ztpp019_1a~recurso AS arbpl , ztpp019_1a~descrec AS ktext FROM ztpp019_a
    INNER JOIN ztpp019_1a ON ztpp019_1a~grupoprod = ztpp019_a~grupoprod
    INNER JOIN crhd_v1 ON crhd_v1~arbpl = ztpp019_1a~recurso
    INTO CORRESPONDING FIELDS OF TABLE @t_recursos
    WHERE ztpp019_a~uname   = @sy-uname
      AND ztpp019_1a~tcode = @sy-tcode.

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
      SELECT '@00@' AS selected, ztpp021_a~* FROM ztpp021_a INTO CORRESPONDING FIELDS OF TABLE @et_status WHERE id_grupo > 2.
    ELSE.
      SELECT '@00@' AS selected, ztpp021_a~* FROM ztpp021_a INTO CORRESPONDING FIELDS OF TABLE @et_status WHERE id_grupo = @id_grupo.
    ENDIF.
  ENDMETHOD.


  METHOD get_turno.
    SELECT SINGLE code FROM ztpp028_2a
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
      FROM ztpp028_1a INTO TABLE @DATA(lt_turno)
      WHERE code = @l_code
        AND eshift_start <= @sy-uzeit.
    IF sy-subrc = 0.
      SORT lt_turno BY eshift_start DESCENDING.
      DATA(ls_turno) = lt_turno[ 1 ].
    ELSE.
      SELECT code,
           schprog,
           eshift_start,
           eshift_end,
           next_day
      FROM ztpp028_1a INTO CORRESPONDING FIELDS OF TABLE @lt_turno
      WHERE code = @l_code
        AND eshift_start >= @sy-uzeit.
      SORT lt_turno BY eshift_end ASCENDING.
      TRY.
          ls_turno = lt_turno[ 1 ].
        CATCH cx_sy_itab_line_not_found.
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
  ENDMETHOD.


  METHOD modify_recurso.
    DATA: ls_save TYPE ztpp018_1.

    MOVE-CORRESPONDING is_ztpp018 TO ls_save.

    MODIFY ztpp018_1 FROM ls_save.
  ENDMETHOD.


  METHOD print_etq.
    CALL FUNCTION 'ZFPP_PRINT_ETQ_REFUGO_RD'
      EXPORTING
        is_etiqueta = gs_etiqueta.
  ENDMETHOD.


  METHOD reimprimir_etq.
    SELECT SINGLE * FROM ztpp029 INTO gs_etiqueta
      WHERE idetiqueta = i_etq.
    IF sy-subrc = 0.
      print_etq( ).
      MESSAGE i000(zpp) WITH 1 'Etiqueta(s) enviada(s) para impressão'.
      ok = abap_true.
      gs_etiqueta-contagem = gs_etiqueta-contagem + 1.
      UPDATE ztpp029 SET contagem = gs_etiqueta-contagem WHERE idetiqueta = i_etq.
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

    is_ztpp013-zidmovpcf = l_range.
    MOVE-CORRESPONDING is_ztpp013 TO ls_conf4.

    INSERT ztpp013_4 FROM ls_conf4.
  ENDMETHOD.


  METHOD save_etq_print.
    LOOP AT gt_etqs_ref ASSIGNING FIELD-SYMBOL(<etq>).
      <etq>-contagem = 1.
    ENDLOOP.
    INSERT ztpp029 FROM TABLE gt_etqs_ref.
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
    IF is_ztpp013-ru_arbpli(04) = 'USIN'.
      gs_etiqueta-local_ct = 'USINAGEM'.
    ELSE.
      gs_etiqueta-local_ct   = is_ztpp013-ru_arbpli && | | && '-' && | | && l_desc .
    ENDIF.
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
    IF NOT is_ztpp018-arbpl IS INITIAL.
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
    SELECT SINGLE * FROM ztpp013 INTO @DATA(ls_upd_013)
                    WHERE zidmovpcf   = @zidmovpcf
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
