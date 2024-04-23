class ZCL_ZGS_PP_APONT_PINT_DPC_EXT definition
  public
  inheriting from ZCL_ZGS_PP_APONT_PINT_DPC
  create public .

public section.

  methods RAISE_EXCEPTION_IWBEP_BUSINESS
    importing
      !IT_BAPI_MSG type BAPIRET2_T
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
protected section.

  methods GETMOTIVOSSET_GET_ENTITY
    redefinition .
  methods GETMOTIVOSSET_GET_ENTITYSET
    redefinition .
  methods GETRECURSOSS_GET_ENTITY
    redefinition .
  methods GETRECURSOSS_GET_ENTITYSET
    redefinition .
  methods MATERIALS_GET_ENTITYSET
    redefinition .
  methods CHANGEMATS_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZGS_PP_APONT_PINT_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
*SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION(
**  EXPORTING
**    iv_action_name          = iv_action_name
**    it_parameter            = it_parameter
**    io_tech_request_context = io_tech_request_context
**  IMPORTING
**    er_data                 = er_data
*       ).
    DATA: lo_mes TYPE  REF TO zcl_pp_mes_rodas.

    FIELD-SYMBOLS:
    <ls_result> TYPE cl_epm_ref_apps_po_apv_mpc_ext=>functionimportresult.

    CREATE OBJECT lo_mes.
    CASE iv_action_name.
      WHEN 'ConfApont'.
        lo_mes->conf_app_1_insp( it_parameter = it_parameter ).
        IF lo_mes->ct_bapiret IS INITIAL.
          CREATE DATA er_data LIKE <ls_result>.
          ASSIGN er_data->* TO <ls_result>.
          <ls_result>-success = abap_true.
        ELSE.
          raise_exception_iwbep_business(
              it_bapi_msg = lo_mes->ct_bapiret
          ).
        ENDIF.
      WHEN 'ConfMAt'.
        lo_mes->conf_app_mat( it_parameter = it_parameter ).
        IF lo_mes->ct_bapiret IS INITIAL.
          CREATE DATA er_data LIKE <ls_result>.
          ASSIGN er_data->* TO <ls_result>.
          <ls_result>-success = abap_true.
        ELSE.
          raise_exception_iwbep_business(
              it_bapi_msg = lo_mes->ct_bapiret
          ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD changemats_get_entityset.
*SUPER->CHANGEMATS_GET_ENTITYSET(
*  EXPORTING
*    IV_ENTITY_NAME           = IV_ENTITY_NAME
*    IV_ENTITY_SET_NAME       = IV_ENTITY_SET_NAME
*    IV_SOURCE_NAME           = IV_SOURCE_NAME
*    IT_FILTER_SELECT_OPTIONS = IT_FILTER_SELECT_OPTIONS
*    IS_PAGING                = IS_PAGING
*    IT_KEY_TAB               = IT_KEY_TAB
*    IT_NAVIGATION_PATH       = IT_NAVIGATION_PATH
*    IT_ORDER                 = IT_ORDER
*    IV_FILTER_STRING         = IV_FILTER_STRING
*    IV_SEARCH_STRING         = IV_SEARCH_STRING
**    io_tech_request_context  = io_tech_request_context
**  IMPORTING
**    et_entityset             = et_entityset
**    es_response_context      = es_response_context
*       ).

    DATA: lr_matnr TYPE /iwbep/t_cod_select_options..

    lr_matnr = VALUE #(
    ( sign = 'E' option = 'CP' low = '*KIT' )
    ( sign = 'E' option = 'CP' low = '*INJE*' )
    ( sign = 'E' option = 'CP' low = '*BRUT*' )
    ( sign = 'E' option = 'CP' low = '*TRAT*' )
    ( sign = 'E' option = 'CP' low = '*USIN*' )
    ( sign = 'E' option = 'CP' low = '*PL' )
    ( sign = 'E' option = 'CP' low = '*CX' )
    ( sign = 'E' option = 'CP' low = '*CP' )
    ( sign = 'E' option = 'CP' low = 'SER*' )
    ( sign = 'E' option = 'CP' low = 'SE0*' )

     ).

    IF NOT it_filter_select_options IS INITIAL.
      DATA(ls_filter) = it_filter_select_options[ 1 ].
      APPEND ls_filter-select_options[ 1 ] TO lr_matnr.
    ENDIF.

    SELECT makt~matnr, makt~maktx FROM mara
      INNER JOIN marc ON marc~matnr = mara~matnr
      INNER JOIN makt ON makt~matnr = mara~matnr
      INTO CORRESPONDING FIELDS OF TABLE @et_entityset
      WHERE makt~spras = @sy-langu
        AND mara~matnr IN @lr_matnr
        AND mara~mtart = 'HALB'
        AND marc~dispo = '100'.
  ENDMETHOD.


  METHOD getmotivosset_get_entity.
*SUPER->GETMOTIVOSSET_GET_ENTITY(
*  EXPORTING
*    IV_ENTITY_NAME          = IV_ENTITY_NAME
*    IV_ENTITY_SET_NAME      = IV_ENTITY_SET_NAME
*    IV_SOURCE_NAME          = IV_SOURCE_NAME
*    IT_KEY_TAB              = IT_KEY_TAB
**    io_request_object       = io_request_object
**    io_tech_request_context = io_tech_request_context
*    IT_NAVIGATION_PATH      = IT_NAVIGATION_PATH
**  IMPORTING
**    er_entity               = er_entity
**    es_response_context     = es_response_context
*       ).

    DATA(l_motivo) = it_key_tab[ 1 ]-value.
    SELECT SINGLE id_status, rec_status FROM ztpp023 INTO CORRESPONDING FIELDS OF @er_entity WHERE id_status = @l_motivo.

  ENDMETHOD.


  METHOD getmotivosset_get_entityset.
*SUPER->GETMOTIVOSSET_GET_ENTITYSET(
*  EXPORTING
*    IV_ENTITY_NAME           = IV_ENTITY_NAME
*    IV_ENTITY_SET_NAME       = IV_ENTITY_SET_NAME
*    IV_SOURCE_NAME           = IV_SOURCE_NAME
*    IT_FILTER_SELECT_OPTIONS = IT_FILTER_SELECT_OPTIONS
*    IS_PAGING                = IS_PAGING
*    IT_KEY_TAB               = IT_KEY_TAB
*    IT_NAVIGATION_PATH       = IT_NAVIGATION_PATH
*    IT_ORDER                 = IT_ORDER
*    IV_FILTER_STRING         = IV_FILTER_STRING
*    IV_SEARCH_STRING         = IV_SEARCH_STRING
**    io_tech_request_context  = io_tech_request_context
**  IMPORTING
**    et_entityset             = et_entityset
**    es_response_context      = es_response_context
*       ).

    SELECT id_status, rec_status FROM ztpp023 INTO CORRESPONDING FIELDS OF TABLE @et_entityset.
  ENDMETHOD.


  METHOD getrecursoss_get_entity.
*SUPER->GETRECURSOSS_GET_ENTITY(
*  EXPORTING
*    IV_ENTITY_NAME          = IV_ENTITY_NAME
*    IV_ENTITY_SET_NAME      = IV_ENTITY_SET_NAME
*    IV_SOURCE_NAME          = IV_SOURCE_NAME
*    IT_KEY_TAB              = IT_KEY_TAB
**    io_request_object       = io_request_object
**    io_tech_request_context = io_tech_request_context
*    IT_NAVIGATION_PATH      = IT_NAVIGATION_PATH
**  IMPORTING
**    er_entity               = er_entity
**    es_response_context     = es_response_context
*       ).
    DATA(l_recurso) = it_key_tab[ 1 ]-value.
    SELECT SINGLE ztpp019_1~* FROM ztpp019
  INNER JOIN ztpp019_1
  ON ztpp019_1~grupoprod = ztpp019~grupoprod
  INTO CORRESPONDING FIELDS OF @er_entity
  WHERE ztpp019~uname   = @sy-uname
    AND ztpp019_1~tcode = 'ZPP059'
    AND ztpp019_1~recurso = @l_recurso.

  ENDMETHOD.


  METHOD getrecursoss_get_entityset.
    SELECT ztpp019_1~* FROM ztpp019
      INNER JOIN ztpp019_1
      ON ztpp019_1~grupoprod = ztpp019~grupoprod
      INTO CORRESPONDING FIELDS OF TABLE @et_entityset
      WHERE ztpp019~uname   = @sy-uname
        AND ztpp019_1~tcode = 'ZPP059'.
  ENDMETHOD.


  METHOD materials_get_entityset.
*SUPER->MATERIALS_GET_ENTITYSET(
*  EXPORTING
*    IV_ENTITY_NAME           = IV_ENTITY_NAME
*    IV_ENTITY_SET_NAME       = IV_ENTITY_SET_NAME
*    IV_SOURCE_NAME           = IV_SOURCE_NAME
*    IT_FILTER_SELECT_OPTIONS = IT_FILTER_SELECT_OPTIONS
*    IS_PAGING                = IS_PAGING
*    IT_KEY_TAB               = IT_KEY_TAB
*    IT_NAVIGATION_PATH       = IT_NAVIGATION_PATH
*    IT_ORDER                 = IT_ORDER
*    IV_FILTER_STRING         = IV_FILTER_STRING
*    IV_SEARCH_STRING         = IV_SEARCH_STRING
**    io_tech_request_context  = io_tech_request_context
**  IMPORTING
**    et_entityset             = et_entityset
**    es_response_context      = es_response_context
*       ).
    SELECT matnr FROM ztpp026 INTO TABLE et_entityset.
    SORT et_entityset BY matnr.
  ENDMETHOD.


    METHOD raise_exception_iwbep_business.

      DATA:
        lo_msg_container TYPE REF TO /iwbep/if_message_container.

* Check that we have something to raise
      IF   it_bapi_msg     IS INITIAL.
        RETURN.
      ENDIF.
      lo_msg_container = mo_context->get_message_container( ).
* Handle non-leading messages in BAPI return message format
      IF it_bapi_msg IS NOT INITIAL.
        CALL METHOD lo_msg_container->add_messages_from_bapi
          EXPORTING
            it_bapi_messages         = it_bapi_msg
            iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-none.
      ENDIF.



* Now raise the exception
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg_container.


    ENDMETHOD.
ENDCLASS.
