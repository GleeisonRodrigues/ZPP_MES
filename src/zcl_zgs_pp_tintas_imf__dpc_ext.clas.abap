class ZCL_ZGS_PP_TINTAS_IMF__DPC_EXT definition
  public
  inheriting from ZCL_ZGS_PP_TINTAS_IMF__DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.

  methods DATASET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZGS_PP_TINTAS_IMF__DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.
*SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY(
*  EXPORTING
**    iv_entity_name          = iv_entity_name
**    iv_entity_set_name      = iv_entity_set_name
**    iv_source_name          = iv_source_name
*    IO_DATA_PROVIDER        = IO_DATA_PROVIDER
**    it_key_tab              = it_key_tab
**    it_navigation_path      = it_navigation_path
**    io_tech_request_context = io_tech_request_context
**  IMPORTING
**    er_entity               = er_entity
*       ).
    DATA: ls_save TYPE ztpp046.
    io_data_provider->read_entry_data(
      IMPORTING
        es_data                      = ls_save
    ).

    ls_save-mandt = sy-mandt.
    MODIFY ztpp046 FROM ls_save.
*      CATCH /iwbep/cx_mgw_tech_exception.    "
  ENDMETHOD.


  METHOD dataset_get_entityset.
*SUPER->DATASET_GET_ENTITYSET(
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

    SELECT * FROM ztpp046 INTO TABLE et_entityset.
  ENDMETHOD.
ENDCLASS.
