*&---------------------------------------------------------------------*
*& Report zabaplint_action_example01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabaplint_action_example01.

CLASS lcl_intcode_program DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES t_input TYPE TABLE OF i WITH EMPTY KEY.

    DATA mt_input TYPE t_input.
    DATA mv_input TYPE string.

    METHODS constructor
      IMPORTING iv_input TYPE string.
    METHODS run
      RETURNING
        VALUE(rv_output) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_intcode_program IMPLEMENTATION.
  METHOD constructor.
    mv_input = iv_input.
  ENDMETHOD.

  METHOD run.
    DATA: lv_input LIKE LINE OF mt_input.
    SPLIT mv_input AT ',' INTO TABLE DATA(lt_input).

    mt_input = lt_input.

    LOOP AT mt_input INTO lv_input.
      DATA(lv_i) = sy-tabix.

      IF ( ( lv_i - 1 ) MOD 4 ) <> 0.
        CONTINUE.
      ENDIF.

      CASE lv_input.
        WHEN 1.
          mt_input[ mt_input[ lv_i + 3 ] + 1 ] =  mt_input[ mt_input[ lv_i + 1 ] + 1 ]
            + mt_input[ mt_input[ lv_i + 2 ] + 1 ].
        WHEN 2.
          mt_input[ mt_input[ lv_i + 3 ] + 1 ] =  mt_input[ mt_input[ lv_i + 1 ] + 1 ]
            * mt_input[ mt_input[ lv_i + 2 ] + 1 ].
        WHEN 99.
          EXIT.
      ENDCASE.


    ENDLOOP.

    rv_output = REDUCE #( INIT x = `` FOR wa IN mt_input
                    NEXT x = |{ x }{ wa },| ).

    rv_output = substring( val = rv_output off = 0 len = strlen( rv_output ) - 1 ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_intcode_program DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_01 FOR TESTING RAISING cx_static_check,
      test_02 FOR TESTING RAISING cx_static_check,
      test_03 FOR TESTING RAISING cx_static_check,
      test_04 FOR TESTING RAISING cx_static_check,
      test_05 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_intcode_program IMPLEMENTATION.

  METHOD test_01.
    DATA(lv_input) = `1,0,0,0,99`.
    DATA(lo_cut) = NEW lcl_intcode_program( iv_input = lv_input ).
    cl_abap_unit_assert=>assert_equals( exp = `2,0,0,0,99` act = lo_cut->run( ) ).
  ENDMETHOD.
  METHOD test_02.
    DATA(lv_input) = `2,3,0,3,99`.
    DATA(lo_cut) = NEW lcl_intcode_program( iv_input = lv_input ).
    cl_abap_unit_assert=>assert_equals( exp = `2,3,0,6,99` act = lo_cut->run( ) ).
  ENDMETHOD.

  METHOD test_03.
    DATA(lv_input) = `2,4,4,5,99,0`.
    DATA(lo_cut) = NEW lcl_intcode_program( iv_input = lv_input ).
    cl_abap_unit_assert=>assert_equals( exp = `2,4,4,5,99,9801` act = lo_cut->run( ) ).
  ENDMETHOD.

  METHOD test_04.
    DATA(lv_input) = `1,1,1,4,99,5,6,0,99`.
    DATA(lo_cut) = NEW lcl_intcode_program( iv_input = lv_input ).
    cl_abap_unit_assert=>assert_equals( exp = `30,1,1,4,2,5,6,0,99` act = lo_cut->run( ) ).
  ENDMETHOD.

  METHOD test_05.
    DATA(lv_input) = `1,9,10,3,2,3,11,0,99,30,40,50`.
    DATA(lo_cut) = NEW lcl_intcode_program( iv_input = lv_input ).
    cl_abap_unit_assert=>assert_equals( exp = `3500,9,10,70,2,3,11,0,99,30,40,50` act = lo_cut->run( ) ).

  ENDMETHOD.

ENDCLASS.

PARAMETERS: p_input TYPE string.

START-OF-SELECTION.

  DATA(intcode_program) = NEW lcl_intcode_program( iv_input = p_input ).

  cl_demo_output=>display( intcode_program->run( ) ).
