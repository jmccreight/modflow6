module SnfMmrInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public snf_mmr_param_definitions
  public snf_mmr_aggregate_definitions
  public snf_mmr_block_definitions
  public SnfMmrParamFoundType

  type SnfMmrParamFoundType
    logical :: ipakcb = .false.
    logical :: iseg_order = .false.
    logical :: mann_n = .false.
    logical :: seg_depth = .false.
    logical :: seg_slope = .false.
    logical :: x_coef = .false.
  end type SnfMmrParamFoundType

  type(InputParamDefinitionType), parameter :: &
    snfmmr_ipakcb = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'SAVE_FLOWS', & ! tag name
    'IPAKCB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_iseg_order = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'ISEG_ORDER', & ! tag name
    'ISEG_ORDER', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_mann_n = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'MANN_N', & ! tag name
    'MANN_N', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_seg_depth = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'SEG_DEPTH', & ! tag name
    'SEG_DEPTH', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_seg_slope = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'SEG_SLOPE', & ! tag name
    'SEG_SLOPE', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_x_coef = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'X_COEF', & ! tag name
    'X_COEF', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snf_mmr_param_definitions(*) = &
    [ &
    snfmmr_ipakcb, &
    snfmmr_iseg_order, &
    snfmmr_mann_n, &
    snfmmr_seg_depth, &
    snfmmr_seg_slope, &
    snfmmr_x_coef &
    ]

  type(InputParamDefinitionType), parameter :: &
    snf_mmr_aggregate_definitions(*) = &
    [ &
    InputParamDefinitionType :: &
    ]

  type(InputBlockDefinitionType), parameter :: &
    snf_mmr_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ) &
    ]

end module SnfMmrInputModule
