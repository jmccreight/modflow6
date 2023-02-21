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
    logical :: iprflow = .false.
    logical :: obs_filerecord = .false.
    logical :: obs6 = .false.
    logical :: obs6_filename = .false.
    logical :: iseg_order = .false.
    logical :: qoutflow0 = .false.
    logical :: k_coef = .false.
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
    snfmmr_iprflow = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'PRINT_FLOWS', & ! tag name
    'IPRFLOW', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_obs_filerecord = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'OBS_FILERECORD', & ! tag name
    'OBS_FILERECORD', & ! fortran variable
    'RECORD OBS6 FILEIN OBS6_FILENAME', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_obs6 = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6', & ! tag name
    'OBS6', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_obs6_filename = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'OPTIONS', & ! block
    'OBS6_FILENAME', & ! tag name
    'OBS6_FILENAME', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
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
    snfmmr_qoutflow0 = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'QOUTFLOW0', & ! tag name
    'QOUTFLOW0', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfmmr_k_coef = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'MMR', & ! subcomponent
    'GRIDDATA', & ! block
    'K_COEF', & ! tag name
    'K_COEF', & ! fortran variable
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
    snfmmr_iprflow, &
    snfmmr_obs_filerecord, &
    snfmmr_obs6, &
    snfmmr_obs6_filename, &
    snfmmr_iseg_order, &
    snfmmr_qoutflow0, &
    snfmmr_k_coef, &
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
