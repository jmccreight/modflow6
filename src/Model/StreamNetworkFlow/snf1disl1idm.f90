module SnfDislInputModule
  use InputDefinitionModule, only: InputParamDefinitionType, &
                                   InputBlockDefinitionType
  private
  public snf_disl_param_definitions
  public snf_disl_aggregate_definitions
  public snf_disl_block_definitions
  public SnfDislParamFoundType

  type SnfDislParamFoundType
    logical :: length_units = .false.
    logical :: length_convert = .false.
    logical :: time_convert = .false.
    logical :: nogrb = .false.
    logical :: xorigin = .false.
    logical :: yorigin = .false.
    logical :: angrot = .false.
    logical :: nodes = .false.
    logical :: nvert = .false.
    logical :: segment_length = .false.
    logical :: tosegment = .false.
    logical :: idomain = .false.
    logical :: iv = .false.
    logical :: xv = .false.
    logical :: yv = .false.
    logical :: zv = .false.
    logical :: icell2d = .false.
    logical :: fdc = .false.
    logical :: ncvert = .false.
    logical :: icvert = .false.
  end type SnfDislParamFoundType

  type(InputParamDefinitionType), parameter :: &
    snfdisl_length_units = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_UNITS', & ! tag name
    'LENGTH_UNITS', & ! fortran variable
    'STRING', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_length_convert = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'LENGTH_CONVERT', & ! tag name
    'LENGTH_CONVERT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_time_convert = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'TIME_CONVERT', & ! tag name
    'TIME_CONVERT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_nogrb = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'NOGRB', & ! tag name
    'NOGRB', & ! fortran variable
    'KEYWORD', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_xorigin = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'XORIGIN', & ! tag name
    'XORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_yorigin = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'YORIGIN', & ! tag name
    'YORIGIN', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_angrot = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'OPTIONS', & ! block
    'ANGROT', & ! tag name
    'ANGROT', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_nodes = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'DIMENSIONS', & ! block
    'NODES', & ! tag name
    'NODES', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_nvert = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'DIMENSIONS', & ! block
    'NVERT', & ! tag name
    'NVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_segment_length = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'GRIDDATA', & ! block
    'SEGMENT_LENGTH', & ! tag name
    'SEGMENT_LENGTH', & ! fortran variable
    'DOUBLE1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_tosegment = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'GRIDDATA', & ! block
    'TOSEGMENT', & ! tag name
    'TOSEGMENT', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_idomain = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'GRIDDATA', & ! block
    'IDOMAIN', & ! tag name
    'IDOMAIN', & ! fortran variable
    'INTEGER1D', & ! type
    'NODES', & ! shape
    .false., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_iv = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'IV', & ! tag name
    'IV', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_xv = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'XV', & ! tag name
    'XV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_yv = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'YV', & ! tag name
    'YV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_zv = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'ZV', & ! tag name
    'ZV', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_icell2d = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'ICELL2D', & ! tag name
    'ICELL2D', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_fdc = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'FDC', & ! tag name
    'FDC', & ! fortran variable
    'DOUBLE', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_ncvert = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'NCVERT', & ! tag name
    'NCVERT', & ! fortran variable
    'INTEGER', & ! type
    '', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_icvert = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'ICVERT', & ! tag name
    'ICVERT', & ! fortran variable
    'INTEGER1D', & ! type
    'NCVERT', & ! shape
    .true., & ! required
    .true., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snf_disl_param_definitions(*) = &
    [ &
    snfdisl_length_units, &
    snfdisl_length_convert, &
    snfdisl_time_convert, &
    snfdisl_nogrb, &
    snfdisl_xorigin, &
    snfdisl_yorigin, &
    snfdisl_angrot, &
    snfdisl_nodes, &
    snfdisl_nvert, &
    snfdisl_segment_length, &
    snfdisl_tosegment, &
    snfdisl_idomain, &
    snfdisl_iv, &
    snfdisl_xv, &
    snfdisl_yv, &
    snfdisl_zv, &
    snfdisl_icell2d, &
    snfdisl_fdc, &
    snfdisl_ncvert, &
    snfdisl_icvert &
    ]

  type(InputParamDefinitionType), parameter :: &
    snfdisl_vertices = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'VERTICES', & ! block
    'VERTICES', & ! tag name
    'VERTICES', & ! fortran variable
    'RECARRAY IV XV YV ZV', & ! type
    'NVERT', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snfdisl_cell2d = InputParamDefinitionType &
    ( &
    'SNF', & ! component
    'DISL', & ! subcomponent
    'CELL2D', & ! block
    'CELL2D', & ! tag name
    'CELL2D', & ! fortran variable
    'RECARRAY ICELL2D FDC NCVERT ICVERT', & ! type
    'NODES', & ! shape
    .true., & ! required
    .false., & ! multi-record
    .false., & ! preserve case
    .false. & ! layered
    )

  type(InputParamDefinitionType), parameter :: &
    snf_disl_aggregate_definitions(*) = &
    [ &
    snfdisl_vertices, &
    snfdisl_cell2d &
    ]

  type(InputBlockDefinitionType), parameter :: &
    snf_disl_block_definitions(*) = &
    [ &
    InputBlockDefinitionType( &
    'OPTIONS', & ! blockname
    .false., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'DIMENSIONS', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'GRIDDATA', & ! blockname
    .true., & ! required
    .false. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'VERTICES', & ! blockname
    .true., & ! required
    .true. & ! aggregate
    ), &
    InputBlockDefinitionType( &
    'CELL2D', & ! blockname
    .true., & ! required
    .true. & ! aggregate
    ) &
    ]

end module SnfDislInputModule
