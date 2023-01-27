module SnfDislModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME, DZERO, DONE, LINELENGTH
  use SimVariablesModule, only: errmsg, warnmsg
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_allocate
  use SimModule, only: count_errors, store_error, store_error_unit, &
                       store_warning
  use BaseDisModule, only: DisBaseType
  use DislGeom, only: calcdist, partialdist

  implicit none

  private
  public :: disl_cr

  type, extends(DisBaseType) :: SnfDislType  
    integer(I4B), pointer :: nvert => null()                                     !< number of x,y vertices
    real(DP), pointer :: convlength => null()                                    !< conversion factor for length
    real(DP), pointer :: convtime => null()                                      !< conversion factor for time  
    real(DP), dimension(:), pointer, contiguous :: segment_length => null()      !< length of each segment
    integer(I4B), dimension(:), pointer, contiguous :: tosegment => null()       !< downstream segment index (nodes)
    integer(I4B), dimension(:), pointer, contiguous :: idomain => null()         !< idomain (nodes)
    real(DP), dimension(:, :), pointer, contiguous :: vertices => null()         !< cell vertices stored as 2d array with columns of x, y, and z
    real(DP), dimension(:, :), pointer, contiguous :: cellxyz => null()          !< segment midpoints stored as 2d array with columns of x, y, and z
    real(DP), dimension(:), pointer, contiguous :: fdc => null()                 !< fdc stored as array
    integer(I4B), dimension(:), pointer, contiguous :: iavert => null()          !< cell vertex pointer ia array
    integer(I4B), dimension(:), pointer, contiguous :: javert => null()          !< cell vertex pointer ja array
  contains
    procedure :: disl_load
    procedure :: dis_da => disl_da
    ! -- private
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: source_options
    procedure :: source_dimensions
    procedure :: source_griddata
    procedure :: source_vertices
    procedure :: source_cell2d
    procedure :: log_options
    procedure :: log_dimensions
    procedure :: log_griddata
    procedure :: define_cellverts
    procedure :: grid_finalize
    !procedure :: connect
    procedure :: create_connections
    procedure :: write_grb
    procedure :: get_nodenumber_idx1
    procedure :: nodeu_to_string

  end type SnfDislType 

contains

  !> @brief Create new disl package
  !<
  subroutine disl_cr(dis, name_model, inunit, iout)
    use IdmMf6FileLoaderModule, only: input_load
    class(DisBaseType), pointer :: dis
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    type(SnfDislType), pointer :: disnew
    character(len=*), parameter :: fmtheader = &
      "(1X, /1X, 'DISL -- LINE NETWORK DISCRETIZATION PACKAGE,', &
      &' VERSION 1 : 1/20/2023 - INPUT READ FROM UNIT ', I0, //)"
    allocate (disnew)
    dis => disnew
    call disnew%allocate_scalars(name_model)
    dis%inunit = inunit
    dis%iout = iout
    !
    ! -- if reading from file
    if (inunit > 0) then
      !
      ! -- Identify package
      if (iout > 0) then
        write (iout, fmtheader) inunit
      end if
      !
      ! -- load disl
      call disnew%disl_load()
    end if
    !
    ! -- Return
    return
  end subroutine disl_cr
  
  !> @brief Allocate scalar variables
  !<
  subroutine allocate_scalars(this, name_model)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule,   only: DONE
    ! -- dummy
    class(SnfDislType) :: this
    character(len=*), intent(in) :: name_model
    !
    ! -- Allocate parent scalars
    call this%DisBaseType%allocate_scalars(name_model)
    !
    ! -- Allocate
    call mem_allocate(this%nvert, 'NVERT', this%memoryPath)
    call mem_allocate(this%convlength, 'CONVLENGTH', this%memoryPath)
    call mem_allocate(this%convtime, 'CONVTIME', this%memoryPath)
    !
    ! -- Initialize
    this%nvert = 0
    this%ndim = 1
    this%convlength = DONE
    this%convtime = DONE
    !
    ! -- Return
    return
  end subroutine allocate_scalars
    
  subroutine disl_load(this)
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    !
    ! -- source input data
    call this%source_options()
    call this%source_dimensions()
    call this%source_griddata()

    ! create connectivity using tosegment
    call this%create_connections()

    ! If vertices provided by user, read and store vertices
    if (this%nvert > 0) then
      call this%source_vertices()
      call this%source_cell2d()
    end if

    ! finalize the grid
    call this%grid_finalize()
    !
    ! -- Return
    return
  end subroutine disl_load

  !> @brief Copy options from IDM into package
  !<
  subroutine source_options(this)
    ! -- modules
    use KindModule, only: LGP
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SnfDislInputModule, only: SnfDislParamFoundType
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    character(len=LENVARNAME), dimension(3) :: lenunits = &
      &[character(len=LENVARNAME) :: 'FEET', 'METERS', 'CENTIMETERS']
    type(SnfDislParamFoundType) :: found
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISL', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%lenuni, 'LENGTH_UNITS', idmMemoryPath, lenunits, &
                        found%length_units)
    call mem_set_value(this%convlength, 'CONVLENGTH', idmMemoryPath, found%length_convert)
    call mem_set_value(this%convtime, 'CONVTIME', idmMemoryPath, found%time_convert)
    call mem_set_value(this%nogrb, 'NOGRB', idmMemoryPath, found%nogrb)
    call mem_set_value(this%xorigin, 'XORIGIN', idmMemoryPath, found%xorigin)
    call mem_set_value(this%yorigin, 'YORIGIN', idmMemoryPath, found%yorigin)
    call mem_set_value(this%angrot, 'ANGROT', idmMemoryPath, found%angrot)
    !
    ! -- log values to list file
    if (this%iout > 0) then
      call this%log_options(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_options

  !> @brief Write user options to list file
  !<
  subroutine log_options(this, found)
    use SnfDislInputModule, only: SnfDislParamFoundType
    class(SnfDislType) :: this
    type(SnfDislParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting Discretization Options'

    if (found%length_units) then
      write (this%iout, '(4x,a,i0)') 'Model length unit [0=UND, 1=FEET, &
      &2=METERS, 3=CENTIMETERS] set as ', this%lenuni
    end if

    if (found%nogrb) then
      write (this%iout, '(4x,a,i0)') 'Binary grid file [0=GRB, 1=NOGRB] &
        &set as ', this%nogrb
    end if

    if (found%xorigin) then
      write (this%iout, '(4x,a,G0)') 'XORIGIN = ', this%xorigin
    end if

    if (found%yorigin) then
      write (this%iout, '(4x,a,G0)') 'YORIGIN = ', this%yorigin
    end if

    if (found%angrot) then
      write (this%iout, '(4x,a,G0)') 'ANGROT = ', this%angrot
    end if

    if (found%length_convert) then
      write (this%iout, '(4x,a,G0)') 'LENGTH_CONVERSION = ', this%convlength
    end if

    if (found%time_convert) then
      write (this%iout, '(4x,a,G0)') 'TIME_CONVERSION = ', this%convtime
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Options'

  end subroutine log_options

  !> @brief Copy dimensions from IDM into package
  !<
  subroutine source_dimensions(this)
    use KindModule, only: LGP
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SnfDislInputModule, only: SnfDislParamFoundType
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    integer(I4B) :: n
    type(SnfDislParamFoundType) :: found
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISL', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%nodes, 'NODES', idmMemoryPath, found%nodes)
    call mem_set_value(this%nvert, 'NVERT', idmMemoryPath, found%nvert)
    !
    ! -- for now assume nodes = nodesuser
    this%nodesuser = this%nodes
    !
    ! -- log simulation values
    if (this%iout > 0) then
      call this%log_dimensions(found)
    end if
    !
    ! -- verify dimensions were set
    if (this%nodesuser < 1) then
      call store_error( &
        'NODES was not specified or was specified incorrectly.')
      call store_error_unit(this%inunit)
    end if
    if (this%nvert < 1) then
      call store_warning( &
        'NVERT was not specified or was specified as zero.  The &
        &VERTICES and CELL2D blocks will not be read for the DISL6 &
        &Package in model ' // trim(this%memoryPath) // '.')
    end if
    !
    ! -- Allocate non-reduced vectors for disl
    call mem_allocate(this%segment_length, this%nodesuser, 'SEGMENT_LENGTH', this%memoryPath)
    call mem_allocate(this%tosegment, this%nodesuser, 'TOSEGMENT', this%memoryPath)
    call mem_allocate(this%idomain, this%nodesuser, 'IDOMAIN', this%memoryPath)
    !
    ! -- Allocate vertices array
    if (this%nvert > 0) then
      call mem_allocate(this%vertices, 3, this%nvert, 'VERTICES', this%memoryPath)
      call mem_allocate(this%fdc, this%nodesuser, 'FDC', this%memoryPath)
      call mem_allocate(this%cellxyz, 3, this%nodesuser, 'CELLXYZ', this%memoryPath)
    end if
    !
    ! -- initialize all cells to be active (idomain = 1)
    do n = 1, this%nodesuser
      this%segment_length(n) = DZERO
      this%tosegment(n) = 0
      this%idomain(n) = 1
    end do    
    !
    ! -- Return
    return
  end subroutine source_dimensions

  !> @brief Write dimensions to list file
  !<
  subroutine log_dimensions(this, found)
    use SnfDislInputModule, only: SnfDislParamFoundType
    class(SnfDislType) :: this
    type(SnfDislParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting Discretization Dimensions'

    if (found%nodes) then
      write (this%iout, '(4x,a,i0)') 'NODES = ', this%nodesuser
    end if

    if (found%nvert) then
      write (this%iout, '(4x,a,i0)') 'NVERT = ', this%nvert
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Dimensions'

  end subroutine log_dimensions

  subroutine source_griddata(this)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SnfDislInputModule, only: SnfDislParamFoundType
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(SnfDislParamFoundType) :: found
    ! -- formats
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISL', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%segment_length, 'SEGMENT_LENGTH', idmMemoryPath, &
                       found%segment_length)
    call mem_set_value(this%tosegment, 'TOSEGMENT', idmMemoryPath, &
                       found%tosegment)
    call mem_set_value(this%idomain, 'IDOMAIN', idmMemoryPath, found%idomain)
    !
    ! -- log simulation values
    if (this%iout > 0) then
      call this%log_griddata(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_griddata

  !> @brief Write griddata found to list file
  !<
  subroutine log_griddata(this, found)
    use SnfDislInputModule, only: SnfDislParamFoundType
    class(SnfDislType) :: this
    type(SnfDislParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting Discretization Griddata'

    if (found%segment_length) then
      write (this%iout, '(4x,a)') 'SEGMENT_LENGTH set from input file'
    end if

    if (found%tosegment) then
      write (this%iout, '(4x,a)') 'TOSEGMENT set from input file'
    end if

    if (found%idomain) then
      write (this%iout, '(4x,a)') 'IDOMAIN set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Griddata'

  end subroutine log_griddata

  !> @brief Copy vertex information from input data context
  !! to model context
  !<
  subroutine source_vertices(this)
    ! -- modules
    use MemoryManagerModule, only: mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SnfDislType) :: this
    ! -- local
    integer(I4B) :: i
    character(len=LENMEMPATH) :: idmMemoryPath
    real(DP), dimension(:), contiguous, pointer :: vert_x => null()
    real(DP), dimension(:), contiguous, pointer :: vert_y => null()
    real(DP), dimension(:), contiguous, pointer :: vert_z => null()
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISL', idm_context)
    !
    ! -- set pointers to memory manager input arrays
    call mem_setptr(vert_x, 'XV', idmMemoryPath)
    call mem_setptr(vert_y, 'YV', idmMemoryPath)
    call mem_setptr(vert_z, 'ZV', idmMemoryPath)
    !
    ! -- set vertices 3d array
    if (associated(vert_x) .and. associated(vert_y) .and. &
        associated(vert_z)) then
      do i = 1, this%nvert
        this%vertices(1, i) = vert_x(i)
        this%vertices(2, i) = vert_y(i)
        this%vertices(3, i) = vert_z(i)
      end do
    else
      call store_error('Required Vertex arrays not found.')
    end if
    !
    ! -- log
    if (this%iout > 0) then
      write (this%iout, '(1x,a)') 'Setting Discretization Vertices'
      write (this%iout, '(1x,a,/)') 'End setting discretization vertices'
    end if
    !
    ! -- Return
    return
  end subroutine source_vertices
    
  !> @brief Copy cell2d information from input data context
  !! to model context
  !<
  subroutine source_cell2d(this)
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    integer(I4B), dimension(:), contiguous, pointer :: icell2d => null()
    integer(I4B), dimension(:), contiguous, pointer :: ncvert => null()
    integer(I4B), dimension(:), contiguous, pointer :: icvert => null()
    real(DP), dimension(:), contiguous, pointer :: fdc => null()
    integer(I4B) :: i
    ! -- formats
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'DISL', idm_context)
    !
    ! -- set pointers to input path ncvert and icvert
    call mem_setptr(icell2d, 'ICELL2D', idmMemoryPath)
    call mem_setptr(ncvert, 'NCVERT', idmMemoryPath)
    call mem_setptr(icvert, 'ICVERT', idmMemoryPath)
    !
    ! --
    if (associated(icell2d) .and. associated(ncvert) &
        .and. associated(icvert)) then
      call this%define_cellverts(icell2d, ncvert, icvert)
    else
      call store_error('Required cell vertex arrays not found.')
    end if
    !
    ! -- set pointers to cell center arrays
    call mem_setptr(fdc, 'FDC', idmMemoryPath)
    !
    ! -- set fractional distance to cell center
    if (associated(fdc)) then
      do i = 1, this%nodesuser
        this%fdc(i) = fdc(i)
      end do
      call calculate_cellxyz(this%vertices, this%fdc, this%iavert, &
                             this%javert, this%cellxyz)
    else
      call store_error('Required fdc array not found.')
    end if
    !
    ! -- log
    if (this%iout > 0) then
      write (this%iout, '(1x,a)') 'Setting Discretization CELL2D'
      write (this%iout, '(1x,a,/)') 'End Setting Discretization CELL2D'
    end if
    !
    ! -- Return
    return
  end subroutine source_cell2d

  !> @brief Construct the iavert and javert integer vectors which
  !! are compressed sparse row index arrays that relate the vertices
  !! to segments
  !<
  subroutine define_cellverts(this, icell2d, ncvert, icvert)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(SnfDislType) :: this
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: icell2d
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: ncvert
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: icvert
    ! -- locals
    type(sparsematrix) :: vert_spm
    integer(I4B) :: i, j, ierr
    integer(I4B) :: icv_idx, startvert, maxnnz = 2
! ------------------------------------------------------------------------------
    !
    ! -- initialize sparse matrix
    call vert_spm%init(this%nodesuser, this%nvert, maxnnz)
    !
    ! -- add sparse matrix connections from input memory paths
    icv_idx = 1
    do i = 1, this%nodesuser
      if (icell2d(i) /= i) call store_error('ICELL2D input sequence violation.')
      do j = 1, ncvert(i)
        call vert_spm%addconnection(i, icvert(icv_idx), 0)
        if (j == 1) then
          startvert = icvert(icv_idx)
        end if
        icv_idx = icv_idx + 1
      end do
    end do
    !
    ! -- allocate and fill iavert and javert
    call mem_allocate(this%iavert, this%nodesuser + 1, 'IAVERT', this%memoryPath)
    call mem_allocate(this%javert, vert_spm%nnz, 'JAVERT', this%memoryPath)
    call vert_spm%filliaja(this%iavert, this%javert, ierr)
    call vert_spm%destroy()
    !
    ! -- Return
    return
  end subroutine define_cellverts

  !> @brief Calculate x, y, z coordinates of segment midpoint
  !<
  subroutine calculate_cellxyz(vertices, fdc, iavert, javert, cellxyz)
    ! -- dummy
    real(DP), dimension(:, :), intent(in) :: vertices !< 2d array of vertices with x, y, and z as columns
    real(DP), dimension(:), intent(in) :: fdc  !< fractional distance to segment midpoint (normally 0.5)
    integer(I4B), dimension(:), intent(in) :: iavert  !< csr mapping of vertices to cell segments
    integer(I4B), dimension(:), intent(in) :: javert  !< csr mapping of vertices to cell segments
    real(DP), dimension(:, :), intent(inout) :: cellxyz !< 2d array of segment midpoint with x, y, and z as columns
    ! -- local
    integer(I4B) :: nodes !< number of nodes
    integer(I4B) :: n !< node index
    integer(I4B) :: j !< vertex index
    integer(I4B) :: iv0 !< index for line segment start
    integer(I4B) :: iv1 !< index for linen segment end
    integer(I4B) :: ixyz !< x, y, z column index
    real(DP) :: segment_length !< segment length = sum of individual line segments
    real(DP) :: fd0 !< fractional distance to start of this line segment
    real(DP) :: fd1 !< fractional distance to end fo this line segment
    real(DP) :: fd !< fractional distance where midpoint (defined by fdc) is located
    real(DP) :: d !< distance

    nodes = size(iavert) - 1
    do n = 1, nodes

      ! calculate length of this segment
      segment_length = DZERO
      do j = iavert(n), iavert(n + 1) - 2
        segment_length = segment_length + &
                         calcdist(vertices, javert(j), javert(j + 1))
      end do

      ! find vertices that span midpoint
      iv0 = 0
      iv1 = 0
      fd0 = DZERO
      do j = iavert(n), iavert(n + 1) - 2
        d = calcdist(vertices, javert(j), javert(j + 1))
        fd1 = fd0 + d / segment_length

        ! if true, then we know the midpoint is some fractional distance (fd)
        ! from vertex j to vertex j + 1
        if (fd1 >= fdc(n)) then
          iv0 = javert(j)
          iv1 = javert(j + 1)
          fd = (fdc(n) - fd0) / (fd1 - fd0)
          exit
        end if
        fd0 = fd1
      end do

      ! find x, y, z position of point on line
      do ixyz = 1, 3
        cellxyz(ixyz, n) = (DONE - fd) * vertices(ixyz, iv0) + &
                        fd * vertices(ixyz, iv1)
      end do

    end do
  end subroutine calculate_cellxyz

  !> @brief Finalize grid construction
  !<
  subroutine grid_finalize(this)
    ! -- modules
    use SimModule, only: ustop, count_errors, store_error
    use ConstantsModule,   only: LINELENGTH, DZERO, DONE
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    integer(I4B) :: node, noder, k
    ! -- formats
    character(len=*), parameter :: fmtdz = &
      "('ERROR. CELL (',i0,',',i0,') THICKNESS <= 0. ', " //             &
      "'TOP, BOT: ',2(1pg24.15))"
    character(len=*), parameter :: fmtnr = &
      "(/1x, 'THE SPECIFIED IDOMAIN RESULTS IN A REDUCED NUMBER OF CELLS.'," // &
      "/1x, 'NUMBER OF USER NODES: ',I7," // &
      "/1X, 'NUMBER OF NODES IN SOLUTION: ', I7, //)"
    ! -- data
    !
    ! -- count active cells
    this%nodes = 0
    do k = 1, this%nodesuser
      if(this%idomain(k) > 0) this%nodes = this%nodes + 1
    enddo
    !
    ! -- Check to make sure nodes is a valid number
    if (this%nodes == 0) then
      call store_error('ERROR.  MODEL DOES NOT HAVE ANY ACTIVE NODES.')
      call store_error('MAKE SURE IDOMAIN ARRAY HAS SOME VALUES GREATER &
        &THAN ZERO.')
      call this%parser%StoreErrorUnit()
      call ustop()
    end if

    if (count_errors() > 0) then
      call this%parser%StoreErrorUnit()
      call ustop()
    endif
    !
    ! -- Array size is now known, so allocate
    call this%allocate_arrays()
    !
    ! -- Fill the nodereduced array with the reduced nodenumber, or
    !    a negative number to indicate it is a pass-through cell, or
    !    a zero to indicate that the cell is excluded from the
    !    solution.
    if(this%nodes < this%nodesuser) then
      node = 1
      noder = 1
      do k = 1, this%nodesuser
        if(this%idomain(k) > 0) then
          this%nodereduced(node) = noder
          noder = noder + 1
        elseif(this%idomain(k) < 0) then
          this%nodereduced(node) = -1
        else
          this%nodereduced(node) = 0
        endif
        node = node + 1
      enddo
    endif
    !
    ! -- allocate and fill nodeuser if a reduced grid
    if(this%nodes < this%nodesuser) then
      node = 1
      noder = 1
      do k = 1, this%nodesuser
        if(this%idomain(k) > 0) then
          this%nodeuser(noder) = node
          noder = noder + 1
        endif
          node = node + 1
      enddo
    endif

    ! -- Return
    return
  end subroutine grid_finalize

  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(SnfDislType) :: this
    !
    ! -- Allocate arrays in DisBaseType (mshape, top, bot, area)
    call this%DisBaseType%allocate_arrays()
    !
    ! -- Allocate arrays
    if(this%nodes < this%nodesuser) then
      call mem_allocate(this%nodeuser, this%nodes, 'NODEUSER', this%memoryPath)
      call mem_allocate(this%nodereduced, this%nodesuser, 'NODEREDUCED',       &
                        this%memoryPath)
    else
      call mem_allocate(this%nodeuser, 1, 'NODEUSER', this%memoryPath)
      call mem_allocate(this%nodereduced, 1, 'NODEREDUCED', this%memoryPath)
    endif
    !
    ! -- Initialize
    this%mshape(1) = this%nodesuser
    !
    ! -- Return
    return
  end subroutine allocate_arrays  

  subroutine create_connections(this)
    ! -- modules
    ! -- dummy
    class(SnfDislType) :: this
    ! -- local
    integer(I4B) :: nrsize
    !
    ! -- create and fill the connections object
    nrsize = 0
    if(this%nodes < this%nodesuser) nrsize = this%nodes
    !
    ! -- Allocate connections object
    allocate(this%con)

    call this%con%dislconnections(this%name_model, this%tosegment)
    this%nja = this%con%nja
    this%njas = this%con%njas
    !
    !
    ! -- return
    return
  end subroutine create_connections

  !> @brief Write binary grid file
  !<
  subroutine write_grb(this, icelltype)
    ! -- modules
    use InputOutputModule, only: getunit, openfile
    use OpenSpecModule, only: access, form
    ! -- dummy
    class(SnfDislType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B) :: i, iunit, ntxt
    integer(I4B), parameter :: lentxt = 100
    character(len=50) :: txthdr
    character(len=lentxt) :: txt
    character(len=LINELENGTH) :: fname
    character(len=*), parameter :: fmtgrdsave = &
      "(4X,'BINARY GRID INFORMATION WILL BE WRITTEN TO:', &
       &/,6X,'UNIT NUMBER: ', I0,/,6X, 'FILE NAME: ', A)"
    !
    ! -- Initialize
    ntxt = 10
    if (this%nvert > 0) ntxt = ntxt + 5
    !
    ! -- Open the file
    inquire (unit=this%inunit, name=fname)
    fname = trim(fname)//'.grb'
    iunit = getunit()
    write (this%iout, fmtgrdsave) iunit, trim(adjustl(fname))
    call openfile(iunit, this%iout, trim(adjustl(fname)), 'DATA(BINARY)', &
                  form, access, 'REPLACE')
    !
    ! -- write header information
    write (txthdr, '(a)') 'GRID DISL'
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    write (txthdr, '(a)') 'VERSION 1'
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    write (txthdr, '(a, i0)') 'NTXT ', ntxt
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    write (txthdr, '(a, i0)') 'LENTXT ', lentxt
    txthdr(50:50) = new_line('a')
    write (iunit) txthdr
    !
    ! -- write variable definitions
    write (txt, '(3a, i0)') 'NODES ', 'INTEGER ', 'NDIM 0 # ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'NJA ', 'INTEGER ', 'NDIM 0 # ', this%con%nja
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, 1pg24.15)') 'XORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%xorigin
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, 1pg24.15)') 'YORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%yorigin
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, 1pg24.15)') 'ANGROT ', 'DOUBLE ', 'NDIM 0 # ', this%angrot
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'IA ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'JA ', 'INTEGER ', 'NDIM 1 ', this%con%nja
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'ICELLTYPE ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    write (txt, '(3a, i0)') 'IDOMAIN   ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write (iunit) txt
    !
    ! -- if vertices have been read then write additional header information
    if (this%nvert > 0) then
      write (txt, '(3a, i0)') 'VERTICES ', 'DOUBLE ', 'NDIM 2 3 ', this%nvert
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'CELLX ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'CELLY ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'CELLZ ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'IAVERT ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
      write (txt, '(3a, i0)') 'JAVERT ', 'INTEGER ', 'NDIM 1 ', size(this%javert)
      txt(lentxt:lentxt) = new_line('a')
      write (iunit) txt
    end if
    !
    ! -- write data
    write (iunit) this%nodesuser ! nodes
    write (iunit) this%nja ! nja
    write (iunit) this%xorigin ! xorigin
    write (iunit) this%yorigin ! yorigin
    write (iunit) this%angrot ! angrot
    write (iunit) this%con%iausr ! ia
    write (iunit) this%con%jausr ! ja
    write (iunit) icelltype ! icelltype
    write (iunit) this%idomain ! idomain
    !
    ! -- if vertices have been read then write additional data
    if (this%nvert > 0) then
      write (iunit) this%vertices ! vertices
      write (iunit) (this%cellxyz(1, i), i=1, this%nodesuser) ! cellx
      write (iunit) (this%cellxyz(2, i), i=1, this%nodesuser) ! celly
      write (iunit) (this%cellxyz(3, i), i=1, this%nodesuser) ! cellz
      write (iunit) this%iavert ! iavert
      write (iunit) this%javert ! javert
    end if
    !
    ! -- Close the file
    close (iunit)
    !
    ! -- return
    return
  end subroutine write_grb

  !>
  !! Return a nodenumber from the user specified node number with an 
  !! option to perform a check.  This subroutine can be overridden by 
  !! child classes to perform mapping to a model node number
  !<
  function get_nodenumber_idx1(this, nodeu, icheck) result(nodenumber)
    class(SnfDislType), intent(in) :: this
    integer(I4B), intent(in) :: nodeu
    integer(I4B), intent(in) :: icheck
    integer(I4B) :: nodenumber
    !
    if (icheck /= 0) then
      if (nodeu < 1 .or. nodeu > this%nodes) then
        write (errmsg, '(a,i10)') &
          'Nodenumber less than 1 or greater than nodes:', nodeu
        call store_error(errmsg)
      end if
    end if
    !
    ! -- set node number based on wheter it is reduced or not
    if (this%nodes == this%nodesuser) then
      nodenumber = nodeu
    else
      nodenumber = this%nodereduced(nodeu)
    end if
    !
    ! -- return
    return
  end function get_nodenumber_idx1

  subroutine nodeu_to_string(this, nodeu, str)
    ! -- dummy
    class(SnfDislType) :: this
    integer(I4B), intent(in) :: nodeu
    character(len=*), intent(inout) :: str
    ! -- local
    character(len=10) :: nstr
    !
    write (nstr, '(i0)') nodeu
    str = '('//trim(adjustl(nstr))//')'
    !
    ! -- return
    return
  end subroutine nodeu_to_string

  subroutine disl_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SnfDislType) :: this
    ! -- local
    logical(LGP) :: deallocate_vertices
    !
    ! -- Deallocate idm memory
    call memorylist_remove(this%name_model, 'DISL', idm_context)
    call memorylist_remove(component=this%name_model, &
                           context=idm_context)
    !
    ! -- scalars
    deallocate_vertices = (this%nvert > 0)
    call mem_deallocate(this%nvert)
    call mem_deallocate(this%convlength)
    call mem_deallocate(this%convtime)
    !
    ! -- arrays
    call mem_deallocate(this%nodeuser)
    call mem_deallocate(this%nodereduced)
    call mem_deallocate(this%segment_length)
    call mem_deallocate(this%tosegment)
    call mem_deallocate(this%idomain)
    !
    ! -- cdl hack for arrays for vertices and cell2d blocks
    if (deallocate_vertices) then
      call mem_deallocate(this%vertices)
      call mem_deallocate(this%fdc)
      call mem_deallocate(this%cellxyz)
      call mem_deallocate(this%iavert)
      call mem_deallocate(this%javert)
    end if
    !
    ! -- DisBaseType deallocate
    call this%DisBaseType%dis_da()
    !
    ! -- Return
    return
  end subroutine disl_da

  

end module SnfDislModule