module SnfDislModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME, DZERO, LINELENGTH
  use SimVariablesModule, only: errmsg
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_allocate
  use SimModule, only: count_errors, store_error, store_error_unit
  use BaseDisModule, only: DisBaseType
  use DislGeom, only: calcdist, partialdist

  implicit none

  private
  public :: disl_cr

  type, extends(DisBaseType) :: SnfDislType  
    integer(I4B), pointer :: nvert => null()                                     !< number of x,y vertices
    integer(I4B), pointer :: nsupportedgeoms => null()                           !< number of supported geometries
    integer(I4B), pointer :: nactivegeoms => null()                              !< number of active geometries
    real(DP), pointer :: convlength => null()                                    !< conversion factor for length
    real(DP), pointer :: convtime => null()                                      !< conversion factor for time  
    real(DP), dimension(:,:), pointer, contiguous :: vertices => null()          !< cell vertices stored as 2d array of x, y, and z
    real(DP), dimension(:,:), pointer, contiguous :: cellcenters => null()       !< cell centers stored as 2d array of x, y, and z
    integer(I4B), dimension(:,:), pointer, contiguous :: centerverts => null()   !< vertex at cell center or vertices cell center is between
    real(DP), dimension(:), pointer, contiguous :: fdc => null()                 !< fdc stored as array
    real(DP), dimension(:), pointer, contiguous :: celllen => null()             !< length of each conduit
    integer(I4B), dimension(:), pointer, contiguous :: iavert => null()          !< cell vertex pointer ia array
    integer(I4B), dimension(:), pointer, contiguous :: javert => null()          !< cell vertex pointer ja array
    integer(I4B), dimension(:), pointer, contiguous :: iavertcells => null()     !< vertex to cells ia array
    integer(I4B), dimension(:), pointer, contiguous :: javertcells => null()     !< vertex to cells ja array
    integer(I4B), dimension(:), pointer, contiguous :: idomain => null()         !< idomain (nodes)
    integer(I4B), dimension(:), pointer, contiguous :: iageom => null()          !< cell geometry pointer ia array (nodes))
    integer(I4B), dimension(:), pointer, contiguous :: iageocellnum => null()    !< cell geometry number ia array (nodes))
    !type(GeometryContainer), allocatable, dimension(:) :: jametries              !< active geometry classes ja array
  contains
    procedure :: disl_load
    ! -- private
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: source_options
    procedure :: source_dimensions
    procedure :: source_griddata
    procedure :: source_vertices
    procedure :: source_cell1d
    procedure :: log_options
    procedure :: log_dimensions
    procedure :: log_griddata
    procedure :: define_cellverts
    procedure :: grid_finalize
    procedure :: connect
    procedure :: write_grb

  end type SnfDislType 

contains

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
  
  subroutine allocate_scalars(this, name_model)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use ConstantsModule,   only: DONE
    ! -- dummy
    class(SnfDislType) :: this
    character(len=*), intent(in) :: name_model
! ------------------------------------------------------------------------------
    !
    ! -- Allocate parent scalars
    call this%DisBaseType%allocate_scalars(name_model)
    !
    ! -- Allocate
    call mem_allocate(this%nvert, 'NVERT', this%memoryPath)
    call mem_allocate(this%nsupportedgeoms, 'NSUPPORTEDGEOMS', this%memoryPath)
    call mem_allocate(this%nactivegeoms, 'NACTIVEGEOMS', this%memoryPath)
    call mem_allocate(this%convlength, 'CONVLENGTH', this%memoryPath)
    call mem_allocate(this%convtime, 'CONVTIME', this%memoryPath)

    !
    ! -- Initialize
    this%nvert = 0
    this%nactivegeoms = 0
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
    call this%source_vertices()
    call this%source_cell1d()
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
    call mem_set_value(this%nodesuser, 'NODES', idmMemoryPath, found%nodes)
    call mem_set_value(this%nvert, 'NVERT', idmMemoryPath, found%nvert)
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
      call store_error( &
        'NVERT was not specified or was specified incorrectly.')
      call store_error_unit(this%inunit)
    end if
    !
    ! -- Allocate non-reduced vectors for disl
    call mem_allocate(this%idomain, this%nodesuser, 'IDOMAIN', this%memoryPath)
    !
    ! -- Allocate vertices array
    call mem_allocate(this%vertices, 3, this%nvert, 'VERTICES', this%memoryPath)
    call mem_allocate(this%fdc, this%nodesuser, 'FDC', this%memoryPath)
    call mem_allocate(this%celllen, this%nodesuser, 'CELLLEN', this%memoryPath)
    call mem_allocate(this%cellcenters, 3, this%nodesuser, 'CELLCENTERS', this%memoryPath)
    call mem_allocate(this%centerverts, 2, this%nodesuser, 'CENTERVERTS', this%memoryPath)
    call mem_allocate(this%iageom, this%nodesuser, 'IAGEOM', this%memoryPath)
    call mem_allocate(this%iageocellnum, this%nodesuser, 'IAGEOCELLNUM', this%memoryPath) 
    !cdl allocate(this%jametries(this%nsupportedgeoms))
    !
    ! -- initialize all cells to be active (idomain = 1)
    do n = 1, this%nodesuser
      this%idomain(n) = 1
      this%iageom(n) = 0
      this%iageocellnum(n) = 0
      this%celllen(n) = DZERO
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

    if (found%idomain) then
      write (this%iout, '(4x,a)') 'IDOMAIN set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting Discretization Griddata'

  end subroutine log_griddata

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
      write (this%iout, '(1x,a)') 'Discretization Vertex data loaded'
    end if
    !
    ! -- Return
    return
  end subroutine source_vertices
    
  subroutine source_cell1d(this)
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    integer(I4B), dimension(:), contiguous, pointer :: icell1d => null()
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
    call mem_setptr(icell1d, 'ICELL1D', idmMemoryPath)
    call mem_setptr(ncvert, 'NCVERT', idmMemoryPath)
    call mem_setptr(icvert, 'ICVERT', idmMemoryPath)
    !
    ! --
    if (associated(icell1d) .and. associated(ncvert) &
        .and. associated(icvert)) then
      call this%define_cellverts(icell1d, ncvert, icvert)
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
    else
      call store_error('Required fdc array not found.')
    end if
    !
    ! -- log
    if (this%iout > 0) then
      write (this%iout, '(1x,a)') 'Discretization Cell1d data loaded'
    end if
    !
    ! -- Return
    return
  end subroutine source_cell1d

  subroutine define_cellverts(this, icell1d, ncvert, icvert)
    ! -- modules
    use SparseModule, only: sparsematrix
    ! -- dummy
    class(SnfDislType) :: this
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: icell1d
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: ncvert
    integer(I4B), dimension(:), contiguous, pointer, intent(in) :: icvert
    ! -- locals
    type(sparsematrix) :: vert_spm
    integer(I4B) :: i
    integer(I4B) :: j
    integer(I4B) :: k
    integer(I4B) :: ierr
    integer(I4B) :: icv_idx
    integer(I4B) :: maxnnz = 5
    integer(I4B) :: maxvert
    integer(I4B) :: maxvertcell
    integer(I4B) :: icurcell
    logical(LGP) :: isfound
    integer(I4B), dimension(:), pointer, contiguous :: vnumcells => null()
    ! format
    character(len=*), parameter :: fmtncpl = &
      "(3x, 'SUCCESSFULLY READ ',i0,' CELL2D INFORMATION ENTRIES')"
    character(len=*), parameter :: fmtmaxvert = &
      "(3x, 'MAXIMUM NUMBER OF CELL2D VERTICES IS ',i0,' FOR CELL ', i0)"
    character(len=*), parameter :: fmtvert = &
      "('ERROR. IAVERTCELLS DOES NOT CONTAIN THE CORRECT NUMBER OF '" //   &
      "' CONNECTIONS FOR VERTEX ', i0)"
    !
    ! -- initialize
    maxvert = 0
    maxvertcell = 0
    !
    ! -- initialize sparse matrix
    call vert_spm%init(this%nodesuser, this%nvert, maxnnz)
    !
    ! -- add sparse matrix connections from input memory paths
    icv_idx = 1
    do i = 1, this%nodesuser
      if (icell1d(i) /= i) call store_error('ICELL1D input sequence violation.')
      if(ncvert(i) > maxvert) then
        maxvert = ncvert(i)
        maxvertcell = i
      endif
      do j = 1, ncvert(i)
        call vert_spm%addconnection(i, icvert(icv_idx), 0)
        icv_idx = icv_idx + 1
      end do
    end do
    !
    ! -- allocate and fill iavert and javert
    call mem_allocate(this%iavert, this%nodesuser + 1, 'IAVERT', this%memoryPath)
    call mem_allocate(this%javert, vert_spm%nnz, 'JAVERT', this%memoryPath)
    call vert_spm%filliaja(this%iavert, this%javert, ierr)

    ! allocate vertex to cellids map
    call mem_allocate(this%iavertcells, this%nvert+1, 'IAVERTCELLS', this%memoryPath)
    call mem_allocate(this%javertcells, vert_spm%nnz, 'JAVERTCELLS', this%memoryPath)
    ! calculate number of cell connections for each vertex
    allocate(vnumcells(this%nvert))
    do j = 1, this%nvert
      vnumcells(j) = 0
    end do
    do j = 1, vert_spm%nnz
      vnumcells(this%javert(j)) = vnumcells(this%javert(j)) + 1
    end do
    ! build iavertcells
    this%iavertcells(1) = 1
    do j = 2, this%nvert
      this%iavertcells(j) = this%iavertcells(j-1) + vnumcells(j-1)
    end do
    this%iavertcells(this%nvert+1) = vert_spm%nnz + 1
    ! initialize javertcells
    do j = 1, vert_spm%nnz
      this%javertcells(j) = 0
    end do
    ! build javertcells
    icurcell = 1
    do j = 1, vert_spm%nnz
      if (this%iavert(icurcell+1) == j) then
        icurcell = icurcell + 1
      end if
      isfound = .false.
      inner: do k = this%iavertcells(this%javert(j)), this%iavertcells(this%javert(j)+1) - 1
        if (this%javertcells(k) == 0) then
          ! fill the first available index and exit
          this%javertcells(k) = icurcell
          isfound = .TRUE.
          exit inner
        end if
      end do inner
      if (.not. isfound) then
        write(errmsg, fmtvert) j
        call store_error(errmsg, terminate=.true.)
      endif
    end do
    !
    ! clean up
    deallocate(vnumcells)
    call vert_spm%destroy()
    !
    ! -- Write information
    write(this%iout, fmtncpl) this%nodesuser
    write(this%iout, fmtmaxvert) maxvert, maxvertcell
    write(this%iout,'(1x,a)')'END PROCESSING VERTICES'
    !
    ! -- Return
    return
  end subroutine define_cellverts

  subroutine grid_finalize(this)
    ! -- modules
    use SimModule, only: ustop, count_errors, store_error
    use ConstantsModule,   only: LINELENGTH, DZERO, DONE
    ! -- dummy
    class(SnfDislType) :: this
    ! -- locals
    integer(I4B) :: node, noder, j, k
    real(DP) :: curlen, seglen
    real(DP) :: cendist, segpercent
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

    ! calculate and fill cell center array
    do k = 1, this%nodesuser
      ! calculate node length
      do j = this%iavert(k), this%iavert(k+1) - 2
        this%celllen(k) = this%celllen(k) + calcdist(this%vertices, this%javert(j), &
          this%javert(j+1))
      end do

      ! calculate distance from start of node to cell center
      cendist = this%celllen(k) * this%fdc(k)
      ! calculate cell center location
      curlen = DZERO
      ! loop through cell's vertices
      inner: do j = this%iavert(k), this%iavert(k+1) - 2
        seglen = calcdist(this%vertices, this%javert(j), this%javert(j+1))
        ! if cell center between vertex k and k+1
        if (seglen + curlen >= cendist) then
            ! calculate cell center locations
            segpercent = (cendist - curlen) / seglen
            this%cellcenters(1, k) = partialdist(this%vertices(1,   &
              this%javert(j)), this%vertices(1, this%javert(j+1)),  &
              segpercent)
            this%cellcenters(2, k) = partialdist(this%vertices(2,   &
              this%javert(j)), this%vertices(2, this%javert(j+1)),  &
              segpercent)
            this%cellcenters(3, k) = partialdist(this%vertices(3,   &
              this%javert(j)), this%vertices(3, this%javert(j+1)),  &
              segpercent)
            ! record vertices that cell center is between
            if (abs(segpercent - DONE) < 0.00001) then
              this%centerverts(1, k) = this%javert(j+1)
              this%centerverts(2, k) = 0
            else if (abs(segpercent - DZERO) < 0.00001) then
              this%centerverts(1, k) = this%javert(j)
              this%centerverts(2, k) = 0
            else
              this%centerverts(1, k) = this%javert(j)
              this%centerverts(2, k) = this%javert(j+1)
            end if
            exit inner
        end if
        curlen = curlen + seglen
      end do inner
    end do
    !
    ! -- Build connections
    call this%connect()
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

  subroutine connect(this)
    ! -- modules
    ! -- dummy
    class(SnfDislType) :: this
    ! -- local
    integer(I4B) :: nrsize
    !
    ! -- create and fill the connections object
    nrsize = 0
    if(this%nodes < this%nodesuser) nrsize = this%nodes
    allocate(this%con)
    ! SRP TODO: connections need geometry info
    call this%con%dislconnections(this%name_model, this%nodes, this%nodesuser, &
                                  nrsize, this%nvert, this%vertices,           &
                                  this%iavert, this%javert, this%iavertcells,  &
                                  this%javertcells, this%cellcenters,          &
                                  this%centerverts, this%fdc,                  &
                                  this%nodereduced, this%nodeuser)
    this%nja = this%con%nja
    this%njas = this%con%njas
    !
    !
    ! -- return
    return
  end subroutine connect

  subroutine write_grb(this, icelltype)
! ******************************************************************************
! write_grb -- Write the binary grid file
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use InputOutputModule, only: getunit, openfile
    use OpenSpecModule, only: access, form
    use ConstantsModule, only: DZERO
    ! -- dummy
    class(SnfDislType) :: this
    integer(I4B), dimension(:), intent(in) :: icelltype
    ! -- local
    integer(I4B) :: iunit, ntxt
    integer(I4B), parameter :: lentxt = 100
    character(len=50) :: txthdr
    character(len=lentxt) :: txt
    character(len=LINELENGTH) :: fname
    character(len=*),parameter :: fmtgrdsave = &
      "(4X,'BINARY GRID INFORMATION WILL BE WRITTEN TO:',                      &
       &/,6X,'UNIT NUMBER: ', I0,/,6X, 'FILE NAME: ', A)"
! ------------------------------------------------------------------------------
    !
    ! -- Initialize
    ntxt = 17
    !
    ! -- Open the file
    inquire(unit=this%inunit, name=fname)
    fname = trim(fname) // '.grb'
    iunit = getunit()
    write(this%iout, fmtgrdsave) iunit, trim(adjustl(fname))
    call openfile(iunit, this%iout, trim(adjustl(fname)), 'DATA(BINARY)',      &
                  form, access, 'REPLACE')
    !
    ! -- write header information
    write(txthdr, '(a)') 'GRID DISL'
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    write(txthdr, '(a)') 'VERSION 1'
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    write(txthdr, '(a, i0)') 'NTXT ', ntxt
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    write(txthdr, '(a, i0)') 'LENTXT ', lentxt
    txthdr(50:50) = new_line('a')
    write(iunit) txthdr
    !
    ! -- write variable definitions
    write(txt, '(3a, i0)') 'NCELLS ', 'INTEGER ', 'NDIM 0 # ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NVERT ', 'INTEGER ', 'NDIM 0 # ', this%nvert
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NJAVERT ', 'INTEGER ', 'NDIM 0 # ', size(this%javert)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'NJA ', 'INTEGER ', 'NDIM 0 # ', this%con%nja
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg25.15e3)') 'XORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%xorigin
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg25.15e3)') 'YORIGIN ', 'DOUBLE ', 'NDIM 0 # ', this%yorigin
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, 1pg25.15e3)') 'ANGROT ', 'DOUBLE ', 'NDIM 0 # ', this%angrot
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'VERTICES ', 'DOUBLE ', 'NDIM 2 2 ', this%nvert
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'FDC ', 'DOUBLE ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IAVERT ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JAVERT ', 'INTEGER ', 'NDIM 1 ', size(this%javert)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IAVERTCELLS ', 'INTEGER ', 'NDIM 1 ', size(this%iavertcells)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JAVERTCELLS ', 'INTEGER ', 'NDIM 1 ', size(this%javertcells)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IA ', 'INTEGER ', 'NDIM 1 ', this%nodesuser + 1
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'JA ', 'INTEGER ', 'NDIM 1 ', size(this%con%jausr)
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'IDOMAIN ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    write(txt, '(3a, i0)') 'ICELLTYPE ', 'INTEGER ', 'NDIM 1 ', this%nodesuser
    txt(lentxt:lentxt) = new_line('a')
    write(iunit) txt
    !
    ! -- write data
    write(iunit) this%nodesuser                                                 ! ncells
    write(iunit) this%nvert                                                     ! nvert
    write(iunit) size(this%javert)                                              ! njavert
    write(iunit) this%nja                                                       ! nja
    write(iunit) this%xorigin                                                   ! xorigin
    write(iunit) this%yorigin                                                   ! yorigin
    write(iunit) this%angrot                                                    ! angrot
    write(iunit) this%vertices                                                  ! vertices
    write(iunit) this%fdc                                                       ! fdc
    write(iunit) this%iavert                                                    ! iavert
    write(iunit) this%javert                                                    ! javert
    write(iunit) this%iavertcells                                               ! iavert
    write(iunit) this%javertcells                                               ! javert
    write(iunit) this%con%iausr                                                 ! iausr
    write(iunit) this%con%jausr                                                 ! jausr
    write(iunit) this%idomain                                                   ! idomain
    write(iunit) icelltype                                                      ! icelltype
    !
    ! -- Close the file
    close(iunit)
    !
    ! -- return
    return
  end subroutine write_grb
  
end module SnfDislModule