module SnfMmrModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME, DZERO, LINELENGTH
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_allocate
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: SnfMmrType, mmr_cr

  type, extends(NumericalPackageType) :: SnfMmrType
    real(DP), dimension(:), pointer, contiguous :: mann_n => null() !< manning roughness coefficient
    real(DP), dimension(:), pointer, contiguous :: seg_depth => null() !< depth of bankfull water in segment
    ! get from disl -- real(DP), dimension(:), pointer, contiguous :: seg_length => null() !< muskingum manning parameter
    real(DP), dimension(:), pointer, contiguous :: seg_slope => null() !< surface slope of each segment
    integer(I4B), dimension(:), pointer, contiguous :: tosegment => null() !< index of the downstream segment
    real(DP), dimension(:), pointer, contiguous :: x_coef => null() !< routing weighting factor
  contains
    procedure :: allocate_arrays
    procedure :: mmr_load
    procedure :: source_options
    procedure :: log_options
    procedure :: source_griddata
    procedure :: log_griddata
    procedure :: mmr_da
  end type SnfMmrType

  contains

  subroutine mmr_cr(mmrobj, name_model, inunit, dis, iout)
! ******************************************************************************
! mmr_cr -- Create a new MMR object. Pass a inunit value of 0 if mmr data will
!           initialized from memory
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use IdmMf6FileLoaderModule, only: input_load
    use ConstantsModule, only: LENPACKAGETYPE
    ! -- dummy
    type(SnfMmrType), pointer :: mmrobj
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    class(DisBaseType), pointer, intent(inout) :: dis !< the pointer to the discretization
    integer(I4B), intent(in) :: iout
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'MMR -- MUSKINGUM MANNINGS ROUTING PACKAGE, VERSION 1, 1/23/2023', &
        &' INPUT READ FROM UNIT ', i0, /)"
! ------------------------------------------------------------------------------
    !
    ! -- Create the object
    allocate (mmrobj)
    call mmrobj%set_names(1, name_model, 'MMR', 'MMR')
    call mmrobj%allocate_scalars()
    mmrobj%inunit = inunit
    mmrobj%iout = iout
    mmrobj%dis => dis
    !
    ! -- if reading from file
    if (inunit > 0) then
      !
      ! -- Identify package
      if (iout > 0) then
        write (iout, fmtheader) inunit
      end if
      !
      ! -- allocate arrays
      call mmrobj%allocate_arrays()
      !
      ! -- load mmr
      call mmrobj%mmr_load()
    end if
    !
    ! -- Return
    return
  end subroutine mmr_cr

  subroutine allocate_arrays(this)
    ! -- dummy
    class(SnfMmrType) :: this
    ! -- locals
    !
    ! -- allocate
    call mem_allocate(this%mann_n, this%dis%nodes, 'MANN_N', this%memoryPath)
    call mem_allocate(this%seg_depth, this%dis%nodes, 'SEG_DEPTH', this%memoryPath)
    call mem_allocate(this%seg_slope, this%dis%nodes, 'SEG_SLOPE', this%memoryPath)
    call mem_allocate(this%tosegment, this%dis%nodes, 'TOSEGMENT', this%memoryPath)
    call mem_allocate(this%x_coef, this%dis%nodes, 'X_COEF', this%memoryPath)
    !
    ! -- Return
    return
  end subroutine allocate_arrays
    
  subroutine mmr_load(this)
    ! -- dummy
    class(SnfMmrType) :: this
    ! -- locals
    !
    ! -- source input data
    call this%source_options()
    call this%source_griddata()
    !
    ! -- Return
    return
  end subroutine mmr_load

  !> @brief Copy options from IDM into package
  !<
  subroutine source_options(this)
    ! -- modules
    use KindModule, only: LGP
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SnfMmrInputModule, only: SnfMmrParamFoundType
    ! -- dummy
    class(SnfMmrType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(SnfMmrParamFoundType) :: found
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'MMR', idm_context)
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%ipakcb, 'IPAKCB', idmMemoryPath, found%ipakcb)
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
    use SnfMmrInputModule, only: SnfMmrParamFoundType
    class(SnfMmrType) :: this
    type(SnfMmrParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting MMR Options'

    if (found%ipakcb) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if


    write (this%iout, '(1x,a,/)') 'End Setting MMR Options'

  end subroutine log_options

  subroutine source_griddata(this)
    ! -- modules
    use SimModule, only: count_errors, store_error
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_reallocate
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SnfMmrInputModule, only: SnfMmrParamFoundType
    ! -- dummy
    class(SnfMmrType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    character(len=LINELENGTH) :: errmsg
    type(SnfMmrParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    ! -- formats
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'MMR', idm_context)
    !
    ! -- set map to convert user input data into reduced data
    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%mann_n, 'MANN_N', idmMemoryPath, map, found%mann_n)
    call mem_set_value(this%seg_depth, 'SEG_DEPTH', idmMemoryPath, map, found%seg_depth)
    call mem_set_value(this%seg_slope, 'SEG_SLOPE', idmMemoryPath, map, found%seg_slope)
    call mem_set_value(this%tosegment, 'TOSEGMENT', idmMemoryPath, map, found%tosegment)
    call mem_set_value(this%x_coef, 'X_COEF', idmMemoryPath, map, found%x_coef)
    !
    ! -- ensure MANN_N was found
    if (.not. found%mann_n) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: MANN_N not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure SEG_DEPTH was found
    if (.not. found%seg_depth) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: SEG_DEPTH not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure SEG_SLOPE was found
    if (.not. found%seg_slope) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: SEG_SLOPE not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure TOSEGMENT was found
    if (.not. found%tosegment) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: TOSEGMENT not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure X_COEF was found
    if (.not. found%x_coef) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: X_COEF not found.'
      call store_error(errmsg)
    end if
    !
    ! -- log griddata
    if (this%iout > 0) then
      call this%log_griddata(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_griddata
    
  !> @brief log griddata to list file
  !<
  subroutine log_griddata(this, found)
    use SnfMmrInputModule, only: SnfMmrParamFoundType
    class(SnfMmrType) :: this
    type(SnfMmrParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting MMR Griddata'

    if (found%mann_n) then
      write (this%iout, '(4x,a)') 'MANN_N set from input file'
    end if

    if (found%seg_depth) then
      write (this%iout, '(4x,a)') 'SEG_DEPTH set from input file'
    end if

    if (found%seg_slope) then
      write (this%iout, '(4x,a)') 'SEG_SLOPE set from input file'
    end if

    if (found%tosegment) then
      write (this%iout, '(4x,a)') 'TOSEGMENT set from input file'
    end if

    if (found%x_coef) then
      write (this%iout, '(4x,a)') 'X_COEF set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting MMR Griddata'

  end subroutine log_griddata

  subroutine mmr_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SnfMmrType) :: this
    !
    ! -- Deallocate input memory
    call memorylist_remove(this%name_model, 'MMR', idm_context)
    !
    ! -- Scalars

    !
    ! -- Deallocate arrays
    call mem_deallocate(this%mann_n)
    call mem_deallocate(this%seg_depth)
    call mem_deallocate(this%seg_slope)
    call mem_deallocate(this%tosegment)
    call mem_deallocate(this%x_coef)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mmr_da
    


end module SnfMmrModule