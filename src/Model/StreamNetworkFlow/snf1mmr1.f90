!> @brief Stream Network Flow (SNF) Muskingum-Manning Routing (MMR) Module
!!
!! This module solves one-dimension flow routing using a Muskingum-Manning
!! approach.  The approach uses the following input parameters.
!! 
!<
module SnfMmrModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH, LENVARNAME, LINELENGTH, &
                             DZERO, DHALF, DONE, DTWO, DTHREE
  use MemoryHelperModule, only: create_mem_path
  use MemoryManagerModule, only: mem_allocate
  use NumericalPackageModule, only: NumericalPackageType
  use BaseDisModule, only: DisBaseType
  use SnfDislModule, only: SnfDislType

  implicit none
  private
  public :: SnfMmrType, mmr_cr

  type, extends(NumericalPackageType) :: SnfMmrType

    ! -- user-provided input
    integer(I4B), dimension(:), pointer, contiguous :: iseg_order => null() !< routing calculation order
    real(DP), dimension(:), pointer, contiguous :: qoutflow0 => null() !< initial outflow for each reach
    real(DP), dimension(:), pointer, contiguous :: mann_n => null() !< manning roughness coefficient
    real(DP), dimension(:), pointer, contiguous :: seg_depth => null() !< depth of bankfull water in segment
    real(DP), dimension(:), pointer, contiguous :: seg_slope => null() !< surface slope of each segment
    real(DP), dimension(:), pointer, contiguous :: x_coef => null() !< routing weighting factor

    ! -- input arguments to calc_muskingum_mann routine
    real(DP), dimension(:), pointer, contiguous :: seg_lat_inflow => null() !< lateral inflow to each segment
    real(DP), dimension(:), pointer, contiguous :: seg_inflow0_in => null() !< previous segment inflow variable (internal calculations)
    real(DP), dimension(:), pointer, contiguous :: outflow_ts_in => null() !< outflow timeseries variable (internal calculations)
    integer(I4B), dimension(:), pointer, contiguous :: tsi => null() !< integer flood wave travel time
    real(DP), dimension(:), pointer, contiguous :: ts => null() !< float version of integer flood wave travel time
    real(DP), dimension(:), pointer, contiguous :: c0 => null() !< Muskingum c0 variable
    real(DP), dimension(:), pointer, contiguous :: c1 => null() !< Muskingum c1 variable
    real(DP), dimension(:), pointer, contiguous :: c2 => null() !< Muskingum c2 variable

    ! -- output arguments to calc_muskingum_mann routine
    real(DP), dimension(:), pointer, contiguous :: seg_up_inflow => null() !< inflow for each segment for the current day
    real(DP), dimension(:), pointer, contiguous :: seg_inflow0 => null() !< segment inflow variable
    real(DP), dimension(:), pointer, contiguous :: seg_inflow => null() !< segment inflow variable
    real(DP), dimension(:), pointer, contiguous :: seg_outflow0 => null() !< outflow for each segment for the current day
    real(DP), dimension(:), pointer, contiguous :: seg_outflow => null() !< outflow for each segment for the current day
    real(DP), dimension(:), pointer, contiguous :: inflow_ts => null() !< inflow timeseries variable
    real(DP), dimension(:), pointer, contiguous :: outflow_ts => null() !< outflow timeseries variable (internal calculations)
    real(DP), dimension(:), pointer, contiguous :: seg_current_sum => null() !< summation variable

    ! -- budget vectors
    real(DP), dimension(:), pointer, contiguous :: extoutflow => null() !< flows leaving model (for tosegment = 0)
    real(DP), dimension(:), pointer, contiguous :: qsto => null() !< storage rates

    ! -- pointer to concrete disl subclass of DisBaseType
    type(SnfDislType), pointer :: disl

  contains
    procedure :: allocate_arrays
    procedure :: mmr_load
    procedure :: source_options
    procedure :: log_options
    procedure :: source_griddata
    procedure :: log_griddata
    procedure :: mmr_ar
    procedure :: mmr_ad
    procedure :: mmr_init_data
    procedure :: mmr_solve
    procedure :: mmr_cq
    procedure :: mmr_bd
    procedure :: mmr_save_model_flows
    procedure :: mmr_print_model_flows
    procedure :: mmr_da
  end type SnfMmrType

  contains

  !> @brief create package
  !<
  subroutine mmr_cr(mmrobj, name_model, inunit, dis, iout)
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
    !
    ! -- Create the object
    allocate (mmrobj)
    call mmrobj%set_names(1, name_model, 'MMR', 'MMR')
    call mmrobj%allocate_scalars()
    mmrobj%inunit = inunit
    mmrobj%iout = iout
    mmrobj%dis => dis

    ! -- store pointer to disl
    !    Not normally good practice, but since SNF only works with DISL
    !    may be okay
    select type (dis)
    type is (SnfDislType)
      mmrobj%disl => dis
    end select

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

  !> @brief allocate memory for arrays
  !<
  subroutine allocate_arrays(this)
    ! -- dummy
    class(SnfMmrType) :: this
    ! -- locals
    integer(I4B) :: n
    !
    ! -- user-provided input
    call mem_allocate(this%iseg_order, this%dis%nodes, 'ISEG_ORDER', this%memoryPath)
    call mem_allocate(this%qoutflow0, this%dis%nodes, 'QOUTFLOW0', this%memoryPath)
    call mem_allocate(this%mann_n, this%dis%nodes, 'MANN_N', this%memoryPath)
    call mem_allocate(this%seg_depth, this%dis%nodes, 'SEG_DEPTH', this%memoryPath)
    call mem_allocate(this%seg_slope, this%dis%nodes, 'SEG_SLOPE', this%memoryPath)
    call mem_allocate(this%x_coef, this%dis%nodes, 'X_COEF', this%memoryPath)

    ! -- input arguments to calc_muskingum_mann routine
    call mem_allocate(this%seg_lat_inflow, this%dis%nodes, 'SEG_LAT_INFLOW', this%memoryPath)
    call mem_allocate(this%seg_inflow0_in, this%dis%nodes, 'SEG_INFLOW0_IN', this%memoryPath)
    call mem_allocate(this%outflow_ts_in, this%dis%nodes, 'OUTFLOW_TS_IN', this%memoryPath)
    call mem_allocate(this%tsi, this%dis%nodes, 'TSI', this%memoryPath)
    call mem_allocate(this%ts, this%dis%nodes, 'TS', this%memoryPath)
    call mem_allocate(this%c0, this%dis%nodes, 'C0', this%memoryPath)
    call mem_allocate(this%c1, this%dis%nodes, 'C1', this%memoryPath)
    call mem_allocate(this%c2, this%dis%nodes, 'C2', this%memoryPath)

    ! -- output arguments to calc_muskingum_mann routine
    call mem_allocate(this%seg_up_inflow, this%dis%nodes, 'SEG_UP_INFLOW', this%memoryPath)
    call mem_allocate(this%seg_inflow0, this%dis%nodes, 'SEG_INFLOW0', this%memoryPath)
    call mem_allocate(this%seg_inflow, this%dis%nodes, 'SEG_INFLOW', this%memoryPath)
    call mem_allocate(this%seg_outflow0, this%dis%nodes, 'SEG_OUTFLOW0', this%memoryPath)
    call mem_allocate(this%seg_outflow, this%dis%nodes, 'SEG_OUTFLOW', this%memoryPath)
    call mem_allocate(this%inflow_ts, this%dis%nodes, 'INFLOW_TS', this%memoryPath)
    call mem_allocate(this%outflow_ts, this%dis%nodes, 'OUTFLOW_TS', this%memoryPath)
    call mem_allocate(this%seg_current_sum, this%dis%nodes, 'SEG_CURRENT_SUM', this%memoryPath)

    call mem_allocate(this%extoutflow, this%dis%nodes, 'EXTOUTFLOW', this%memoryPath)
    call mem_allocate(this%qsto, this%dis%nodes, 'QSTO', this%memoryPath)

    do n = 1, this%dis%nodes

      this%iseg_order(n) = 0
      this%qoutflow0(n) = DZERO
      this%mann_n(n) = DZERO
      this%seg_depth(n) = DZERO
      this%seg_slope(n) = DZERO
      this%x_coef(n) = DZERO

      this%seg_lat_inflow(n) = DZERO
      this%seg_inflow0_in(n) = DZERO
      this%outflow_ts_in(n) = DZERO
      this%tsi(n) = 0
      this%ts(n) = DZERO
      this%c0(n) = DZERO
      this%c1(n) = DZERO
      this%c2(n) = DZERO

      this%seg_up_inflow(n) = DZERO
      this%seg_inflow0(n) = DZERO
      this%seg_inflow(n) = DZERO
      this%seg_outflow0(n) = DZERO
      this%seg_outflow(n) = DZERO
      this%inflow_ts(n) = DZERO
      this%outflow_ts(n) = DZERO
      this%seg_current_sum(n) = DZERO

      this%extoutflow(n) = DZERO
      this%qsto(n) = DZERO

    end do

    ! -- Return
    return
  end subroutine allocate_arrays
    
  !> @brief load data from IDM to package
  !<
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
    call mem_set_value(this%iprflow, 'IPRFLOW', idmMemoryPath, found%iprflow)
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

    if (found%iprflow) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if

    if (found%ipakcb) then
      write (this%iout, '(4x,a)') 'Cell-by-cell flow information will be printed &
                                  &to listing file whenever ICBCFL is not zero.'
    end if


    write (this%iout, '(1x,a,/)') 'End Setting MMR Options'

  end subroutine log_options

  !> @brief copy griddata from IDM to package
  !<
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
    call mem_set_value(this%iseg_order, 'ISEG_ORDER', idmMemoryPath, map, found%iseg_order)
    call mem_set_value(this%qoutflow0, 'QOUTFLOW0', idmMemoryPath, map, found%qoutflow0)
    call mem_set_value(this%mann_n, 'MANN_N', idmMemoryPath, map, found%mann_n)
    call mem_set_value(this%seg_depth, 'SEG_DEPTH', idmMemoryPath, map, found%seg_depth)
    call mem_set_value(this%seg_slope, 'SEG_SLOPE', idmMemoryPath, map, found%seg_slope)
    call mem_set_value(this%x_coef, 'X_COEF', idmMemoryPath, map, found%x_coef)
    !
    ! -- ensure ISEG_ORDER was found
    if (.not. found%iseg_order) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: ISEG_ORDER not found.'
      call store_error(errmsg)
    end if
    !
    ! -- ensure QOUTFLOW0 was found
    if (.not. found%qoutflow0) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: QOUTFLOW0 not found.'
      call store_error(errmsg)
    end if
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

    if (found%iseg_order) then
      write (this%iout, '(4x,a)') 'ISEG_ORDER set from input file'
    end if

    if (found%qoutflow0) then
      write (this%iout, '(4x,a)') 'QOUTFLOW0 set from input file'
    end if

    if (found%mann_n) then
      write (this%iout, '(4x,a)') 'MANN_N set from input file'
    end if

    if (found%seg_depth) then
      write (this%iout, '(4x,a)') 'SEG_DEPTH set from input file'
    end if

    if (found%seg_slope) then
      write (this%iout, '(4x,a)') 'SEG_SLOPE set from input file'
    end if

    if (found%x_coef) then
      write (this%iout, '(4x,a)') 'X_COEF set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting MMR Griddata'

  end subroutine log_griddata

  !> @brief deallocate memory
  !<
  subroutine mmr_ar(this)
    ! -- modules
    ! -- dummy
    class(SnfMmrType) :: this !< this instance
    !

    call this%mmr_init_data()

    return
  end subroutine mmr_ar

  subroutine mmr_ad(this, irestore)
    use TdisModule, only: kper, kstp
    !
    class(SnfMmrType) :: this
    integer(I4B), intent(in) :: irestore
    integer(I4B) :: n
    !
    ! todo: need to handle restore if time step failed
    if (irestore == 0) then
      do n = 1, this%disl%nodes
        this%seg_inflow0(n) = this%seg_inflow(n)
      end do
    else
      ! -- todo: reset states back to start of 
      !          time step
    end if
    !
    ! -- Return
    return
  end subroutine mmr_ad

  !> @brief solve
  !<
  subroutine mmr_solve(this, rhs)
    ! -- modules
    ! -- dummy
    class(SnfMmrType) :: this !< this instance
    real(DP), dimension(:), intent(in) :: rhs !< right-hand-side vector of boundary package inflows
    ! -- local
    integer(I4B) :: n
    real(DP) :: q
    !
    ! -- Copy rhs values into seg_lat_inflow
    do n = 1, this%dis%nodes
      q = -rhs(n)
      this%seg_lat_inflow(n) = q
    end do

    ! -- todo: this is a hack to assign flow
    !          to the first segment
    this%seg_inflow0_in(1) = -rhs(1)

    call calc_muskingum_mann( &
      this%dis%nodes, & !nseg, &  ! in int32
      this%iseg_order, & !segment_order, &  ! in int64 size(nseg)
      this%disl%tosegment, & !to_segment, &  ! in int64 size(nseg)
      this%seg_lat_inflow, & !seg_lateral_inflow, &  ! in float64 size(nseg)
      this%seg_inflow0_in, & !seg_inflow0_in, &  ! in float64 size(nseg)
      this%outflow_ts_in, &  !outflow_ts_in, &  ! in float64 size(nseg)
      this%tsi, &  !tsi, &  ! in int64 size(nseg)
      this%ts, &  !ts, &  ! in float64 size(nseg)
      this%c0, &  !c0, &  ! in float64 size(nseg)
      this%c1, &  !c1, &  ! in float64 size(nseg)
      this%c2, &  !c2, &  ! in float64 size(nseg)
      this%seg_up_inflow, &  !seg_upstream_inflow, &  ! out float64 size(nseg)
      this%seg_inflow0, &  !seg_inflow0, &  ! out float64 size(nseg)
      this%seg_inflow, &  !seg_inflow, &  ! out float64 size(nseg)
      this%seg_outflow, &  !seg_outflow, &  ! out float64 size(nseg)
      this%inflow_ts, &  !inflow_ts, &  ! out float64 size(nseg)
      this%outflow_ts, &  !outflow_ts, &  ! out float64 size(nseg)
      this%seg_current_sum &  !seg_current_sum &  ! out float64 size(nseg)
    )

    ! -- return
    return
  end subroutine mmr_solve

  subroutine mmr_cq(this, flowja)
    ! -- dummy
    class(SnfMmrType) :: this
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
    !
    ! -- Transfer seg_outflow into flowja
    do n = 1, this%dis%nodes
      do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
        m = this%dis%con%ja(ipos)
        if (m < n) cycle
        qnm = this%seg_outflow(n)
        flowja(ipos) = -qnm
        flowja(this%dis%con%isym(ipos)) = qnm
      end do
    end do

    ! Transfer any flows leaving tosegment 0 into extoutflow
    do n = 1, this%dis%nodes
      if (this%disl%tosegment(n) == 0) then
        this%extoutflow(n) = -this%seg_outflow(n)
      else
        this%extoutflow(n) = DZERO
      end if
    end do

    ! Transfer storage terms into qsto
    do n = 1, this%dis%nodes
      this%qsto(n) = this%seg_outflow(n) - this%seg_inflow(n)
    end do

    !
    ! -- Return
    return
  end subroutine mmr_cq

  !> @ brief Model budget calculation for package
  !!
  !!  Budget calculation for the MMR package components. Components include
  !!  external outflow
  !!
  !<
  subroutine mmr_bd(this, isuppress_output, model_budget)
    ! -- modules
    use TdisModule, only: delt
    use BudgetModule, only: BudgetType, rate_accumulator
    ! -- dummy variables
    class(SnfMmrType) :: this !< SnfMmrType object
    integer(I4B), intent(in) :: isuppress_output !< flag to suppress model output
    type(BudgetType), intent(inout) :: model_budget !< model budget object
    ! -- local variables
    real(DP) :: rin
    real(DP) :: rout
    !
    ! -- Add external outflow rates to model budget
    call rate_accumulator(this%extoutflow, rin, rout)
    call model_budget%addentry(rin, rout, delt, '             MMR', &
                               isuppress_output, '     EXT-OUTFLOW')
    !
    ! -- Add storage rates to model budget
    call rate_accumulator(this%qsto, rin, rout)
    call model_budget%addentry(rin, rout, delt, '             MMR', &
                               isuppress_output, '         STORAGE')
    !
    ! -- return
    return
  end subroutine mmr_bd

  !> @ brief save flows for package
  !<
  subroutine mmr_save_model_flows(this, flowja, icbcfl, icbcun)
    ! -- dummy
    class(SnfMmrType) :: this
    real(DP), dimension(:), intent(in) :: flowja
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: icbcun
    ! -- local
    integer(I4B) :: ibinun
    ! -- formats
    !
    ! -- Set unit number for binary output
    if (this%ipakcb < 0) then
      ibinun = icbcun
    elseif (this%ipakcb == 0) then
      ibinun = 0
    else
      ibinun = this%ipakcb
    end if
    if (icbcfl == 0) ibinun = 0
    !
    ! -- Write the face flows if requested
    if (ibinun /= 0) then
      call this%dis%record_connection_array(flowja, ibinun, this%iout)
    end if
    !
    ! -- Return
    return
  end subroutine mmr_save_model_flows

  !> @ brief print flows for package
  !<
  subroutine mmr_print_model_flows(this, ibudfl, flowja)
    ! -- modules
    use TdisModule, only: kper, kstp
    use ConstantsModule, only: LENBIGLINE
    ! -- dummy
    class(SnfMmrType) :: this
    integer(I4B), intent(in) :: ibudfl
    real(DP), intent(inout), dimension(:) :: flowja
    ! -- local
    character(len=LENBIGLINE) :: line
    character(len=30) :: tempstr
    integer(I4B) :: n, ipos, m
    real(DP) :: qnm
    ! -- formats
    character(len=*), parameter :: fmtiprflow = &
      &"(/,4x,'CALCULATED INTERCELL FLOW FOR PERIOD ', i0, ' STEP ', i0)"
! ------------------------------------------------------------------------------
    !
    ! -- Write flowja to list file if requested
    if (ibudfl /= 0 .and. this%iprflow > 0) then
      write (this%iout, fmtiprflow) kper, kstp
      do n = 1, this%dis%nodes
        line = ''
        call this%dis%noder_to_string(n, tempstr)
        line = trim(tempstr)//':'
        do ipos = this%dis%con%ia(n) + 1, this%dis%con%ia(n + 1) - 1
          m = this%dis%con%ja(ipos)
          call this%dis%noder_to_string(m, tempstr)
          line = trim(line)//' '//trim(tempstr)
          qnm = flowja(ipos)
          write (tempstr, '(1pg15.6)') qnm
          line = trim(line)//' '//trim(adjustl(tempstr))
        end do
        write (this%iout, '(a)') trim(line)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine mmr_print_model_flows

  !> @brief deallocate memory
  !<
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
    call mem_deallocate(this%iseg_order)
    call mem_deallocate(this%qoutflow0)
    call mem_deallocate(this%mann_n)
    call mem_deallocate(this%seg_depth)
    call mem_deallocate(this%seg_slope)
    call mem_deallocate(this%x_coef)

    ! -- input arguments to calc_muskingum_mann routine
    call mem_deallocate(this%seg_lat_inflow)
    call mem_deallocate(this%seg_inflow0_in)
    call mem_deallocate(this%outflow_ts_in)
    call mem_deallocate(this%tsi)
    call mem_deallocate(this%ts)
    call mem_deallocate(this%c0)
    call mem_deallocate(this%c1)
    call mem_deallocate(this%c2)

    ! -- output arguments to calc_muskingum_mann routine
    call mem_deallocate(this%seg_up_inflow)
    call mem_deallocate(this%seg_inflow0)
    call mem_deallocate(this%seg_inflow)
    call mem_deallocate(this%seg_outflow0)
    call mem_deallocate(this%seg_outflow)
    call mem_deallocate(this%inflow_ts)
    call mem_deallocate(this%outflow_ts)
    call mem_deallocate(this%seg_current_sum)

    call mem_deallocate(this%extoutflow)
    call mem_deallocate(this%qsto)

    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Return
    return
  end subroutine mmr_da

  !> @brief initialize mmr data
  !<
  subroutine mmr_init_data(this)
    ! -- modules
    ! -- dummy
    class(SnfMmrType) :: this !< this instance
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: ito_seg
    real(DP) :: v
    real(DP) :: tfact
    real(DP) :: d
    real(DP) :: dk
    real(DP) :: xk
    real(DP) :: halfts
    real(DP), dimension(:), allocatable :: kcoef

    ! allocate temporary storage
    allocate(kcoef(this%dis%nodes))

    ! -- todo: if seg_slope < 1.e-7 then set to 0.0001.  Not a good idea.
    !    should implement an input check instead.

    ! initialize Kcoef to 24.0 for segments with zero velocities
    ! this is different from PRMS, which relied on divide by zero resulting
    ! in a value of infinity that when evaluated relative to a maximum
    ! desired Kcoef value of 24 would be reset to 24. This approach is
    ! equivalent and avoids the occurence of a divide by zero.
    do n = 1, this%dis%nodes
      kcoef(n) = 24.d0
    end do

    ! # only calculate Kcoef for cells with velocities greater than zero
    ! todo: did not implement LAKE segment adjustment
    ! Kcoef = np.where(
    !     self.segment_type == SegmentType.LAKE.value, 24.0, Kcoef
    ! )
    tfact = 60.d0 * 60.d0
    do n = 1, this%dis%nodes
      !
      ! calculate velocity in meters per hour
      v = velocity_func( &
        this%mann_n(n), &
        this%seg_slope(n), &
        this%seg_depth(n), &
        tfact&
      )
      if (v > DZERO) then
        dk = this%disl%segment_length(n) / v
        if (dk < 1.d-2) then
          dk = 1.d-2
        else if (dk > 24.d0) then
          dk = 24.d0
        end if
        kcoef(n) = dk
      end if
    end do

    do n = 1, this%dis%nodes
      this%ts(n) = DONE
      this%tsi(n) = 1      
    end do
    do n = 1, this%dis%nodes
      dk = kcoef(n)
      if (dk < 1.0) then
        this%tsi(n) = -1
      else if (dk < 2.d0) then
        this%ts(n) = DONE
        this%tsi(n) = 1
      else if (dk < 3.d0) then
        this%ts(n) = DTWO
        this%tsi(n) = 2
      else if (dk < 4.d0) then
        this%ts(n) = 3.d0
        this%tsi(n) = 3
      else if (dk < 6.d0) then
        this%ts(n) = 4.d0
        this%tsi(n) = 4
      else if (dk < 8.d0) then
        this%ts(n) = 6.d0
        this%tsi(n) = 6
      else if (dk < 12.d0) then
        this%ts(n) = 8.d0
        this%tsi(n) = 8
      else if (dk < 24.d0) then
        this%ts(n) = 12.d0
        this%tsi(n) = 12
      else
        this%ts(n) = 24.d0
        this%tsi(n) = 24
      end if
    end do

    do n = 1, this%dis%nodes
      xk = kcoef(n) * this%x_coef(n)
      halfts = DHALF * this%ts(n)
      d = kcoef(n) - xk + halfts
      if (abs(d) < 1.d-6) d = 1.d-4
      this%c0(n) = (-xk + halfts) / d
      this%c1(n) = (xk + halfts) / d
      this%c2(n) = (kcoef(n) - xk - halfts) / d
    end do

    ! -- Short travel time
    do n = 1, this%dis%nodes
      if (this%c2(n) < DZERO) then
        this%c1(n) = this%c1(n) + this%c2(n)
        this%c2(n) = DZERO
      end if
    end do

    ! -- Long travel time
    do n = 1, this%dis%nodes
      if (this%c0(n) < DZERO) then
        this%c1(n) = this%c1(n) + this%c0(n)
        this%c0(n) = DZERO
      end if
    end do

    ! -- todo: hack for first segment
    this%seg_inflow0_in(1) = this%qoutflow0(1)

    ! -- Set initial conditions
    do n = 1, this%dis%nodes
      this%outflow_ts_in(n) = this%qoutflow0(n)
      ito_seg = this%disl%tosegment(n)
      if (ito_seg > 0) then
        this%seg_inflow0_in(ito_seg) = &
          this%seg_inflow0_in(ito_seg) + this%qoutflow0(n)
      end if
    end do

    ! clean up
    deallocate(kcoef)

    ! -- return
    return
  end subroutine mmr_init_data

  function velocity_func(dmann, dslope, ddepth, tfact) result(v)
    real(DP) :: v
    real(DP), intent(in) :: dmann
    real(DP), intent(in) :: dslope
    real(DP), intent(in) :: ddepth
    real(DP), intent(in) :: tfact
    v = (  (DONE / dmann) * sqrt(dslope) * ddepth ** (DTWO / DTHREE) &
        ) * tfact
  end function velocity_func

! Muskingum routing function that calculates the upstream inflow and
! outflow for each segment

! Args:
!     segment_order: segment routing order
!     to_segment: downstream segment for each segment
!     seg_lateral_inflow: segment lateral inflow
!     seg_inflow0: previous segment inflow variable (internal calculations)
!     outflow_ts_in: outflow timeseries variable (internal calculations)
!     tsi: integer flood wave travel time
!     ts: float version of integer flood wave travel time
!     c0: Muskingum c0 variable
!     c1: Muskingum c1 variable
!     c2: Muskingum c2 variable

! Returns:
!     seg_upstream_inflow: inflow for each segment for the current day
!     seg_inflow0: segment inflow variable
!     seg_inflow: segment inflow variable
!     seg_outflow0: outflow for each segment for the current day
!     seg_outflow: outflow for each segment for the current day
!     inflow_ts: inflow timeseries variable
!     outflow_ts: outflow timeseries variable (internal calculations)
!     seg_current_sum: summation variable

  pure subroutine calc_muskingum_mann( &
  nseg, &  ! in
  segment_order, &  ! in
  to_segment, &  ! in
  seg_lateral_inflow, &  ! in
  seg_inflow0_in, &  ! in
  outflow_ts_in, &  ! in
  tsi, &  ! in
  ts, &  ! in
  c0, &  ! in
  c1, &  ! in
  c2, &  ! in
  seg_upstream_inflow, &  ! out
  seg_inflow0, &  ! out
  seg_inflow, &  ! out
  seg_outflow, &  ! out
  inflow_ts, &  ! out
  outflow_ts, &  ! out
  seg_current_sum &  ! out
)

  implicit none
  
  ! cdl -- changed all 8 byte integers to 4 byte
  ! cdl -- removed conversion of zero-based

  ! Inputs
  integer(I4B), intent(in) :: nseg
  integer(I4B), intent(in), dimension(nseg) :: &
      segment_order, to_segment, tsi
  real(kind=8), intent(in), dimension(nseg) :: &
      seg_lateral_inflow, seg_inflow0_in, outflow_ts_in, ts, &
      c0, c1, c2
  
  ! Outputs
  real(kind=8), intent(out), dimension(nseg) :: &
      seg_upstream_inflow,  seg_inflow0,  seg_inflow, seg_outflow, inflow_ts, &
      outflow_ts,  seg_current_sum

  ! Locals
  real(kind=8), dimension(nseg) :: seg_outflow0
  integer(I4B) :: ihr, jj
  ! cdl -- changed following from kind=8 to kind=4
  integer(I4B) :: jseg, to_seg, remainder
  real(kind=8) :: seg_current_inflow

  real(kind=8), parameter :: zero = 0.0000000000000000000000000000000

  ! Initialize variables
 
  ! In to out copies.
  seg_inflow0 = seg_inflow0_in
  outflow_ts = outflow_ts_in
  
  ! initialize variables for the day
  seg_inflow = zero
  seg_outflow = zero
  inflow_ts = zero
  seg_current_sum = zero
  
  do ihr = 0, 23
      seg_upstream_inflow = zero

      do jj = 1, nseg
          jseg = segment_order(jj)  ! + 1  ! convert from zero based in python
          
          ! current inflow to the segment is the time-weighted average
          ! of the outflow of the upstream segments and the lateral HRU
          ! inflow plus any gains
          seg_current_inflow = seg_lateral_inflow(jseg) + seg_upstream_inflow(jseg)

          ! todo: evaluate if obsin_segment needs to be implemented -
          !  would be needed needed if headwater basins are not included
          !  in a simulation
          ! seg_current_inflow += seg_upstream_inflow(jseg)

          seg_inflow(jseg) = seg_inflow(jseg) + seg_current_inflow
          inflow_ts(jseg) = inflow_ts(jseg) + seg_current_inflow
          seg_current_sum(jseg) = seg_current_sum(jseg) + seg_upstream_inflow(jseg)

          remainder = modulo(ihr + 1, tsi(jseg))
          if (remainder == 0) then
              ! segment routed on current hour
              inflow_ts(jseg) = inflow_ts(jseg) / ts(jseg)

              if (tsi(jseg) > 0) then

                  ! todo: evaluated if denormal results should be dealt with
                  
                  ! Muskingum routing equation
                  outflow_ts(jseg) = ( &
                      inflow_ts(jseg) * c0(jseg) &
                      + seg_inflow0(jseg) * c1(jseg) & 
                      + outflow_ts(jseg) * c2(jseg) )
                  
              else
                  ! travel time is 1 hour or less so outflow is set
                  ! equal to the inflow - outflow_ts is the value for
                  ! the previous hour
                  outflow_ts(jseg) = inflow_ts(jseg)
                  
              end if
                      
              ! previous inflow is equal to inflow_ts from the previous
              ! routed time step
              seg_inflow0(jseg) = inflow_ts(jseg)

              ! upstream inflow is used, reset it to zero so a new
              ! average can be calculated next routing time step
              inflow_ts(jseg) = zero

          end if
          
          ! todo: evaluate if obsout_segment needs to be implemented -
          !  would be needed needed fixing ourflow to observed data is
          !  required in a simulation

          ! todo: water use

          ! segment outflow (the mean daily flow rate for each segment)
          ! will be the average of hourly values
          seg_outflow(jseg) = seg_outflow(jseg) + outflow_ts(jseg)

          ! previous segment outflow is equal to the inflow_ts on the
          ! previous routed timestep
          seg_outflow0(jseg) = outflow_ts(jseg)
          
          ! add current time step flow rate to the upstream flow rate
          ! for the segment this segment is connected to
          to_seg = to_segment(jseg) ! cdl + 1 ! convert from zero based in python
          if (to_seg >= 1) then
              seg_upstream_inflow(to_seg) = &
                  seg_upstream_inflow(to_seg) + outflow_ts(jseg)
          endif

      end do
  end do

  seg_outflow = seg_outflow / 24.0
  seg_inflow = seg_inflow / 24.0
  seg_upstream_inflow = seg_current_sum / 24.0
      
end subroutine calc_muskingum_mann  

end module SnfMmrModule