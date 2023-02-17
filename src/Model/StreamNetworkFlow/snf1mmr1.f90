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
    real(DP), dimension(:), pointer, contiguous :: k_coef => null() !< manning K coefficient
    real(DP), dimension(:), pointer, contiguous :: x_coef => null() !< routing weighting factor

    ! -- input arguments to calc_muskingum_mann routine
    real(DP), dimension(:), pointer, contiguous :: inflow_old => null() !< inflow to each segment for last time step
    real(DP), dimension(:), pointer, contiguous :: inflow_new => null() !< inflow to each segment for current time step
    real(DP), dimension(:), pointer, contiguous :: outflow_old => null() !< outflow from each segment for last time step
    real(DP), dimension(:), pointer, contiguous :: outflow_new => null() !< outflow from each segment for current time step
    real(DP), dimension(:), pointer, contiguous :: c0 => null() !< Muskingum c0 variable
    real(DP), dimension(:), pointer, contiguous :: c1 => null() !< Muskingum c1 variable
    real(DP), dimension(:), pointer, contiguous :: c2 => null() !< Muskingum c2 variable

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
    call mem_allocate(this%k_coef, this%dis%nodes, 'K_COEF', this%memoryPath)
    call mem_allocate(this%x_coef, this%dis%nodes, 'X_COEF', this%memoryPath)

    ! -- input arguments to calc_muskingum_mann routine
    call mem_allocate(this%inflow_old, this%dis%nodes, 'INFLOW_OLD', this%memoryPath)
    call mem_allocate(this%inflow_new, this%dis%nodes, 'INFLOW_NEW', this%memoryPath)
    call mem_allocate(this%outflow_old, this%dis%nodes, 'OUTFLOW_OLD', this%memoryPath)
    call mem_allocate(this%outflow_new, this%dis%nodes, 'OUTFLOW_NEW', this%memoryPath)
    call mem_allocate(this%c0, this%dis%nodes, 'C0', this%memoryPath)
    call mem_allocate(this%c1, this%dis%nodes, 'C1', this%memoryPath)
    call mem_allocate(this%c2, this%dis%nodes, 'C2', this%memoryPath)

    ! -- budgeting variables
    call mem_allocate(this%extoutflow, this%dis%nodes, 'EXTOUTFLOW', this%memoryPath)
    call mem_allocate(this%qsto, this%dis%nodes, 'QSTO', this%memoryPath)

    do n = 1, this%dis%nodes

      this%iseg_order(n) = 0
      this%qoutflow0(n) = DZERO
      this%k_coef(n) = DZERO
      this%x_coef(n) = DZERO

      this%inflow_old(n) = DZERO
      this%inflow_new(n) = DZERO
      this%outflow_old(n) = DZERO
      this%outflow_new(n) = DZERO
      this%c0(n) = DZERO
      this%c1(n) = DZERO
      this%c2(n) = DZERO

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
    call mem_set_value(this%k_coef, 'K_COEF', idmMemoryPath, map, found%k_coef)
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
    if (.not. found%k_coef) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: K_COEF not found.'
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

    if (found%k_coef) then
      write (this%iout, '(4x,a)') 'K_COEF set from input file'
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
    ! Advance forward or backward depending on irestore
    if (irestore == 0) then
      do n = 1, this%disl%nodes
        this%inflow_old(n) = this%inflow_new(n)
        this%outflow_old(n) = this%outflow_new(n)
      end do
    else
      do n = 1, this%disl%nodes
        this%inflow_new(n) = this%inflow_old(n)
        this%outflow_new(n) = this%outflow_old(n)
      end do
    end if
    !
    ! -- Return
    return
  end subroutine mmr_ad

  !> @brief solve
  !<
  subroutine mmr_solve(this, rhs)
    ! -- modules
    use TdisModule, only: delt
    ! -- dummy
    class(SnfMmrType) :: this !< this instance
    real(DP), dimension(:), intent(in) :: rhs !< right-hand-side vector of boundary package inflows
    ! -- local
    integer(I4B) :: n
    real(DP) :: q
    real(DP) :: dtoverk
    real(DP) :: twox
    real(DP) :: two_oneminusx
    real(DP) :: denom
    !
    ! -- Copy rhs values into seg_lat_inflow
    do n = 1, this%dis%nodes
      if (n == 1) then  ! hack to have inflow_new keep rhs value
        q = -rhs(n)
        this%inflow_new(n) = q
      else
        this%inflow_new(n) = DZERO
      end if
    end do

    ! -- Calculate muskingum C coefficients, using Ponce versions
    do n = 1, this%dis%nodes
      dtoverk = delt / this%k_coef(n)
      twox = DTWO * this%x_coef(n)
      two_oneminusx = DTWO * (DONE - this%x_coef(n))
      denom = two_oneminusx + dtoverk
      this%c0(n) = (dtoverk - twox) / denom
      this%c1(n) = (dtoverk + twox) / denom
      this%c2(n) = (two_oneminusx - dtoverk) / denom
    end do

    call calc_muskingum(this%disl%tosegment, this%iseg_order, this%inflow_old, &
                        this%outflow_old, this%inflow_new, this%outflow_new, &
                        this%c0, this%c1, this%c2)

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
        qnm = this%outflow_new(n)
        flowja(ipos) = -qnm
        flowja(this%dis%con%isym(ipos)) = qnm
      end do
    end do

    ! Transfer any flows leaving tosegment 0 into extoutflow
    do n = 1, this%dis%nodes
      if (this%disl%tosegment(n) == 0) then
        this%extoutflow(n) = -this%outflow_new(n)
      else
        this%extoutflow(n) = DZERO
      end if
    end do

    ! Transfer storage terms into qsto
    do n = 1, this%dis%nodes
      this%qsto(n) = this%outflow_new(n) - this%inflow_new(n)
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
    call mem_deallocate(this%k_coef)
    call mem_deallocate(this%x_coef)

    ! -- input arguments to calc_muskingum_mann routine
    call mem_deallocate(this%inflow_old)
    call mem_deallocate(this%inflow_new)
    call mem_deallocate(this%outflow_old)
    call mem_deallocate(this%outflow_new)
    call mem_deallocate(this%c0)
    call mem_deallocate(this%c1)
    call mem_deallocate(this%c2)

    ! -- budget variables
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

    ! -- Set inflow_new and outflow_new from qoutflow0
    !    These will be copied into inflow_old and outflow_old
    !    as part of the first advance
    do n = 1, this%dis%nodes
      this%outflow_new(n) = this%qoutflow0(n)
      ito_seg = this%disl%tosegment(n)
      if (ito_seg > 0) then
        this%inflow_new(ito_seg) = &
          this%inflow_new(ito_seg) + this%qoutflow0(n)
      end if
    end do

    ! TODO: HACK, this must be done from initial conditions somehow
    this%inflow_new(1) = this%outflow_new(1)

    ! -- return
    return
  end subroutine mmr_init_data

  subroutine calc_muskingum(itosegment, iseg_order, inflow_old, outflow_old, &
                            inflow_new, outflow_new, c0, c1, c2)
    integer(I4B), dimension(:), intent(in) :: itosegment
    integer(I4B), dimension(:), intent(in) :: iseg_order
    real(DP), dimension(:), intent(in) :: inflow_old
    real(DP), dimension(:), intent(in) :: outflow_old
    real(DP), dimension(:), intent(inout) :: inflow_new
    real(DP), dimension(:), intent(inout) :: outflow_new
    real(DP), dimension(:), intent(in) :: c0
    real(DP), dimension(:), intent(in) :: c1
    real(DP), dimension(:), intent(in) :: c2
    ! TODO: HACK NEED LATERAL INFLOW
    integer(I4B) :: n
    integer(I4B) :: j
    integer(I4B) :: i
    real(DP) :: qoutflow
    real(DP) :: qinflow

    do n = 1, size(itosegment)
      j = iseg_order(n)
      qinflow = inflow_new(j) ! todo: hack + qlateral(j)
      qoutflow = c0(j) * qinflow + c1(j) * inflow_old(j) + c2(j) * outflow_old(j)
      outflow_new(j) = qoutflow
      i = itosegment(j)
      if (i > 0) then
          inflow_new(i) = inflow_new(i) + qoutflow
      end if
    end do

  end subroutine calc_muskingum

end module SnfMmrModule