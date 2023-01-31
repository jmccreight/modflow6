
!> @brief Stream Network Flow (SNF) Module
!!
!! This module contains the SNF Model
!!
!! Status and remaining tasks
!!   ONGOING -- Implement SNF infrastructure
!!   DONE -- Implement Explicit Model Solution (EMS6) for handle explicit models
!!   DONE -- Implement DISL Package
!!   ONGOING -- Implement MMR Package
!!   DONE -- Use MMR package to solve a flow problem in combination with DISL and FLW
!!   DONE -- Implement FLW Package to handle lateral and point inflows
!!   DONE -- Transfer results into the flowja vector
!!   DONE -- Implement strategy for storing outflow terms and getting them into budget
!!   Implement SNF and FLW advance routines to handle transient problems
!!   Implement output control
!!   Flopy support for DISL and DISL binary grid file
!!   Implement storage terms and getting them into budget
!!   Observations
!!   mf6io guide
!!   Initial conditions?
!!   Deal with the timestep and subtiming issues
!!   Rework the Iterative Model Solution (IMS6) to handle both implicit and explicit models
!!   Mover support?
!!   SNF-SNF Exchange
!!   SNF-SNF Exchange in parallel
!!   Use dag_module to calculate iseg_order (if iseg_order not specified by user)
!! 
!<
module SnfModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, LENFTYPE
  use InputOutputModule, only: ParseLine, upcase
  use SimVariablesModule, only: errmsg
  use MemoryManagerModule, only: mem_allocate
  use VersionModule, only: write_listfile_header
  use BaseModelModule, only: BaseModelType
  use NumericalModelModule, only: NumericalModelType
  use BndModule, only: BndType, AddBndToList, GetBndFromList
  use SnfMmrModule, only: SnfMmrType
  use BudgetModule, only: BudgetType

  implicit none

  private
  public :: snf_cr

  type, extends(NumericalModelType) :: SnfModelType
    type(SnfMmrType), pointer :: mmr => null() !< muskingum-manning routing package
    type(BudgetType), pointer :: budget => null() ! budget object
    integer(I4B), pointer :: inmmr => null() ! unit number MMR
  contains
    procedure :: allocate_scalars
    procedure :: allocate_arrays
    procedure :: model_df => snf_df
    procedure :: model_ar => snf_ar
    procedure :: model_rp => snf_rp
    procedure :: model_ad => snf_ad
    procedure :: model_solve => snf_solve !< routine for solving this model for the current time step
    procedure :: model_cq => snf_cq
    procedure :: model_bd => snf_bd
    procedure :: model_ot => snf_ot
    procedure :: model_da => snf_da
    procedure :: snf_ot_flow
    procedure :: snf_ot_bdsummary
    procedure :: package_create
    procedure :: ftype_check
    procedure :: load_input_context => snf_load_input_context
  end type SnfModelType

    ! -- Module variables constant for simulation
  integer(I4B), parameter :: NIUNIT = 100
  character(len=LENFTYPE), dimension(NIUNIT) :: cunit
  data cunit/'DISL6', 'MMR6 ', 'FLW6 ', '     ', '     ', & !  5
            &95*'     '/

  contains

  !> @brief Create a new stream network flow model object
  !!
  !! (1) creates model object and add to modellist
  !! (2) assign values
  !!
  !<
  subroutine snf_cr(filename, id, modelname)
    ! -- modules
    use ListsModule, only: basemodellist
    use MemoryHelperModule, only: create_mem_path
    use BaseModelModule, only: AddBaseModelToList
    use SimModule, only: store_error, count_errors
    use GenericUtilitiesModule, only: write_centered
    use ConstantsModule, only: LINELENGTH, LENPACKAGENAME
    use MemoryManagerModule, only: mem_allocate
    use SnfDislModule, only: disl_cr
    use SnfMmrModule, only: mmr_cr
    use BudgetModule, only: budget_cr
    use NameFileModule, only: NameFileType
    ! -- dummy
    character(len=*), intent(in) :: filename
    integer(I4B), intent(in) :: id
    character(len=*), intent(in) :: modelname
    ! -- local
    integer(I4B) :: indisl
    integer(I4B) :: ipakid, i, j, iu, ipaknum
    character(len=LENPACKAGENAME) :: pakname
    type(NameFileType) :: namefile_obj
    type(SnfModelType), pointer :: this
    class(BaseModelType), pointer :: model
    integer(I4B) :: nwords
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    ! -- format
    !
    ! -- Allocate a new SNF Model (this) and add it to basemodellist
    allocate (this)
    !
    ! -- Set memory path before allocation in memory manager can be done
    this%memoryPath = create_mem_path(modelname)
    !
    call this%allocate_scalars(modelname)
    model => this
    call AddBaseModelToList(basemodellist, model)
    !
    ! -- Assign values
    this%filename = filename
    this%name = modelname
    this%macronym = 'SNF'
    this%id = id
    !
    ! -- Open namefile and set iout
    call namefile_obj%init(this%filename, 0)
    call namefile_obj%add_cunit(niunit, cunit)
    call namefile_obj%openlistfile(this%iout)
    !
    ! -- Write header to model list file
    call write_listfile_header(this%iout, 'STREAM NETWORK FLOW MODEL (SNF)')
    !
    ! -- Open files
    call namefile_obj%openfiles(this%iout)
    !
    ! -- SNF options
    if (size(namefile_obj%opts) > 0) then
      write (this%iout, '(1x,a)') 'NAMEFILE OPTIONS:'
    end if
    !
    ! -- Parse options in the SNF name file
    do i = 1, size(namefile_obj%opts)
      call ParseLine(namefile_obj%opts(i), nwords, words)
      call upcase(words(1))
      select case (words(1))
      case ('PRINT_INPUT')
        this%iprpak = 1
        write (this%iout, '(4x,a)') 'STRESS PACKAGE INPUT WILL BE PRINTED '// &
          'FOR ALL MODEL STRESS PACKAGES'
      case ('PRINT_FLOWS')
        this%iprflow = 1
        write (this%iout, '(4x,a)') 'PACKAGE FLOWS WILL BE PRINTED '// &
          'FOR ALL MODEL PACKAGES'
      case ('SAVE_FLOWS')
        this%ipakcb = -1
        write (this%iout, '(4x,a)') &
          'FLOWS WILL BE SAVED TO BUDGET FILE SPECIFIED IN OUTPUT CONTROL'
      case default
        write (errmsg, '(4x,a,a,a,a)') &
          'Unknown SNF namefile (', &
          trim(adjustl(this%filename)), ') option: ', &
          trim(adjustl(namefile_obj%opts(i)))
        call store_error(errmsg, terminate=.TRUE.)
      end select
    end do
    !
    ! -- Assign unit numbers to attached modules, and remove
    ! -- from unitnumber (by specifying 1 for iremove)
    !
    indisl = 0
    call namefile_obj%get_unitnumber('DISL6', indisl, 1)
    call namefile_obj%get_unitnumber('MMR6', this%inmmr, 1)
    ! call namefile_obj%get_unitnumber('OBS6', this%inobs, 1)
    !
    ! -- Check to make sure that required ftype's have been specified
    call this%ftype_check(namefile_obj, indisl)
    !
    ! -- Create discretization object
    if (indisl > 0) then
      call this%load_input_context('DISL6', this%name, 'DISL', indisl, &
                                   this%iout)
      call disl_cr(this%dis, this%name, indisl, this%iout)
    end if
    !
    ! -- Create utility objects
    call budget_cr(this%budget, this%name)
    !
    ! -- Muskingum Manning Routing Package
    if (this%inmmr > 0) then
      call this%load_input_context('MMR6', this%name, 'MMR', this%inmmr, this%iout)
      call mmr_cr(this%mmr, this%name, this%inmmr, this%dis, this%iout)
    end if
    !
    ! -- Create packages that are tied directly to model
    ! call ic_cr(this%ic, this%name, this%inic, this%iout, this%dis)
    ! call oc_cr(this%oc, this%name, this%inoc, this%iout)
    ! call snf_obs_cr(this%obs, this%inobs)
    !
    ! -- Create stress packages
    ipakid = 1
    do i = 1, niunit
      ipaknum = 1
      do j = 1, namefile_obj%get_nval_for_row(i)
        iu = namefile_obj%get_unitnumber_rowcol(i, j)
        call namefile_obj%get_pakname(i, j, pakname)
        call this%package_create(cunit(i), ipakid, ipaknum, pakname, iu, &
                                 this%iout)
        ipaknum = ipaknum + 1
        ipakid = ipakid + 1
      end do
    end do

    ! put this here in CR, but it is supposed to be in df.  It seems
    ! like now that dis can be loaded entirely up front in CR, it may
    ! be okay to leave this here now.
    !
    ! -- Assign or point model members to dis members
    !    this%neq will be incremented if packages add additional unknowns
    this%neq = this%dis%nodes
    this%nja = this%dis%nja
    this%ia => this%dis%con%ia
    this%ja => this%dis%con%ja
    !
    ! -- Allocate model arrays, now that neq and nja are known
    call this%allocate_arrays()
    !
    ! -- return
    return
  end subroutine snf_cr

  !> @brief Allocate memory for scalar members
  subroutine allocate_scalars(this, modelname)
    ! -- modules
    ! -- dummy
    class(SnfModelType) :: this
    character(len=*), intent(in) :: modelname
    !
    ! -- allocate members from parent class
    call this%NumericalModelType%allocate_scalars(modelname)
    !
    ! -- allocate members that are part of model class
    call mem_allocate(this%inmmr, 'INMMR', this%memoryPath)
    !
    this%inmmr = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  !> @brief Allocate memory for scalar members
  subroutine allocate_arrays(this)
    ! -- modules
    ! -- dummy
    class(SnfModelType) :: this
    integer(I4B) :: i
    !
    ! -- allocate members from parent class
    call this%NumericalModelType%allocate_arrays()
    !
    ! -- This is not a numerical solution, so x, rhs, and active
    !    are allocated by a numerical solution, so need to do it
    !    here.
    call mem_allocate(this%x, this%neq, 'X', this%memoryPath)
    call mem_allocate(this%rhs, this%neq, 'RHS', this%memoryPath)
    call mem_allocate(this%ibound, this%neq, 'IBOUND', this%memoryPath)
    !
    ! initialize arrays
    do i = 1, this%neq
      this%x(i) = DZERO
      this%rhs(i) = DZERO
      this%ibound(i) = 1
    end do
    !
    ! -- return
    return
  end subroutine allocate_arrays

  !> @brief Define packages of the model
  !<
  subroutine snf_df(this)
    ! -- modules
    ! -- dummy
    class(SnfModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    !
    call this%budget%budget_df(niunit, 'VOLUME', 'L**3')
    !
    ! -- Define packages and assign iout for time series managers
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_df(this%neq, this%dis)
    end do
    !
    ! -- Store information needed for observations
    !call this%obs%obs_df(this%iout, this%name, 'SNF', this%dis)
    !
    ! -- return
    return
  end subroutine snf_df

  !> @brief SNF Allocate and Read
  !<
  subroutine snf_ar(this)
    ! -- dummy
    class(SnfModelType) :: this
    ! -- locals
    integer(I4B), dimension(:), allocatable :: itemp
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Allocate and read modules attached to model
    ! if (this%inobs > 0) call this%obs%snf_obs_ar(this%ic, this%x, this%flowja)
    !
    ! -- Call dis_ar to write binary grid file
    allocate(itemp(this%dis%nodes))
    call this%dis%dis_ar(itemp)
    call this%mmr%mmr_ar()
    deallocate(itemp)
    !
    ! -- set up output control
    ! call this%oc%oc_ar(this%x, this%dis, this%npf%hnoflo)
    ! call this%budget%set_ibudcsv(this%oc%ibudcsv)
    !
    ! -- Package input files now open, so allocate and read
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%set_pointers(this%dis%nodes, this%ibound, this%x, &
                                this%xold, this%flowja)
      ! -- Read and allocate package
      call packobj%bnd_ar()
    end do
    !
    ! -- return
    return
  end subroutine snf_ar

  !> @brief Stream Network Flow Model Read and Prepare
  !!
  !! (1) calls package read and prepare routines
  !!
  !<
  subroutine snf_rp(this)
    ! -- modules
    use TdisModule, only: readnewdata
    ! -- dummy
    class(SnfModelType) :: this
    ! -- local
    class(BndType), pointer :: packobj
    integer(I4B) :: ip
    !
    ! -- Check with TDIS on whether or not it is time to RP
    if (.not. readnewdata) return
    !
    ! -- Read and prepare
    !if (this%inmmr > 0) call this%mmr%mmr_rp()
    !if (this%inoc > 0) call this%oc%oc_rp()
    !if (this%insto > 0) call this%sto%sto_rp()
    !if (this%inmvr > 0) call this%mvr%mvr_rp()
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_rp()
      !call packobj%bnd_rp_obs()
    end do
    !
    ! -- Return
    return
  end subroutine snf_rp

  !> @brief Stream Network Flow Model Time Step Advance
  !!
  !! (1) calls package advance subroutines
  !!
  !<
  subroutine snf_ad(this)
    ! -- modules
    use SimVariablesModule, only: isimcheck, iFailedStepRetry
    ! -- dummy
    class(SnfModelType) :: this
    class(BndType), pointer :: packobj
    ! -- local
    integer(I4B) :: irestore
    integer(I4B) :: ip, n
    !
    ! -- Reset state variable
    irestore = 0
    if (iFailedStepRetry > 0) irestore = 1
    if (irestore == 0) then
      !
      ! -- copy x into xold
      do n = 1, this%dis%nodes
        this%xold(n) = this%x(n)
      end do
    else
      !
      ! -- copy xold into x if this time step is a redo
      do n = 1, this%dis%nodes
        this%x(n) = this%xold(n)
      end do
    end if
    !
    ! -- Advance
    !if (this%inmmr > 0) call this%mmr%mmr_ad(this%dis%nodes, this%xold, &
    !                                         this%x, irestore)
    !if (this%insto > 0) call this%sto%sto_ad()
    !if (this%inmvr > 0) call this%mvr%mvr_ad()
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ad()
      if (isimcheck > 0) then
        call packobj%bnd_ck()
      end if
    end do
    !
    ! -- Push simulated values to preceding time/subtime step
    !call this%obs%obs_ad()
    !
    ! -- return
    return
  end subroutine snf_ad

  !> @brief Make explicit solve for this time step
  subroutine snf_solve(this)
    ! -- modules
    ! -- dummy
    class(SnfModelType) :: this
    ! -- local
    integer(I4B) :: n
    integer(I4B) :: i
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Initialize rhs accumulator
    do n = 1, this%dis%nodes
      this%rhs(n) = DZERO
    end do
    !
    ! -- Call boundary packages to set up inflows
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_cf()

      ! accumulate individual package rhs into this%rhs
      ! todo: will need to rethink if/how we use packobj
      !       rhs to store/add flows
      do i = 1, packobj%nbound
        n = packobj%nodelist(i)
        if (this%ibound(n) > 0) then
          this%rhs(n) = this%rhs(n) + packobj%rhs(i)
        end if
      end do

    end do

    ! -- MMR Solve
    call this%mmr%mmr_solve(this%rhs)

    ! -- return
    return
  end subroutine snf_solve

  !> @brief Calculate flow
  !<
  subroutine snf_cq(this, icnvg, isuppress_output)
    ! -- modules
    ! -- dummy
    class(SnfModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Construct the flowja array.  Flowja is calculated each time, even if
    !    output is suppressed.  (flowja is positive into a cell.)  The diagonal
    !    position of the flowja array will contain the flow residual after
    !    these routines are called, so each package is responsible for adding
    !    its flow to this diagonal position.
    do i = 1, this%nja
      this%flowja(i) = DZERO
    end do
    if (this%inmmr > 0) call this%mmr%mmr_cq(this%flowja)
    !
    ! -- Go through packages and call cq routines.  cf() routines are called
    !    first to regenerate non-linear terms to be consistent with the final
    !    head solution.
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_cf(reset_mover=.false.)
      call packobj%bnd_cq(this%x, this%flowja)
    end do
    !
    ! -- Return
    return
  end subroutine snf_cq

  !> @brief Model Budget
  !<
  subroutine snf_bd(this, icnvg, isuppress_output)
    ! -- modules
    use SparseModule, only: csr_diagsum
    ! -- dummy
    class(SnfModelType) :: this
    integer(I4B), intent(in) :: icnvg
    integer(I4B), intent(in) :: isuppress_output
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Finalize calculation of flowja by adding face flows to the diagonal.
    !    This results in the flow residual being stored in the diagonal
    !    position for each cell.
    call csr_diagsum(this%dis%con%ia, this%flowja)
    !
    ! -- Save the solution convergence flag
    this%icnvg = icnvg
    !
    ! -- Budget routines (start by resetting).  Sole purpose of this section
    !    is to add in and outs to model budget.  All ins and out for a model
    !    should be added here to this%budget.  In a subsequent exchange call,
    !    exchange flows might also be added.
    call this%budget%reset()
    if (this%inmmr > 0) call this%mmr%mmr_bd(isuppress_output, this%budget)
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_bd(this%budget)
    end do
    !
    ! -- Return
    return
  end subroutine snf_bd

  !> @brief GroundWater Flow Model Output
  subroutine snf_ot(this)
    ! -- modules
    use TdisModule, only: kstp, kper, tdis_ot, endofperiod
    ! -- dummy
    class(SnfModelType) :: this
    ! -- local
    integer(I4B) :: idvsave
    integer(I4B) :: idvprint
    integer(I4B) :: icbcfl
    integer(I4B) :: icbcun
    integer(I4B) :: ibudfl
    integer(I4B) :: ipflag
    ! -- formats
    character(len=*), parameter :: fmtnocnvg = &
      "(1X,/9X,'****FAILED TO MEET SOLVER CONVERGENCE CRITERIA IN TIME STEP ', &
      &I0,' OF STRESS PERIOD ',I0,'****')"
    !
    ! -- Set write and print flags
    idvsave = 0
    idvprint = 0
    icbcfl = 0
    ibudfl = 1 !cdl force to 1 -- should be zero when OC implemented
    ! if (this%oc%oc_save('HEAD')) idvsave = 1
    ! if (this%oc%oc_print('HEAD')) idvprint = 1
    ! if (this%oc%oc_save('BUDGET')) icbcfl = 1
    ! if (this%oc%oc_print('BUDGET')) ibudfl = 1
    ! icbcun = this%oc%oc_save_unit('BUDGET')
    !
    ! -- Override ibudfl and idvprint flags for nonconvergence
    !    and end of period
    ! ibudfl = this%oc%set_print_flag('BUDGET', this%icnvg, endofperiod)
    ! idvprint = this%oc%set_print_flag('HEAD', this%icnvg, endofperiod)
    !
    !   Calculate and save observations
    ! call this%gwf_ot_obs()
    ! !
    !   Save and print flows
    call this%snf_ot_flow(icbcfl, ibudfl, icbcun)
    ! !
    ! !   Save and print dependent variables
    ! call this%gwf_ot_dv(idvsave, idvprint, ipflag)
    !
    !   Print budget summaries
    call this%snf_ot_bdsummary(ibudfl, ipflag)
    !
    ! -- Timing Output; if any dependendent variables or budgets
    !    are printed, then ipflag is set to 1.
    if (ipflag == 1) call tdis_ot(this%iout)
    !
    ! -- Write non-convergence message
    if (this%icnvg == 0) then
      write (this%iout, fmtnocnvg) kstp, kper
    end if
    !
    ! -- Return
    return
  end subroutine snf_ot

  subroutine snf_ot_flow(this, icbcfl, ibudfl, icbcun)
    class(SnfModelType) :: this
    integer(I4B), intent(in) :: icbcfl
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(in) :: icbcun
    class(BndType), pointer :: packobj
    integer(I4B) :: ip

    ! -- Save GWF flows
    if (this%inmmr > 0) then
      call this%mmr%mmr_save_model_flows(this%flowja, icbcfl, icbcun)
    end if
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_model_flows(icbcfl=icbcfl, ibudfl=0, icbcun=icbcun)
    end do

    ! -- Save advanced package flows
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_package_flows(icbcfl=icbcfl, ibudfl=0)
    end do
    ! if (this%inmvr > 0) then
    !   call this%mvr%mvr_ot_saveflow(icbcfl, ibudfl)
    ! end if

    ! -- Print GWF flows
    if (this%inmmr > 0) call this%mmr%mmr_print_model_flows(ibudfl, this%flowja)
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_model_flows(icbcfl=icbcfl, ibudfl=ibudfl, icbcun=0)
    end do

    ! -- Print advanced package flows
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_package_flows(icbcfl=0, ibudfl=ibudfl)
    end do
    ! if (this%inmvr > 0) then
    !   call this%mvr%mvr_ot_printflow(icbcfl, ibudfl)
    ! end if

  end subroutine snf_ot_flow

  subroutine snf_ot_bdsummary(this, ibudfl, ipflag)
    use TdisModule, only: kstp, kper, totim
    class(SnfModelType) :: this
    integer(I4B), intent(in) :: ibudfl
    integer(I4B), intent(inout) :: ipflag
    class(BndType), pointer :: packobj
    integer(I4B) :: ip

    !
    ! -- Package budget summary
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_ot_bdsummary(kstp, kper, this%iout, ibudfl)
    end do

    ! ! -- mover budget summary
    ! if (this%inmvr > 0) then
    !   call this%mvr%mvr_ot_bdsummary(ibudfl)
    ! end if

    ! -- model budget summary
    if (ibudfl /= 0) then
      ipflag = 1
      call this%budget%budget_ot(kstp, kper, this%iout)
    end if

    ! -- Write to budget csv every time step
    call this%budget%writecsv(totim)

  end subroutine snf_ot_bdsummary

  !> @brief Deallocate
  subroutine snf_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(SnfModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
    !
    ! -- Internal flow packages deallocate
    call this%dis%dis_da()
    call this%mmr%mmr_da()
    call this%budget%budget_da()
    !
    ! -- Internal package objects
    deallocate (this%dis)
    deallocate (this%budget)
    !
    ! -- Boundary packages
    do ip = 1, this%bndlist%Count()
      packobj => GetBndFromList(this%bndlist, ip)
      call packobj%bnd_da()
      deallocate (packobj)
    end do
    !
    ! -- Scalars
    call mem_deallocate(this%inmmr)
    !
    ! -- NumericalModelType
    call this%NumericalModelType%model_da()
    !
    ! -- return
    return
  end subroutine snf_da

  !> @brief Create boundary condition packages for this model
  !<
  subroutine package_create(this, filtyp, ipakid, ipaknum, pakname, inunit, &
                            iout)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error
    use SnfFlwModule, only: flw_create
    ! -- dummy
    class(SnfModelType) :: this
    character(len=*), intent(in) :: filtyp
    integer(I4B), intent(in) :: ipakid
    integer(I4B), intent(in) :: ipaknum
    character(len=*), intent(in) :: pakname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    ! -- local
    class(BndType), pointer :: packobj
    class(BndType), pointer :: packobj2
    integer(I4B) :: ip
    !
    ! -- This part creates the package object
    select case (filtyp)
    case ('FLW6')
      call flw_create(packobj, ipakid, ipaknum, inunit, iout, this%name, pakname)
    case default
      write (errmsg, *) 'Invalid package type: ', filtyp
      call store_error(errmsg, terminate=.TRUE.)
    end select
    !
    ! -- Check to make sure that the package name is unique, then store a
    !    pointer to the package in the model bndlist
    do ip = 1, this%bndlist%Count()
      packobj2 => GetBndFromList(this%bndlist, ip)
      if (packobj2%packName == pakname) then
        write (errmsg, '(a,a)') 'Cannot create package.  Package name  '// &
          'already exists: ', trim(pakname)
        call store_error(errmsg, terminate=.TRUE.)
      end if
    end do
    call AddBndToList(this%bndlist, packobj)
    !
    ! -- return
    return
  end subroutine package_create

  !> @brief Check to make sure required input files have been specified
  subroutine ftype_check(this, namefile_obj, indis)
    ! -- modules
    use ConstantsModule, only: LINELENGTH
    use SimModule, only: store_error, count_errors
    use NameFileModule, only: NameFileType
    ! -- dummy
    class(SnfModelType) :: this
    type(NameFileType), intent(in) :: namefile_obj
    integer(I4B), intent(in) :: indis
    ! -- local
    integer(I4B) :: i, iu
    character(len=LENFTYPE), dimension(4) :: nodupftype = &
                                              (/'DISL6', 'MMR6 ', &
                                                'OC6  ', 'OBS6 '/)
! ------------------------------------------------------------------------------
    !
    ! -- Check for DISL, and MMR. Stop if not present.
    if (indis == 0) then
      write (errmsg, '(1x,a)') &
        'Discretization (DISL6) Package not specified.'
      call store_error(errmsg)
    end if
    if (this%inmmr == 0) then
      write (errmsg, '(1x,a)') &
        'Muskingum Manning Routing (MMR6) Package not specified.'
      call store_error(errmsg)
    end if
    if (count_errors() > 0) then
      write (errmsg, '(1x,a)') 'One or more required package(s) not specified.'
      call store_error(errmsg)
    end if
    !
    ! -- Check to make sure that some SNF packages are not specified more
    !    than once
    do i = 1, size(nodupftype)
      call namefile_obj%get_unitnumber(trim(nodupftype(i)), iu, 0)
      if (iu > 0) then
        write (errmsg, '(1x, a, a, a)') &
          'Duplicate entries for FTYPE ', trim(nodupftype(i)), &
          ' not allowed for SNF Model.'
        call store_error(errmsg)
      end if
    end do
    !
    ! -- Stop if errors
    if (count_errors() > 0) then
      write (errmsg, '(a, a)') 'Error occurred while reading file: ', &
        trim(namefile_obj%filename)
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    ! -- return
    return
  end subroutine ftype_check

  !> @brief Load input context for supported package
  !<
  subroutine snf_load_input_context(this, filtyp, modelname, pkgname, inunit, &
                                    iout, ipaknum)
    ! -- modules
    use IdmMf6FileLoaderModule, only: input_load
    ! -- dummy
    class(SnfModelType) :: this
    character(len=*), intent(in) :: filtyp
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: pkgname
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    integer(I4B), optional, intent(in) :: ipaknum
    ! -- local
    !
    ! -- only load if there is a file to read
    if (inunit <= 0) return
    !
    ! -- Load model package input to input context
    select case (filtyp)
    case ('MMR6')
      call input_load('MMR6', 'SNF', 'MMR', modelname, pkgname, inunit, iout)
    case default
      call this%NumericalModelType%load_input_context(filtyp, modelname, &
                                                      pkgname, inunit, iout, &
                                                      ipaknum)
    end select
    !
    ! -- return
    return
  end subroutine snf_load_input_context

end module SnfModule