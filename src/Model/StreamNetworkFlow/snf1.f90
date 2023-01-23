
module SnfModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENFTYPE
  use InputOutputModule, only: ParseLine, upcase
  use SimVariablesModule, only: errmsg
  use VersionModule, only: write_listfile_header
  use BaseModelModule, only: BaseModelType
  use NumericalModelModule, only: NumericalModelType
  use BndModule, only: BndType, GetBndFromList
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
    procedure :: model_da => snf_da
    procedure :: model_solve => snf_solve !< routine for solving this model for the current time step
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
    integer(I4B) :: indis, indisl6
    integer(I4B) :: ipakid, i, j, iu, ipaknum
    character(len=LENPACKAGENAME) :: pakname
    type(NameFileType) :: namefile_obj
    type(SnfModelType), pointer :: this
    class(BaseModelType), pointer :: model
    integer(I4B) :: nwords
    character(len=LINELENGTH), allocatable, dimension(:) :: words
    ! -- format
! ------------------------------------------------------------------------------
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
    indis = 0
    indisl6 = 0
    call namefile_obj%get_unitnumber('DISL6', indisl6, 1)
    if (indisl6 > 0) indis = indisl6
    ! call namefile_obj%get_unitnumber('IC6', this%inic, 1)
    ! call namefile_obj%get_unitnumber('OC6', this%inoc, 1)
    call namefile_obj%get_unitnumber('MMR6', this%inmmr, 1)
    ! call namefile_obj%get_unitnumber('OBS6', this%inobs, 1)
    !
    ! -- Check to make sure that required ftype's have been specified
    ! call this%ftype_check(namefile_obj, indis)
    !
    ! -- Create discretization object
    if (indisl6 > 0) then
      call this%load_input_context('DISL6', this%name, 'DISL', indis, this%iout)
      call disl_cr(this%dis, this%name, indis, this%iout)
    end if
    !
    ! -- Create utility objects
    !call budget_cr(this%budget, this%name)
    !
    ! -- Load input context for currently supported packages
    call this%load_input_context('MMR6', this%name, 'MMR', this%inmmr, this%iout)
    !
    ! -- Create packages that are tied directly to model
    call mmr_cr(this%mmr, this%name, this%inmmr, this%dis, this%iout)
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
        !call this%package_create(cunit(i), ipakid, ipaknum, pakname, iu, &
        !                         this%iout)
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
    ! -- Define packages and assign iout for time series managers
    !do ip = 1, this%bndlist%Count()
      !cdl packobj => GetBndFromList(this%bndlist, ip)
      ! cdl call packobj%bnd_df(this%neq, this%dis)
    !end do


    !
    ! -- return
    return
  end subroutine snf_cr

  !> @brief Allocate memory for non-allocatable members
  subroutine allocate_scalars(this, modelname)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(SnfModelType) :: this
    character(len=*), intent(in) :: modelname
! ------------------------------------------------------------------------------
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

  !> @brief Make explicit solve for this time step
  subroutine snf_solve(this)
    ! -- modules
    ! -- dummy
    class(SnfModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Call boundary packages to set up inflows
    ! call this%flw%cf()

    ! -- Internal flow packages deallocate
    ! call this%spf%solve()
    !
    ! call calc_muskingum_mann( &
    !   nseg, &  ! in int32
    !   segment_order, &  ! in int64 size(nseg)
    !   to_segment, &  ! in int64 size(nseg)
    !   seg_lateral_inflow, &  ! in float64 size(nseg)
    !   seg_inflow0_in, &  ! in float64 size(nseg)
    !   outflow_ts_in, &  ! in float64 size(nseg)
    !   tsi, &  ! in int64 size(nseg)
    !   ts, &  ! in float64 size(nseg)
    !   c0, &  ! in float64 size(nseg)
    !   c1, &  ! in float64 size(nseg)
    !   c2, &  ! in float64 size(nseg)
    !   seg_upstream_inflow, &  ! out float64 size(nseg)
    !   seg_inflow0, &  ! out float64 size(nseg)
    !   seg_inflow, &  ! out float64 size(nseg)
    !   seg_outflow, &  ! out float64 size(nseg)
    !   inflow_ts, &  ! out float64 size(nseg)
    !   outflow_ts, &  ! out float64 size(nseg)
    !   seg_current_sum &  ! out float64 size(nseg)
    ! )

    ! -- return
    return
  end subroutine snf_solve

  !> @brief Deallocate
  subroutine snf_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(SnfModelType) :: this
    ! -- local
    integer(I4B) :: ip
    class(BndType), pointer :: packobj
! ------------------------------------------------------------------------------
    !
    ! -- Internal flow packages deallocate
    call this%dis%dis_da()
    call this%mmr%mmr_da()
    !
    ! -- Internal package objects
    deallocate (this%dis)
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
! ------------------------------------------------------------------------------
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