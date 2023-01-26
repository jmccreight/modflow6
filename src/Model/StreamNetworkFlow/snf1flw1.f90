!> @brief This module contains the FLW package methods
!!
!! This module can be used to represent inflow to streams.  It is based
!! on the GWF WEL package.
!!
!<
module SnfFlwModule
  ! -- modules used by WelModule methods
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM1, DONE, LENFTYPE, DNODATA, MAXCHARLEN
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error
  use MemoryHelperModule, only: create_mem_path
  use BndModule, only: BndType
  use ObsModule, only: DefaultObsIdProcessor
  use SmoothingModule, only: sQSaturation, sQSaturationDerivative
  use ObserveModule, only: ObserveType
  use TimeSeriesLinkModule, only: TimeSeriesLinkType, &
                                  GetTimeSeriesLinkFromList
  use BlockParserModule, only: BlockParserType
  use InputOutputModule, only: GetUnit, openfile
  use MatrixModule
  !
  implicit none
  !
  private
  public :: flw_create
  !
  character(len=LENFTYPE) :: ftype = 'FLW' !< package ftype
  character(len=16) :: text = '             FLW' !< package flow text string
  !
  type, extends(BndType) :: SnfFlwType
  contains
    procedure :: bnd_options => flw_options
    procedure :: bnd_cf => flw_cf
    procedure :: bnd_fc => flw_fc
    procedure :: define_listlabel
    ! -- methods for observations
    procedure, public :: bnd_obs_supported => flw_obs_supported
    procedure, public :: bnd_df_obs => flw_df_obs
    procedure, public :: bnd_bd_obs => flw_bd_obs
    ! -- methods for time series
    procedure, public :: bnd_rp_ts => flw_rp_ts
  end type SnfFlwType

contains

  !> @ brief Create a new package object
  !!
  !!  Create a new WEL Package object
  !!
  !<
  subroutine flw_create(packobj, id, ibcnum, inunit, iout, namemodel, pakname)
    ! -- dummy variables
    class(BndType), pointer :: packobj !< pointer to default package type
    integer(I4B), intent(in) :: id !< package id
    integer(I4B), intent(in) :: ibcnum !< boundary condition number
    integer(I4B), intent(in) :: inunit !< unit number of WEL package input file
    integer(I4B), intent(in) :: iout !< unit number of model listing file
    character(len=*), intent(in) :: namemodel !< model name
    character(len=*), intent(in) :: pakname !< package name
    ! -- local variables
    type(SnfFlwType), pointer :: flwobj
    !
    ! -- allocate the object and assign values to object variables
    allocate (flwobj)
    packobj => flwobj
    !
    ! -- create name and memory path
    call packobj%set_names(ibcnum, namemodel, pakname, ftype)
    packobj%text = text
    !
    ! -- allocate scalars
    call flwobj%allocate_scalars()
    !
    ! -- initialize package
    call packobj%pack_initialize()

    packobj%inunit = inunit
    packobj%iout = iout
    packobj%id = id
    packobj%ibcnum = ibcnum
    packobj%ncolbnd = 1
    packobj%iscloc = 1
    !
    ! -- return
    return
  end subroutine flw_create

  !> @ brief Read additional options for package
    !!
    !!  Read additional options for WEL package.
    !!
  !<
  subroutine flw_options(this, option, found)
    ! -- modules
    use InputOutputModule, only: urword
    ! -- dummy variables
    class(SnfFlwType), intent(inout) :: this !< SnfFlwType object
    character(len=*), intent(inout) :: option !< option keyword string
    logical, intent(inout) :: found !< boolean indicating if option found
    ! -- local variables
    ! -- formats
    !
    select case (option)
    case ('MOVER')
      this%imover = 1
      write (this%iout, '(4x,A)') 'MOVER OPTION ENABLED'
      found = .true.
    case default
      !
      ! -- No options found
      found = .false.
    end select
    !
    ! -- return
    return
  end subroutine flw_options

  !> @ brief Formulate the package hcof and rhs terms.
    !!
    !!  Formulate the hcof and rhs terms for the WEL package that will be
    !!  added to the coefficient matrix and right-hand side vector.
    !!
  !<
  subroutine flw_cf(this, reset_mover)
    ! -- dummy variables
    class(SnfFlwType) :: this !< SnfFlwType  object
    logical, intent(in), optional :: reset_mover !< boolean for resetting mover
    ! -- local variables
    integer(I4B) :: i, node
    real(DP) :: q
    logical :: lrm
    !
    ! -- Return if no wells
    if (this%nbound == 0) return
    !
    ! -- pakmvrobj cf
    lrm = .true.
    if (present(reset_mover)) lrm = reset_mover
    if (this%imover == 1 .and. lrm) then
      call this%pakmvrobj%cf()
    end if
    !
    ! -- Calculate hcof and rhs for each well entry
    do i = 1, this%nbound
      node = this%nodelist(i)
      this%hcof(i) = DZERO
      if (this%ibound(node) <= 0) then
        this%rhs(i) = DZERO
        cycle
      end if
      q = this%bound(1, i)
      this%rhs(i) = -q
    end do
    !
    return
  end subroutine flw_cf

  !> @ brief Copy hcof and rhs terms into solution.
    !!
    !!  Add the hcof and rhs terms for the WEL package to the
    !!  coefficient matrix and right-hand side vector.
    !!
  !<
  subroutine flw_fc(this, rhs, ia, idxglo, matrix_sln)
    ! -- dummy variables
    class(SnfFlwType) :: this !< SnfFlwType  object
    real(DP), dimension(:), intent(inout) :: rhs !< right-hand side vector for model
    integer(I4B), dimension(:), intent(in) :: ia !< solution CRS row pointers
    integer(I4B), dimension(:), intent(in) :: idxglo !< mapping vector for model (local) to solution (global)
    class(MatrixBaseType), pointer :: matrix_sln !< solution coefficient matrix
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: ipos
    !
    ! -- pakmvrobj fc
    if (this%imover == 1) then
      call this%pakmvrobj%fc()
    end if
    !
    ! -- Copy package rhs and hcof into solution rhs and amat
    do i = 1, this%nbound
      n = this%nodelist(i)
      rhs(n) = rhs(n) + this%rhs(i)
      ipos = ia(n)
      call matrix_sln%add_value_pos(idxglo(ipos), this%hcof(i))
      !
      ! -- If mover is active and this well is discharging,
      !    store available water (as positive value).
      if (this%imover == 1 .and. this%rhs(i) > DZERO) then
        call this%pakmvrobj%accumulate_qformvr(i, this%rhs(i))
      end if
    end do
    !
    ! -- return
    return
  end subroutine flw_fc

  !> @ brief Define the list label for the package
    !!
    !!  Method defined the list label for the WEL package. The list label is
    !!  the heading that is written to iout when PRINT_INPUT option is used.
    !!
  !<
  subroutine define_listlabel(this)
    ! -- dummy variables
    class(SnfFlwType), intent(inout) :: this !< SnfFlwType  object
    !
    ! -- create the header list label
    this%listlabel = trim(this%filtyp)//' NO.'
    if (this%dis%ndim == 3) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'ROW'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'COL'
    elseif (this%dis%ndim == 2) then
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'LAYER'
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'CELL2D'
    else
      write (this%listlabel, '(a, a7)') trim(this%listlabel), 'NODE'
    end if
    write (this%listlabel, '(a, a16)') trim(this%listlabel), 'STRESS RATE'
    if (this%inamedbound == 1) then
      write (this%listlabel, '(a, a16)') trim(this%listlabel), 'BOUNDARY NAME'
    end if
    !
    ! -- return
    return
  end subroutine define_listlabel

  ! -- Procedures related to observations

  !> @brief Determine if observations are supported.
    !!
    !! Function to determine if observations are supported by the WEL package.
    !! Observations are supported by the WEL package.
    !!
    !! @return  flw_obs_supported       boolean indicating if observations are supported
    !!
  !<
  logical function flw_obs_supported(this)
    ! -- dummy variables
    class(SnfFlwType) :: this !< SnfFlwType  object
    !
    ! -- set boolean
    flw_obs_supported = .true.
    !
    ! -- return
    return
  end function flw_obs_supported

  !> @brief Define the observation types available in the package
    !!
    !! Method to define the observation types available in the WEL package.
    !!
  !<
  subroutine flw_df_obs(this)
    ! -- dummy variables
    class(SnfFlwType) :: this !< SnfFlwType  object
    ! -- local variables
    integer(I4B) :: indx
    !
    ! -- initialize observations
    call this%obs%StoreObsType('flw', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- Store obs type and assign procedure pointer
    !    for to-mvr observation type.
    call this%obs%StoreObsType('to-mvr', .true., indx)
    this%obs%obsData(indx)%ProcessIdPtr => DefaultObsIdProcessor
    !
    ! -- return
    return
  end subroutine flw_df_obs

  !> @brief Save observations for the package
    !!
    !! Method to save simulated values for the WEL package.
    !!
  !<
  subroutine flw_bd_obs(this)
    ! -- dummy variables
    class(SnfFlwType) :: this !< SnfFlwType  object
    ! -- local variables
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: jj
    real(DP) :: v
    type(ObserveType), pointer :: obsrv => null()
    !
    ! -- clear the observations
    call this%obs%obs_bd_clear()
    !
    ! -- Save simulated values for all of package's observations.
    do i = 1, this%obs%npakobs
      obsrv => this%obs%pakobs(i)%obsrv
      if (obsrv%BndFound) then
        do n = 1, obsrv%indxbnds_count
          v = DNODATA
          jj = obsrv%indxbnds(n)
          select case (obsrv%ObsTypeId)
          case ('TO-MVR')
            if (this%imover == 1) then
              v = this%pakmvrobj%get_qtomvr(jj)
              if (v > DZERO) then
                v = -v
              end if
            end if
          case ('FLW')
            v = this%simvals(jj)
          case default
            errmsg = 'Unrecognized observation type: '//trim(obsrv%ObsTypeId)
            call store_error(errmsg)
          end select
          call this%obs%SaveOneSimval(obsrv, v)
        end do
      else
        call this%obs%SaveOneSimval(obsrv, DNODATA)
      end if
    end do
    !
    ! -- return
    return
  end subroutine flw_bd_obs

  ! -- Procedure related to time series

  !> @brief Assign time series links for the package
  !!
  !! Assign the time series links for the FLW package. Only
  !! the Q variable can be defined with time series.
  !!
  !<
  subroutine flw_rp_ts(this)
    ! -- dummy variables
    class(SnfFlwType), intent(inout) :: this !< SnfFlwType  object
    ! -- local variables
    integer(I4B) :: i, nlinks
    type(TimeSeriesLinkType), pointer :: tslink => null()
    !
    ! -- set up the time series links
    nlinks = this%TsManager%boundtslinks%Count()
    do i = 1, nlinks
      tslink => GetTimeSeriesLinkFromList(this%TsManager%boundtslinks, i)
      if (associated(tslink)) then
        if (tslink%JCol == 1) then
          tslink%Text = 'Q'
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine flw_rp_ts

end module SnfFlwModule
